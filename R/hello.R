#'Linear Regression OLS
#'
#'Estimates the coefficients of a linear model, including their standard errors, t-statistic, and corresponding p-value.
#'
#'@param data Data frame of data used.
#'@param y Character denoting the name of the column in data to be used as the response variable.
#'@param x Vector of characters denoting the columns in data to be used as the predictor variables. Can handle categorical variables.
#'@param cat_coding Argument to set coding scheme to use in place of non-binary categorical covariates. Defaults to "reference" cell coding. Enter "means" for mean cell coding.
#'@param reference_cat String argument to set reference category for categorical covariates. Defaults to first category in multi-level categorical covariates.
#'
#'@return Estimates of the linear regression coefficients, their standard errors, t-statistics, and two-sided p-values.
#'
#'@examples
#'linear_regression_ols(data = testData, y = "score", x = c("age", "perf", "gender"), cat_coding = "means", reference_cat = NULL)
#'linear_regression_ols(data = testData, y = "score", x = c("age", "perf", "gender"), reference_cat = "low")
#'
#'@export
#'

linear_regression_ols = function(data, y, x,
                      cat_coding = "reference",
                      reference_cat = NULL){

  response = data[,y]
  covariate_data = data[,x]

  ### Binary categorical variables

  binary_cat_columns = which(sapply(covariate_data, function(x) is.character(x) && length(unique(x)) == 2))

  for (col in binary_cat_columns){
    unique_values = unique(covariate_data[, col])
    original_col_name = colnames(covariate_data)[col]

    covariate_data[, paste0(original_col_name, "_", unique_values[2])] = as.numeric(covariate_data[, col] == unique_values[2])
    covariate_data[, col] = NULL  # Remove the original column
  }

  ### Multi-level categorical variables

  multiple_cat_columns = which(sapply(covariate_data, function(x) is.character(x) && length(unique(x) > 2)))

  for (mcol in multiple_cat_columns) {

    unique_values = unique(covariate_data[, mcol])
    original_col_name = colnames(covariate_data)[mcol]

    if (cat_coding == "reference") {

      if (!is.null(reference_cat)) {

        cat_columns_factor = levels(factor(unique_values, levels = c(reference_cat, unique_values[unique_values != reference_cat])))

        for (mult in 1:(length(cat_columns_factor) - 1)) {

          covariate_data[, paste0(original_col_name, "_", cat_columns_factor[mult + 1])] =
            as.numeric(covariate_data[, mcol] == cat_columns_factor[mult + 1])

        }

      } else {

        for (mult in 1:(length(unique_values) - 1)) {

          covariate_data[, paste0(original_col_name, "_", unique_values[mult + 1])] =
            as.numeric(covariate_data[, mcol] == unique_values[mult + 1])

        }
      }

      covariate_data[, col] = NULL  # Remove the original column
      X = cbind(rep(1, length(response)), as.matrix(covariate_data))

    } else {

      for (mult in 1:length(unique_values)) {

        covariate_data[, paste0(original_col_name, "_", unique_values[mult])] =
          as.numeric(covariate_data[, mcol] == unique_values[mult])

      }
      covariate_data[, col] = NULL  # Remove the original column
      X = covariate_data
    }
  }

  beta_hat = solve(t(X) %*% X) %*% t(X) %*% response

  ### Standard errors ###

  ident = diag(length(response))
  H = X %*% solve(t(X) %*% X) %*% t(X)
  epsilon_hat = (ident - H) %*% response
  n = length(response)
  p = length(covariate_data)
  sigma_sq_hat = as.numeric((t(epsilon_hat) %*% epsilon_hat) / (n - p))

  beta_est_vars = sigma_sq_hat * solve(t(X) %*% X)
  std_errors = sqrt(diag(beta_est_vars))

  ### T-statistic ###

  t_statistic = beta_hat / std_errors

  ### P-values ###
  p_value = 2*(1 - pt(abs(t_statistic), n-p))

  data.frame(beta_hat, std_errors, t_statistic, p_value)
}
