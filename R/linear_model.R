#'Linear Regression OLS
#'
#'Estimates the coefficients of a linear model, including their standard errors, t-statistic, and corresponding p-value.
#'
#'@param formula formula: describes the model to be fitted symbolically.
#'@param data data frame of data to be used.
#'@param y character denoting the name of the column in data to be used as the response variable.
#'
#'@return Returns estimates of the coefficients from a linear regression model, including their standard errors, t-statistics, and 2-sided p-values. This function also automatically outputs additional information such as a five number summary of the residuals, R-square and adjusted R-squared values, and an F-test,
#'
#'@examples
#'library(lineRegOLS)
#'set.seed(202311)
#'score = rnorm(1000, mean = 10, sd = 3)
#'
#'age = rnorm(1000, mean = 35, sd = 7.5)
#'
#'perf = round(runif(1000, 0, 2))
#'perf[perf == 0] = "low"
#'perf[perf == 1] = "medium"
#'perf[perf == 2] = "high"
#'
#'gender = round(runif(1000, 0, 1))
#'gender[gender == "0"] = "male"
#'gender[gender == "1"] = "female"
#'
#'testNum = round(runif(1000, 0, 2))
#'testNum[testNum == 0] = "one"
#'testNum[testNum == 1] = "two"
#'testNum[testNum == 2] = "three"
#'
#'testScores = data.frame(score, age, perf, gender, testNum)
#'
#'linear_regression_ols(score~age+gender+perf+testNum, y = "score", data = testScores)
#'linear_regression_ols(score~-1+age+gender+perf+testNum, y = "score", data = testScores)
#'
#'@export
#'

linear_regression_ols = function(formula, y, data){

  Y = data[,y]
  X = model.matrix(formula, data)

  ### Estimates

  beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

  ### Standard errors ###

  ident = diag(length(Y))
  H = X %*% solve(t(X) %*% X) %*% t(X)
  epsilon_hat = (ident - H) %*% Y
  n = length(Y)
  p = length(beta_hat)
  sigma_sq_hat = as.numeric((t(epsilon_hat) %*% epsilon_hat) / (n - p))

  beta_est_vars = sigma_sq_hat * solve(t(X) %*% X)
  std_errors = round(sqrt(diag(beta_est_vars)), 6)

  ### T-statistic ###

  t_statistic = beta_hat / std_errors

  ### P-values ###
  p_value = 2*(1 - pt(abs(t_statistic), n-p))
  Coefficients = data.frame(round(beta_hat, 6), round(std_errors,6), round(t_statistic, 4), round(p_value, 4))
  colnames(Coefficients) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  ### Five number sumamry of
  fiveNumSumm = t(data.frame(round(fivenum(epsilon_hat), 4)))
  colnames(fiveNumSumm) <- c("Min", "Q2", "Median", "Q3", "Max")


  ### Residual Standard Errors ###
  resid_SS = sqrt(sum(epsilon_hat^2)/(n-p))
  resid_SS_output = paste("Residual standard error:", round(resid_SS,2), "on", n-p, "degrees of freedom")

  ### Multiple R-squared and Adjusted ###
  SSY = sum((Y-mean(Y))^2)
  SSE = sum(epsilon_hat^2)

  R_2 = 1 - (SSE / SSY)
  R_2_adjusted = 1 - ((1 - R_2) * (n - 1) / (n - p))

  R_2_conclusion = paste("Multiple R-squared: ", round(R_2, 6),
                         ", Adjusted R-squared: ", round(R_2_adjusted, 6))

  ### F-statistic
  F_stat = ((SSY - SSE) / (p - 1)) / (SSE / (n - p))
  F_p_value = 1 - pf(F_stat, p - 1, n - p)

  F_conclusion = paste("F-statistic: ", round(F_stat, 4), "on", p-1, "and", n-p, "DF", "p-value: ", round(F_p_value, 4))

  return(list(Residuals = fiveNumSumm[1,],
              Coefficients = Coefficients,
              Additional = c(resid_SS_output, R_2_conclusion, F_conclusion)))
}
