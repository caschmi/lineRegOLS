# lineRegOLS

## 1. Introduction
This R package was created as a product of taking the Biostatistics 625 class at the University of Michigan. The goal of this project was an exploratory dive into creating an R package containing one (or more) pre-existing functions and comparing both its correctness and efficieny against the prexisting function. `lineRegOLS` contains a function that performs simple and multiple linear regression and returns estimates of the model parameters.  

## 2. Background 
The `linear_regression_ols` function in `lineRegOLS` allows a user to perform linear regression on a data frame and regression categorical and continous covariates against the response variable. The function allows allows for easy specification of mean cell coding vs. reference cell coding (default), and allows the user to specify the reference category for one categorical variable. (see limitations below)

## 3. Installation 
To install `lineRegOLS`: 
1. install.packages("devtools")
2. library(devtools)
3. devtools::install_github(“caschmi/lineRegOLS”)
4. library(lineRegOLS)
