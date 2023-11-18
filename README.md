# lineRegOLS

## Introduction
This R package was created as a product of taking the Biostatistics 625 class at the University of Michigan. The goal of this project was an exploratory dive into creating an R package containing one (or more) pre-existing functions and comparing both its correctness and efficieny against the prexisting function. `lineRegOLS` contains a function that performs simple and multiple linear regression and returns estimates of the model parameters.  

## Background 
The `linear_regression_ols` function in `lineRegOLS` allows a user to perform linear regression on a data frame and regress categorical and continous covariates against the response variable. The function is comperable to the lm (summary( ... )) functions available in the stats package, and the comparison between the two is available in the vignette. 

## Installation 
To install `lineRegOLS`: 
1. install.packages("devtools")
2. library(devtools)
3. devtools::install_github(“caschmi/lineRegOLS”)
4. library(lineRegOLS)
5. To run in R, use the function `linear_regression_ols`

## Usage 
Below are two captures from R to illustrate the function formulation and corresponding output. The data set used in this example is simulated, with score and age as continous, gender as binary, and perf and testNum as three-level-categorical variables. 

<img width="634" alt="linearModelFormulation" src="https://github.com/caschmi/lineRegOLS/assets/148912328/ae94027d-477a-4439-a723-8d4c65fe6144">

<img width="547" alt="linearModelOutput" src="https://github.com/caschmi/lineRegOLS/assets/148912328/48819ff5-f716-49b3-bd77-bf26b128fd7a">

The idea was to emulated the output of the summary() table available in R. 

## Author
Casey Schmidt (caschmi@umich.edu)
