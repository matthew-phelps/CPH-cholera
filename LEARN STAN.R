# STAN Learning script
# Comes from pg 13 in http://www3.nd.edu/~mclark19/learn/IntroBayes.pdf
# 
rm(list=ls())
set.seed(123)

# Create matrix of covariates
N <- 250
K <- 3

# Creates our fake dataset
covariates <- replicate(K, rnorm(n = N))
colnames(covariates) <- c('X1', "X2", "X3")
