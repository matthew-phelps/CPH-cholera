# STAN Learning script
# Comes from pg 13 in http://www3.nd.edu/~mclark19/learn/IntroBayes.pdf
# 
rm(list=ls())
set.seed(123)

# Create matrix of covariates
N <- 2500
K <- 3

# Creates our fake dataset
covariates <- replicate(K, rnorm(n = N, mean = 10))
colnames(covariates) <- c('X1', "X2", "X3")

# Create model matrix with randomly chosen intercept of 1
X <- cbind(Intercept = 1, covariates)
rm(covariates)

# Create normally distributed variable (y) that is a linear function of the covariates:
# Make up 4 coefficients
coefs <- c(2, 5, .3, .2)
mu <- X %*% coefs # get's the mean
sigma <- 10
y <- rnorm(n = N, mean = mu, sd = sigma)

# Same as
# y = 5 + .2*X1 - 1.5*X2 + .9*X3 + rnorm(N, mean=0, sd=2)

# Run model on fake date to try and recover our coefficients
modlm = lm(y~., data=data.frame(X[,-1]))
summary(modlm)


# Create the data list object for stan inupt
# Include the N, the number of columns in the model matrix, 
# the target variable and the model matrix itself (the fake data)
dat <- list(N=N, K=ncol(X), y=y, X=X)

