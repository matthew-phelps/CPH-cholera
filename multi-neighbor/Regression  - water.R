# Author: Matthew Phelps
# Desc: Linear regression with water variables as covariates



# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())

library(RCurl) # https compatability for Githib download


# LOAD --------------------------------------------------------------------
water_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/water-matrix.csv")
border_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/border-matrix.csv")
betas_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/betas-matrix.csv")

water <- read.csv(text = water_url)
border <- read.csv(text = border_url)
betas <- read.csv(betas_url)

# Convert to vector to lm()
water_vec <-as.vector(t(water))
border_vec <- as.vector(t(border))

# Make diagnols of beta "NA" since we are not looking at internal transmission
diag(betas) <- NA
betas_vec <- as.vector(t(betas))


# CHECK DATA --------------------------------------------------------------
hist(log(betas_vec))
qqnorm(log(betas_vec))
qqline(log(betas_vec)) # Not ideally normally distributed, but not entirely off


# UNIVARIATE REGRESSIONS ------------------------------------------------------------
lm_border <- lm(log(betas_vec) ~ border_vec)
lm_pipes <- lm(log(betas_vec) ~ water_vec)
summary(lm_border)
summary(lm_pipes)


# FULL REGRESSION -----------------------------------------------------
lm_full <- lm(log(betas_vec) ~ water_vec + border_vec)
summary(lm_full)

anova(lm_full, lm_pipes, test="Chisq")
anova(lm_full, lm_border, test="Chisq")
# Little evidence that water and/or sharing a border is significant in
# predicting the force of transmission between neighborhoods. 