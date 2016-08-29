# Author: Matthew Phelps
# Desc: Linear regression with water variables as covariates
# Dependencies: sim-model-5-data-prep.R


# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())
library(RCurl) # https compatability
# garbage collection in case I had previously loaded large JAGS outputs and they
# are still lingering in memory
gc() 


# LOAD --------------------------------------------------------------------


q_names <- c("Combined_upper", "Combined_lower", "Christianshavn", "Kjoebmager",
             "Nyboder", "Oester", "Rosenborg", "St. Annae Oester", "St. Annae Vester")
print(q_names)

water_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/water-matrix.csv")
border_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/border-matrix.csv")
betas_url <- getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/betas-matrix.csv")

water <- read.csv(text = water_url)
border <- read.csv(text = border_url)
betas <- read.csv(text = betas_url)

# Convert to vector to lm()
water_vec <-as.vector(t(water))
border_vec <- as.vector(t(border))

# Make diagnols of beta "NA" since we are not looking at internal transmission
diag(betas) <- NA
betas_vec <- as.vector(t(betas))

# UNIVARIATE REGRESSIONS ------------------------------------------------------------

hist(log(betas_vec))
qqnorm(log(betas_vec))
qqline(log(betas_vec)) # Not ideally normally distributed, but not entirely off
plot(water_vec, log(betas_vec))
plot(border_vec, log(betas_vec))

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