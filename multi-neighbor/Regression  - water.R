# Author: Matthew Phelps
# Desc: Linear regression with water variables as covariates

# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())
getwd()
library(RCurl) # https compatability for Githib download
source("Spatial-data-1.R")

# DATA --------------------------------------------------------------------
water <- read.csv("data/water-matrix.csv")
border <- read.csv("data/border-matrix.csv")
betas <- read.csv("Data/Rdata/betas-matrix.csv", row.names = 1)
pop_den <- quarter_shp@data %>%
  arrange(quarter)


# Attach Betas to quarter dataframe
pop_den$betas <- diag(as.matrix(betas))


# Re-order matrices to make sure all quarters are in the same order in all
# objects
matOrderFun <- function(x) {
  x[order(rownames(x)), order(colnames(x))]
}
water <- matOrderFun(water)
border <- matOrderFun(border)

# Convert to vector to to work with lm()
water_vec <-as.vector(t(water))
border_vec <- as.vector(t(border))


# Make diagnols of beta "NA" when we look at cross-neighborhood transmission
diag(betas) <- NA
foi_vec <- as.vector(t(betas))


hist(log(foi_vec))
qqnorm(log(foi_vec))
qqline(log(foi_vec)) # Not ideally normally distributed, but not entirely off


# WATER  & BORDER REGRESSIONS ------------------------------------------------------------
# Univariate regression
lm_border <- lm(log(foi_vec) ~ border_vec)
lm_pipes <- lm(log(foi_vec) ~ water_vec)
summary(lm_border)
summary(lm_pipes)

# Full regression
lm_full <- lm(log(foi_vec) ~ water_vec + border_vec)
summary(lm_full)

anova(lm_full, lm_pipes, test="Chisq")
anova(lm_full, lm_border, test="Chisq")

# POP DENSITY -------------------------------------------------------------
hist((pop_den$pop_density))

# Regress beta values on population density
lm_beta <- lm(betas ~ pop_density, data = pop_den)
summary(lm_den)

# Regress Attack Rates on population density
lm_AR <- lm(AR ~ pop_density, data = pop_den)
summary(lm_AR)
