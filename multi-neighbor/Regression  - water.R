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
gc()



# LOAD --------------------------------------------------------------------

load("data/Rdata/sim-model-5-data-b.Rdata")

water <- read.csv("data/water-matrix.csv")
border <- read.csv("data/border-matrix.csv")
water <- water[, 2:(length(q_names)+1)] # Remove variable with quarter names
border <- border[, 2:(length(q_names)+1)]
rownames(water) <- q_names
rownames(border) <- q_names

# Convert to vector to lm()
water_vec <-as.vector(t(water))
border_vec <- as.vector(t(border))

# Make diagnols of beta "NA" since we are not looking at internal transmission
diag(betas) <- NA
betas_vec <- as.vector(t(betas))

# REGERSSION 1 ------------------------------------------------------------

hist(log(betas_vec))
qqnorm(log(betas_vec))
qqline(log(betas_vec))
plot(water_vec, log(betas_vec))
plot(border_vec, log(betas_vec))

x <- lm(log(betas_vec) ~ water_vec + border_vec)
x2 <- lm(log(betas_vec) ~ border_vec)
x3 <- lm(log(betas_vec) ~ water_vec)
summary(x)
summary(x2)
summary(x3)
anova(x3, x2, x)


# Little evidence that water and/or sharing a border is significant in
# predicting the force of transmission between neighborhoods. 