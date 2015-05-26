# Author: Matthew Phelps
#Desc: Rshape data for GLM model
# output datasets: glm cholera data.csv


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drev\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
library (xlsx) # reading excel files
library(foreign)
library(reshape)
library(ggplot2)
library(plyr)
library(AER)
library(glmnet)

load("quarter_glm")

quarter.glm$R <- quarter.glm$cum.sick <- NULL


# Models ------------------------------------------------------------------

 # overview - GLM for entire dataset: Infections at t+1 are predicted by I in 
 # all quarters at current t
quarter.1<- glm(I_t_1 ~ I_grp1 + I_grp2 + I_grp3 + I_grp4 + I_grp5 + I_grp6 + I_grp7 + I_grp8 + I_grp9 + I_grp10 + I_grp11 + I_grp12 + I_grp13 , family = poisson, data = quarter.glm, offset = log(quarter.glm$S))

summary(quarter.1)


 # 13 models - one for each quarter
 # For each model the outcome (I_t+1), is predicted by I in all other quarters
glm.quarter.list <- list()
x <- list()
for (i in 1:13){
     x <- glm(I_t_1 ~ I_grp1 + I_grp2 + I_grp3 + I_grp4 + I_grp5 + I_grp6 + 
                   I_grp7 + I_grp8 + I_grp9 + I_grp10 + I_grp11 + I_grp12 + 
                   I_grp13 + offset(log(quarter.glm$S)), 
              family = poisson, data = quarter.glm,
              subset = quarterID==i,
              model = F)
     glm.quarter.list[i] <- list(x)
}






# Model outputs -----------------------------------------------------------

# Overview model
summary(quarter.1)

 # creat list of summaries for easy viewing
summary.list <- list()
for (i in 1:13){
 x <- summary(glm.quarter.list[[i]])
 summary.list[i] <- list(x)
}

summary.list




# Diagnostics -------------------------------------------------------------
j <-list()
k <- list()
for (i in 1:13){
      j[i] <- deviance(glm.list[[i]]) / glm.list[[i]]$df.residual
     k[i] <- dispersiontest(glm.list[[i]])
}

for (i in 1:13){
influencePlot(glm.list[[i]])
}



# glmnet models -----------------------------------------------------------
x <- quarter.glm [,8:20]
y <- quarter.glm[,3]
glmnet(x, y, family = "poisson", offset = log(quarter.glm$S))
