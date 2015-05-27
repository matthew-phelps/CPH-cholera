# Author: Matthew Phelps
#Desc: Rshape data for GLM model
# output datasets: glm cholera data.csv


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
library (xlsx) # reading excel files
library(foreign)
library(reshape)
library(ggplot2)
library(plyr)
# library(AER)
library(glmnet)
library(stargazer) # for nice output for html etc

load("Rdata\\quarter_combined_glm.Rdata")
quarter.glm <- quarter.merged.glm
rm(quarter.merged.glm)
quarter.glm$R <- quarter.glm$cum.sick <- NULL


# Models ------------------------------------------------------------------
quarter.glm$logS <- log(quarter.glm$S)
fit <- glm(I.t ~ quarter*(Christianshavn + combinedquarter + Kjoebmager + Nyboder + Oester + Rosenborg + St.Annae.Oester+ St.Annae.Vester ) + offset(logS),
           data=quarter.glm, family=poisson())
summary(fit)
fit.html <- stargazer(fit, type = "html", dep.var.labels=c("Infectious"),
                      out = "fit_combined.htm")
# Thomas' code using function I don't have {
# output <- publish(fit)
# output <- output[,-3]
# output
# # }
quarter.list <- split(quarter.glm, quarter.glm$quarter)
results <- lapply(quarter.list,function(d){
  fitquarter <- glm(I.t ~ Christianshavn + combinedquarter + Kjoebmager + Nyboder + Oester + Rosenborg + St.Annae.Oester + St.Annae.Vester + offset(logS),
                    data=d,family=poisson())
  summary(fitquarter)
})
names(results)
results

# 
# 
# uniresults <- do.call("rbind",lapply(names(quarter.list),function(n){
#     d <- quarter.list[[n]]
#     form <- as.formula(paste("sick.total.week~ + offset(logS)+ ",n))
#     fitquarter <- glm(form, data=d,family=poisson())
#     publish(fitquarter)
# }))
# uniresults


#  overview - GLM for entire dataset: Infections at t+1 are predicted by I in 
#  all quarters at current t
quarter.fit.1<- glm(I.t ~ Christianshavn + combinedquarter + Kjoebmager + Nyboder + Rosenborg  + Oester + offset(logS), family = poisson, data = quarter.glm)
summary(quarter.fit.1)






#  13 models - one for each quarter
#  For each model the outcome (I_t+1), is predicted by I in all other quarters
glm.quarter.list <- list()
x <- list()
for (i in 1:8){
  x[[i]] <- glm(I.t ~ Christianshavn + combinedquarter + Kjoebmager + Nyboder + Rosenborg  + St.Annae.Vester + St.Annae.Oester + Oester + offset(log(quarter.glm$S)), 
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
