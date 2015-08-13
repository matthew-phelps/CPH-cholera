# Author: Matthew Phelps
#Desc: Rshape data for GLM model
# output datasets: glm cholera data.csv


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
library(reshape)
library(ggplot2)
library(plyr)
# library(AER)
#library(glmnet)
library(stargazer) # for nice output for html etc

load("Rdata\\quarter_combined_glm.Rdata")

quarter.glm <- quarter.merged.glm
rm(quarter.merged.glm)
quarter.glm$R <- quarter.glm$cum.sick <- NULL
quarter.glm$logS <- log(quarter.glm$S)

# Models 1.1a - no interaction - no topographical disction between ------------------------------------------------------------------
# combined quarters 
quarter.list <- split(quarter.glm, quarter.glm$quarter)
results <- lapply(quarter.list,function(d){
  fitquarter <- glm(I.t ~ Christianshavn + combinedquarter + Kjoebmager + 
                      Nyboder + Oester + Rosenborg + St.Annae.Oester + St.Annae.Vester +
                      offset(logS),
                    data=d,family=poisson())
  summary(fitquarter)
})
results




# Model 1.2 WITH interaction NO TOPOGRAPHIC discintion ----------------------------------------------
fit <- glm(I.t ~ quarter*(Christianshavn + combinedquarter + Kjoebmager + 
                            Nyboder + Oester + Rosenborg + St.Annae.Oester+ 
                            St.Annae.Vester ) + offset(logS),
           data=quarter.glm, family=poisson())
summary(fit)
fit.html <- stargazer(fit, type = "html", dep.var.labels=c("Infectious"),
                      out.header = F, out = "fit_combined.htm")


#  ------------------------------------------------------------------------

# Thomas' code using function I don't have {
# output <- publish(fit)
# output <- output[,-3]
# output
# # }


# 
# 
# uniresults <- do.call("rbind",lapply(names(quarter.list),function(n){
#     d <- quarter.list[[n]]
#     form <- as.formula(paste("sick.total.week~ + offset(logS)+ ",n))
#     fitquarter <- glm(form, data=d,family=poisson())
#     publish(fitquarter)
# }))
# uniresults





# Model 2.1  Using quarters split by topography --------------------------
load("Rdata\\quarter_topo_combined_glm.Rdata")
quarter.glm <- topo.combined
rm(topo.combined)
quarter.glm$R <- quarter.glm$cum.sick <- NULL
quarter.glm$logS <- log(quarter.glm$S)

quarter.list <- split(quarter.glm, quarter.glm$quarter)
results <- lapply(quarter.list,function(d){
  fitquarter <- glm(I.t ~ Christianshavn + combined.lower + combined.upper + 
                      Kjoebmager + Nyboder + Oester + Rosenborg + St.Annae.Oester +
                      St.Annae.Vester + offset(logS),
                    data=d, family=poisson())
  summary(fitquarter)
})
names(results)
results



# Model 2.2 Using quarters split by topography --------------------------
load("Rdata\\quarter_topo_combined_glm.Rdata")
quarter.glm <- topo.combined
rm(topo.combined)
quarter.glm$R <- quarter.glm$cum.sick <- NULL
quarter.glm$logS <- log(quarter.glm$S)

fitquarter <- glm(I.t ~ quarter* (Christianshavn + combined.lower + combined.upper + 
                      Kjoebmager + Nyboder + Oester + Rosenborg + St.Annae.Oester +
                      St.Annae.Vester) + offset(logS),
                    data=quarter.glm, family=poisson())
summary(fitquarter)
names(results)
results


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
