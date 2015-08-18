# Author: Matthew Phelps
#Desc: Rshape data for GLM model
# output datasets: glm cholera data.csv


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
rm(pc, mac)
library(reshape)
library(ggplot2)
library(plyr)
# library(AER)
#library(glmnet)
library(stargazer) # for nice output for html etc
library(Publish) #Thomas' own package


load("Rdata\\quarter_combined_glm.Rdata")

quarter.glm <- quarter.merged.glm
rm(quarter.merged.glm)
quarter.glm$R <- quarter.glm$cum.sick <- NULL
quarter.glm$logS <- log(quarter.glm$S)
quarter.glm$N <- quarter.glm$I.t + quarter.glm$S + quarter.glm$R


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

quarter.glm$week.group <- factor(cut(quarter.glm$week.id, c(-1,5,10,16), labels = c(1,2,3)))
fit0 <- glm(I.t ~ week.group + offset(logS),
           data=quarter.glm, family=poisson())
publish(fit0)

table(quarter.glm$week.id)

fit1 <- glm(I.t ~ week.group + quarter*(Christianshavn + combinedquarter + Kjoebmager + 
                            Nyboder + Oester + Rosenborg + St.Annae.Oester+ 
                            St.Annae.Vester ) + offset(logS),
           data=quarter.glm, family=poisson())
publish(fit1)


quarter.glm.subset <- quarter.glm[quarter.glm$quarter %in% c("Christianshavn", "St.Annae.Vester", "Kjoebmager"),]
fit2 <- glm(I.t ~ week.group + quarter*(St.Annae.Vester ) + offset(logS),
           data=quarter.glm.subset, family=poisson())
publish(fit2, digits = 4)


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
