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

quarter.sheet <- read.xlsx2(file = "quarter.xlsx",
                            sheetIndex = 1, 
                            colClasses = c("character", rep("numeric", 8)))
quarter.sheet <- rename(quarter.sheet, replace = c("sick.total.week" = "I.t"))

# Reshape data ALL QUARTERS ------------------------------------------------------------
quarter.glm <- quarter.sheet[ order(quarter.sheet$week.id, quarter.sheet$quarterID),] # order by chronology
quarter.glm$pop1855 <- quarter.glm$dead.total.week <- NULL

quarter.glm$Quarter <- factor(quarter.glm$quarter,levels=c("Christianshavn","Frimands","Kjoebmager","Klaedebo","Noerre","Nyboder","Oester","Rosenborg","Snarens","St Annae Vester","St Annae Oester", "Strand","Vester"),labels=c("Christianshavn","Frimands","Kjoebmager","Klaedebo","Noerre","Nyboder","Oester","Rosenborg","Snarens","St.Annae.Vester","St.Annae.Oester", "Strand","Vester"))
quarter.glm$quarter <- quarter.glm$Quarter
levels(quarter.glm$quarter) <- list("Christianshavn"="Christianshavn",
                                    "Nyboder"="Nyboder",
                                    "Oester"="Oester",
                                    "Rosenborg"="Rosenborg",
                                    "St.Annae.Oester"="St.Annae.Oester",
                                    "St.Annae.Vester"="St.Annae.Vester",
                                    "Kjoebmager"="Kjoebmager",
                                    "Snarens" = "Snarens"
                                    "Strand" = "Strand"
                                    "Frimands" = "Frimands"
                                    "Klaedebo" = "Klaedebo"
                                    "Noerre" = "Noerre"
                                    "Vester" = "Vester"))
# add columns to df to be filled in later
quarter.week <- split(quarter.glm,quarter.glm$week.id)
quarter.week <- lapply(1:length(quarter.week),function(w){
  week.data <- quarter.week[[w]]
  # browser()
  if (w==1)
    sick.total.previous.week <- matrix(0,ncol=13,nrow=13)
  else
    sick.total.previous.week <- matrix(quarter.week[[w-1]][,"I.t"],ncol=13,nrow=13,byrow=TRUE)
  colnames(sick.total.previous.week) <- week.data$quarter
  cbind(week.data,sick.total.previous.week)
})
quarter.glm <- do.call("rbind",quarter.week)
quarter.glm$quarter
# fill in columns in df
save(quarter.glm, file = "quarter_glm.Rdata")



# Reshape data - MERGED QUARTERS\ -----------------------------------------
quarter.merged.glm <- quarter.sheet[ order(quarter.sheet$week.id, quarter.sheet$quarterID),] # order by chronology
quarter.merged.glm$pop1855 <- quarter.merged.glm$dead.total.week <- NULL

quarter.merged.glm$Quarter <- factor(quarter.merged.glm$quarter,levels=c("Christianshavn","Frimands","Kjoebmager","Klaedebo","Noerre","Nyboder","Oester","Rosenborg","Snarens","St Annae Vester","St Annae Oester", "Strand","Vester"),labels=c("Christianshavn","Frimands","Kjoebmager","Klaedebo","Noerre","Nyboder","Oester","Rosenborg","Snarens","St.Annae.Vester","St.Annae.Oester", "Strand","Vester"))
quarter.merged.glm$quarter <- quarter.merged.glm$Quarter
levels(quarter.merged.glm$quarter) <- list("Christianshavn"="Christianshavn",
                                           "Nyboder"="Nyboder",
                                           "Oester"="Oester",
                                           "Rosenborg"="Rosenborg",
                                           "St.Annae.Oester"="St.Annae.Oester",
                                           "St.Annae.Vester"="St.Annae.Vester",
                                           "Kjoebmager"="Kjoebmager",
                                           "combinedquarter"=c("Snarens","Strand","Frimands","Klaedebo","Noerre","Vester"))
# add columns to df to be filled in later
quarter.week <- split(quarter.merged.glm,quarter.merged.glm$week.id)
quarter.week <- lapply(1:length(quarter.week),function(w){
  week.data <- quarter.week[[w]]
  # browser()
  if (w==1)
    sick.total.previous.week <- matrix(0,ncol=13,nrow=13)
  else
    sick.total.previous.week <- matrix(quarter.week[[w-1]][,"I.t"],ncol=13,nrow=13,byrow=TRUE)
  colnames(sick.total.previous.week) <- week.data$quarter
  cbind(week.data,sick.total.previous.week)
})
quarter.merged.glm <- do.call("rbind",quarter.week)
quarter.merged.glm$quarter

save(quarter.merged.glm, file = "quarter_merged_glm")
