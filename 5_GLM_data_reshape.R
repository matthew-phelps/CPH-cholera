.++++# Author: Matthew Phelps
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
quarter.sheet <- rename(quarter.sheet, replace = c("sick.total.week" = "I_t_1"))



# Reshape data ------------------------------------------------------------

quarter.glm <- quarter.sheet[ order(quarter.sheet$week.id, quarter.sheet$quarterID),]
quarter.glm$pop1855 <- quarter.glm$dead.total.week <- NULL


# add columns to df to be filled in later
x<-0
y<-0
for (i in 1:13){
     x[i] <- as.character(i)
     y[i] <-paste("I_grp", x[i], sep="")
     quarter.glm[, y[i]] <- 0
}


# fill in columns in df
for (k in seq(from = 13, to = 195, by = 13)){ # iterate over time-steps -1 (to give I_t+1)
     for (j in 1:13){                        # iterate over number of quarters
          for (i in 1:13){                   # iterate over quarters
               quarter.glm[j+k, 7 + i] <- quarter.glm[i+k-13, 3]
               
          }
     }
}

save(quarter.glm, file = "quarter_glm")
