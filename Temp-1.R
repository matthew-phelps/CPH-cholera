# Author: Matthew Phelps
#Desc: Getting data into correct shape for JAGS run. 
#       Multiple relpicates of random allocation over days of
#       week
# Dependicies: Data 4


# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")

setwd(wd.path)
rm(list = ls())
library(dplyr)
library(ggplot2)


load("Data_4.Rdata")


# SUBSET to CHRISTIANIA ---------------------------------------------------

I_chris <- I_multi_replicate[which(I_multi_replicate$quarter == "Christianshavn"), ]

# Selecton only columns with "rep" in name. See: http://goo.gl/s9xRKr
X_ch <- I_chris[, grepl("rep", names(I_chris))]



# DATA PREP FOR STAN ------------------------------------------------------
# Once method is tested, repeat for all quarters

# Count cumilative infections:
cum_chris <- data.frame(matrix(NA, dim(X_ch)[1], dim(X_ch)[2]))
colnames(cum_chris) <- colnames(X_ch)
cum_chris[1, ] <- X_ch[1, ]
for (t in 2:nrow(X_ch)){
  for (rep in 1:ncol(X_ch)){
    cum_chris[t, rep] <- cum_chris[t - 1, rep] + X_ch[t, rep]
  }
}

Nsteps <- nrow(I_chris)
Nrep <- ncol(X_ch)
N_christianshavn <- 15836
S_it <- matrix(0, Nsteps, Nrep)
I_it <- X_ch
N_i  <- matrix(0, Nsteps, Nrep)
N_i <- N_christianshavn

for (rep in 1:Nrep){
  for( t in 1:Nsteps){
    S_it[t, rep] <- N_christianshavn - cum_chris[t, rep]
  }
}
row.names(I_it) <- q_names[, 1]

