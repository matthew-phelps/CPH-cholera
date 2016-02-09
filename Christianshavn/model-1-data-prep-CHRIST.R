# Author: Matthew Phelps
#Desc: Used for CHRISTIANSHAVN. ONLY for PLOTTING purposes
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


# SUBSET to 1 QUARTER ---------------------------------------------------

I_qrt <- I_multi_replicate[which(I_multi_replicate$quarter == "Christianshavn"), ]

# Selecton only columns with "rep" in name. See: http://goo.gl/s9xRKr
X_qrt <- I_qrt[, grepl("rep", names(I_qrt))]



# DATA PREP FOR STAN ------------------------------------------------------
# Once method is tested, repeat for all quarters

# Count cumilative infections:
cum_qrt <- data.frame(matrix(NA, dim(X_qrt)[1], dim(X_qrt)[2]))
colnames(cum_qrt) <- colnames(X_qrt)
cum_qrt[1, ] <- X_qrt[1, ]
for (t in 2:nrow(X_qrt)){
  for (rep in 1:ncol(X_qrt)){
    cum_qrt[t, rep] <- cum_qrt[t - 1, rep] + X_qrt[t, rep]
  }
}

Nsteps <- nrow(cum_qrt)
Nrep <- ncol(X_qrt)
N_St_annae_v <- 15836
S_it_daily <- matrix(0, Nsteps, Nrep)
I_incidence <- X_qrt
N_i_daily  <- matrix(0, Nsteps, Nrep)
N_i_daily <- N_St_annae_v

for (rep in 1:Nrep){
  for( t in 1:Nsteps){
    S_it_daily[t, rep] <- N_St_annae_v - cum_qrt[t, rep]
  }
}


# SAVE --------------------------------------------------------------------

save(list = ls(), file = "model-1-data-prep_CHRIST.Rdata")
