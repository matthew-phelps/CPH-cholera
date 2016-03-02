# Author: Matthew Phelps
#Desc: Getting data into correct shape for JAGS run. 
#       Multiple relpicates of random allocation over days of
#       week



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
quarter_chosen <- as.character("Christianshavn")

I_qrt <- I_multi_replicate[which(I_multi_replicate$quarter == quarter_chosen), ]

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
N_quarter_chosen <- 15836
S_it_daily <- matrix(0, Nsteps, Nrep)
I_incidence <- X_qrt
N_i_daily  <- matrix(0, Nsteps, Nrep)
N_i_daily <- N_quarter_chosen

for (rep in 1:Nrep){
  for( t in 1:Nsteps){
    S_it_daily[t, rep] <- N_quarter_chosen - cum_qrt[t, rep]
  }
}


# Restict time period for St. Annaes Vester outbreak:
splice <- 15:85
Nsteps <- length(splice)
I_incidence <- (I_incidence[splice, ])
S_it_daily <- (S_it_daily[splice, ])

rm(list = setdiff(ls(), c("I_incidence", "N_i_daily", "Nsteps",
                          "S_it_daily"))) #http://goo.gl/88L5C2
# DATA SHAPE --------------------------------------------------------------
# Create separate vectors for several replicates
# These will be fit separately in JAGS
num_replicates <- 9
I_rep <- data.frame(matrix(NA, nrow = Nsteps , ncol = num_replicates))
S_rep <- data.frame(matrix(NA, nrow = Nsteps, ncol = num_replicates))

for (i in 1:num_replicates){
  I_rep[, i] <- I_incidence[, i]
  S_rep[, i] <- S_it_daily[, i]
}


# SAVE --------------------------------------------------------------------

rm(I_incidence, S_it_daily)
save(list = ls(), file = "model-1-data-prep_CHRIST.Rdata")
