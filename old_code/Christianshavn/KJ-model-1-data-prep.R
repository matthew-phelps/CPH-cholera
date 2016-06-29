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
load(file = "quarter_combined.Rdata")

# SUBSET to 1 QUARTER ---------------------------------------------------
pop <- combined[, c("quarter", "est.pop.1853")]
quarter_subset <- "Kjoebmager"

I_qrt <- I_multi_replicate[which(I_multi_replicate$quarter == quarter_subset), ]
N_St_annae_v <- pop[pop$quarter == quarter_subset, "est.pop.1853"][1]
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

S_it_daily <- matrix(0, Nsteps, Nrep)
I_incidence <- X_qrt
N_i_daily  <- matrix(0, Nsteps, Nrep)
N_i_daily <- N_St_annae_v

for (rep in 1:Nrep){
  for( t in 1:Nsteps){
    S_it_daily[t, rep] <- N_St_annae_v - cum_qrt[t, rep]
  }
}

plot(I_incidence$rep1, type = "l")
lines(I_incidence$rep2, col = "red")
# Restict time period for St. Annaes Vester outbreak:
splice <- 25:75
Nsteps <- length(splice)
I_incidence <- (I_incidence[splice, ])
S_it_daily <- (S_it_daily[splice, ])

plot(I_incidence$rep1, type = "l")
lines(I_incidence$rep2, col ="red")
rm(list = setdiff(ls(), c("I_incidence", "N_i_daily", "Nsteps",
                          "S_it_daily"))) #http://goo.gl/88L5C2
# DATA SHAPE --------------------------------------------------------------
# Create separate vectors for several replicates
# These will be fit separately in JAGS
I_rep1 <- I_incidence[, 1]
I_rep2 <- I_incidence[, 2]
I_rep3 <- I_incidence[, 3]
I_rep4 <- I_incidence[, 4]
I_rep5 <- I_incidence[, 5]
I_rep6 <- I_incidence[, 6]
I_rep7 <- I_incidence[, 7]
I_rep8 <- I_incidence[, 8]
I_rep9 <- I_incidence[, 9]
I_rep10 <- I_incidence[, 10]
I_reps <- list(I_rep1, I_rep2, I_rep3, I_rep4,
               I_rep5, I_rep6, I_rep7, I_rep8,
               I_rep9, I_rep10)
# Sanity check:
plot(I_rep2, type = "l")
lines(I_rep1, col = "red")

S_rep1 <- S_it_daily[, 1]
S_rep2 <- S_it_daily[, 2]
S_rep3 <- S_it_daily[, 3]
S_rep4 <- S_it_daily[, 4]
S_rep5 <- S_it_daily[, 5]
S_rep6 <- S_it_daily[, 6]
S_rep7 <- S_it_daily[, 7]
S_rep8 <- S_it_daily[, 8]
S_rep9 <- S_it_daily[, 9]
S_rep10 <- S_it_daily[, 10]
S_reps <- list(S_rep1, S_rep2, S_rep3, S_rep4,
               S_rep5, S_rep6, S_rep7, S_rep8,
               S_rep9, S_rep10)
plot(S_rep1, type = "l")
lines(S_rep2, col = "red")
# SAVE --------------------------------------------------------------------

rm(list = setdiff(ls(), c("I_reps", "N_i_daily", "Nsteps",
                          "S_reps"))) #http://goo.gl/88L5C2

save(list = ls(), file = "Kj-model-1-data-prep.Rdata")
