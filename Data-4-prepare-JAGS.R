# Author: Matthew Phelps
#Desc: Prep data to send to JAGS. Save data as list 
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

library(plyr)
source("Data-3-combine quarters.R")

# COMBINED quarters -------------------------------------------------------

### Prepare data to send to Stan
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))


S_it <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_it[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$est.pop.1853[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names


dataList <- list(Nquarter=Nquarter,
                 S_it = S_it,
                 N_i = N_i,
                 I_it=I_it,
                 Nsteps=Nsteps)


# Clean
rm(i, t, week_date)

