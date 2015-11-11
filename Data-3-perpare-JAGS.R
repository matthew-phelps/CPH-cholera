# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Currently uses combined quarters 
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata"
setwd(pc)
rm(list = ls())
library(plyr)


# COMBINED quarters -------------------------------------------------------
rm(list = ls())
load(file = "quarter_combined.Rdata")


### Prepare data to send to Stan
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))
q_names <-as.data.frame(unique(combined$quarter))

S_it <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_it[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names[, 1]


dataList <- list(Nquarter=Nquarter,
                 S_it = S_it,
                 N_i = N_i,
                 I_it=I_it,
                 Nsteps=Nsteps)


# SAVE --------------------------------------------------------------------

save(list = ls(), file = 'Data_3.Rdata')
