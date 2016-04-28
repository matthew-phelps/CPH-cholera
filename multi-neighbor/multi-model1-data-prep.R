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
library(plyr)
library(ggplot2)


load("Data_4.Rdata")
load(file = "quarter_combined.Rdata")
rm(day_sum)
# Only using 10 replicates for now, so restrict data for easier handling:
I_multi_replicate <- I_multi_replicate[1:12]


######
###### SELECT ONLY 3 NEIGHBORHOODS FOR TEST PURPOSES
sel <- I_multi_replicate$quarter == "St. Annae Oester" | I_multi_replicate$quarter == "St. Annae Vester" | I_multi_replicate$quarter == "Nyboder" 
I_multi_replicate <- I_multi_replicate[sel,]
rm(sel)
Nquarter <- length(unique(I_multi_replicate$quarter))

###### 


# Separate population and incidence data - will use pop data later for S calculation
pop <- combined[, c("quarter", "est.pop.1853")]
pop <- unique(pop)
rownames(pop) <- NULL
# X_qrt <- select(I_multi_replicate, contains("rep"))

#
# DATA PREP FOR STAN ------------------------------------------------------
# Find cumsum for each rep for each quarter using ddply() function
cum_qrt <- ddply(I_multi_replicate[, 3:12], .(I_multi_replicate$quarter), cumsum)
cum_qrt$day_index <- I_multi_replicate$day_index
colnames(cum_qrt)[1] <- "quarter"

# Reshape I data:
long_I <- tidyr::gather(I_multi_replicate, rep_num, I, 3:12)
wide_I <- tidyr::spread(long_I, quarter, I)
wide_I <- arrange(wide_I, rep_num)

long_sum <- tidyr::gather(cum_qrt, rep_num, cumsumS, 2:11)
wide_sum <- tidyr::spread(long_sum, quarter, cumsumS)
wide_sum <- arrange(wide_sum, rep_num)


# Calculate S
Nsteps <- max(wide_I$day_index)
N_pop <- pop

S_it_daily <- wide_sum
# I_incidence <- X_qrt
# N_i_daily  <- matrix(0, Nsteps, Nrep)
# N_i_daily <- N_St_annae_v

for (qrt in 3:(Nquarter + 2)){
  for( t in 1:nrow(S_it_daily)){
    S_it_daily[t, qrt] <- N_pop[qrt-2, 2] - wide_sum[t, qrt]
  }
}

I_rep <- wide_I[, 3:(Nquarter + 2)]
I_rep <- matrix(I_rep, nrow = nrow(wide_I), ncol = Nquarter)
I_reps <- split(wide_I, f = wide_I$rep_num)
for (i in 1:length(I_reps)) {
  I_reps[[i]] <- I_reps[[i]][, 3:(Nquarter + 2)]
  I_reps[[i]] <- data.matrix(I_reps[[i]])
  }
S_reps <- split(S_it_daily, f = S_it_daily$rep_num)


# CLEAN DATASPACE ---------------------------------------------------------


rm(list = setdiff(ls(), c("I_reps", 
                          "S_reps",
                          "Nsteps",
                          "N_pop",
                          "Nquarter"))) #http://goo.gl/88L5C2

# Plot to check output
plot(I_reps[[1]][, 1], type = "l")
lines(I_reps[[1]][, 2], col = "red")
lines(I_reps[[1]][, 3], col = "green")

# Save --------------------------------------------------------------------



save(list = ls(), file = "multi-model1-data-prep.Rdata")
