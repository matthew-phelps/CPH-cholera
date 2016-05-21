# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")


amazon <- F

ifelse(amazon == T,
       data.path <- "~/Dropbox/AWS-Rstudio",
       data.path <- data.path)
setwd(data.path)

options(mc.cores = (parallel::detectCores() -1 ))
rm(amazon)

# LOAD JAGS OUTPUTS-------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_jags_list_1 <- list()
dataList <- list()
num_reps <- length(I_reps)
rep_num <- 4
dataList<- list(N_i_daily = N_pop[, 2],
                I_incidence=I_reps[[rep_num]],
                Nsteps=Nsteps,
                Nquarter = Nquarter)



# Model 1 -----------------------------------------------------------------
# Need to reun extract() on runjags output
dic_m1_rep4 <- runjags::extract(model = , what = "dic")
dic2 <- runjags::extract(model = 'JAGS-multi-quarter-2.stan', what = "dic")
dic3 <- runjags::extract(model = 'JAGS-multi-quarter-3.stan', what = "dic")
dic4 <- runjags::extract(model = 'JAGS-multi-quarter-4.stan', what = "dic")
