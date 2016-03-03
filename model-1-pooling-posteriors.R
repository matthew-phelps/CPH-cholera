#Author: Matthew Phelps 
#Desc: Pooling the posterior distributions from JAGS model runs 
#Dependicies: Data 1, Data 2, 5_GLM_data_reshape, Data-3 Intro
#-------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")
setwd(wd.path)
rm(list = ls())


# LOAD DATA ---------------------------------------------------------------
file_names.df <- file.info(list.files(path = getwd(),
                                      pattern = "model-1-rep.*\\-jags.Rdata$", 
                                      full.names = T))
file_path <- rownames(file_names.df)
for (files in 1:length(file_path)){
  load(file = file_path[files])
}
rm(file_path, files, file_names.df)

ls()[1]
# COMBINE CHAINS IN EACH JAGS ---------------------------------------------
rep1_mcmc <- (combine.mcmc(model_1_jags_rep1))
rep2_mcmc <- (combine.mcmc(model_1_jags_rep2))
rep3_mcmc <- (combine.mcmc(model_1_jags_rep3))
rep4_mcmc <- (combine.mcmc(model_1_jags_rep4))
rep5_mcmc <- (combine.mcmc(model_1_jags_rep5))
rep6_mcmc <- (combine.mcmc(model_1_jags_rep6))
rep7_mcmc <- (combine.mcmc(model_1_jags_rep7))
rep8_mcmc <- (combine.mcmc(model_1_jags_rep8))
rep9_mcmc <- (combine.mcmc(model_1_jags_rep9))

mean(rep6_mcmc[,1])


mcmc_list <- list(rep9_mcmc, rep8_mcmc, rep7_mcmc, rep6_mcmc,
                  rep5_mcmc, rep4_mcmc, rep3_mcmc, rep2_mcmc, rep1_mcmc)
# View means for beta to check that I am on track:
for(i in 1:9){
print(mean(mcmc_list[[6]][1,1]))
}

rm(list = setdiff(ls(), c("mcmc_list"))) #http://goo.gl/88L5C2

# Combine all realizations into one MCMC:
mcmc_comb <- data.frame(combine.mcmc(mcmc_list))
beta_mean <- mean(mcmc_comb[, 1]) # check mean of beta
phi_mean <- mean(mcmc_comb[, 2]) # check mean of phi
rm(mcmc_comb, mcmc_list)

save(list = ls(), file = "model-1-pooling-posteriors.Rdata" )
