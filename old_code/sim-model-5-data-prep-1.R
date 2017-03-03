
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")


# MCMC PREP ---------------------------------------------------------------

# Combine chains into 1
y <- jags_m5_ls %>%
  combine.MCMC() %>%
  combine.MCMC()
rm(jags_m5_ls)
gc()

# Get median values for each parameter
mcmc_median <- apply(y, MARGIN = 2, FUN = median)
mcmc_names <- names(y[1, ]) # name the rows


# Convert to matrix format for easier reading
betas_temp <- mcmc_median[1:81]
mkDf <- function(x){
  # browser()
  dimx <- sqrt(length(x))
  data.frame(matrix(x, nrow = dimx, ncol = dimx))
}
betas <- mkDf(betas_temp)
rm(betas_temp)

# Re-order based on alphabetical. Should already by alphabetical, but just in
# case
matNames <- function(x, names_m) {
  rownames(x) <- names_m
  colnames(x) <- names_m
  x
}
matOrderFun <- function(x) {
  x[order(rownames(x)), order(colnames(x))]
}
betas <- matNames(betas, q_names)
betas <- matOrderFun(betas)


phi <- mcmc_median['phi']



# 95% CI ------------------------------------------------------------------

int_hpd <- data.frame(HPDinterval(y, 0.95))
hi_hdp <- int_hpd$upper
lo_hdp <- int_hpd$lower
ynrow <- nrow(y)


  

# delete TRUE rows
testHi <- function(x, hi_hdp) ifelse(x > hi_hdp, T, F)
testLo <- function(x, lo_hdp) ifelse(x < lo_hdp, T, F)
test95 <- function(x, inx){
  z <- sum(inx)
  z / nrow(x)
}



# Make vector of which MCMC draws result in parameter combinations that fall
# within the 95% HDP for all parameters
x <- data.frame(as.matrix(y, iters = FALSE))
rm(y)
gc()
xrow <- nrow(x)

inx_hi_1 <- x %>%
  slice(1:(xrow/2)) %>%
  mapply(testHi, ., hi_hdp) %>%
  rowSums() %>%
  {ifelse(.>0, FALSE, TRUE)}
gc()
inx_hi_2 <- x %>%
  slice(((xrow/2)+1):xrow) %>%
  mapply(testHi, ., hi_hdp) %>%
  rowSums() %>%
  {ifelse(.>0, FALSE, TRUE)}
gc()
inx_hi <- c(inx_hi_1, inx_hi_2)

# Remove any MCMC that produce results below 95%HDP for any parameter
x_small <- x[inx_hi, ]

inx_lo <- x_small %>%
  mapply(testLo, ., lo_hdp) %>%
  rowSums() %>%
  {ifelse(.>0, FALSE, TRUE)}
mcmc_95hdp <- x_small[inx_lo, ]
rm(x_small, x, xrow)
gc()

# Turn each mcmc row into a matrix
betas_95hdp <- mcmc_95hdp %>%
  dplyr::select(-82, -83) %>%
  apply(1, mkDf)
phi_95hdp <- mcmc_95hdp %>% dplyr::select(82)
gamma_95hdp <- mcmc_95hdp %>% dplyr::select(83)

# SAVE --------------------------------------------------------------------
# If in future we sample from posterior, keep "y" object that I remove below 
rm(mcmc_median, matNames, matOrderFun, mkDf)
save(int_hpd, file = 'Data/Rdata/int_hpd.Rdata')
rm(int_hpd, inx_hi, inx_hi_1, inx_hi_2, inx_lo, lo_hdp, test95, testHi, testLo)
save(list = ls(), file = 'Data/Rdata/sim-model-5-data-1.Rdata' )
write.csv(betas, file = "Data/Rdata/betas-matrix.csv")
