# Author: Matthew Phelps
#Desc: Interpolate between weekly observations
# Dependicies: Data 1, Data 2, Data, 3, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------
source("Data-3-combine quarters.R")

bord <- read.csv("Data/border-matrix.csv")
wat <- read.csv("Data/water-matrix.csv")
q_names[q_names=="Combined_lower"] <- "Combined lower"
q_names[q_names=="Combined_upper"] <- "Combined upper"

#re-order matrices
matOrderFun <- function(x) {
  x[order(rownames(x)), order(colnames(x))]
}

wat <- wat %>%
  matOrderFun() %>%
  `colnames<-` (q_names) %>%
  mutate(Quarter = q_names) %>%
  `rownames<-` (NULL) %>%
  dplyr::select(Quarter, everything())
  

bord <- bord %>%
  matOrderFun() %>%
  `colnames<-` (q_names) %>%
  mutate(Quarter = q_names) %>%
  `rownames<-` (NULL) %>%
  dplyr::select(Quarter, everything())


rm(list = setdiff(ls(),c("bord", "wat", "q_names")))

save(list=ls(), file = "Data/Rdata/hydraulic-covariates.Rdata")

