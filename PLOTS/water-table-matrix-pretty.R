# Author: Matthew Phelps
# Desc: Visualize parameters
# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())

library(tidyverse)
require(grid)
library(coda)
library(CholeraDataDK)
library(plotrix)

# LOAD data ---------------------------------------------------------------

# Load model 2b and 2c to see effect that water and border has
load(file = "Data/Rdata/hydraulic-covariates.Rdata")
figure_path <- "C:/Users/wrz741/Google Drive/Copenhagen/Thesis/Figures/"
wat<- wat[,2:10]
bord <- bord[, 2:10]
row.names(wat) <- colnames(wat)
row.names(bord) <- colnames(bord)
load(file = "Data/Rdata/Attributable-cases-tplus1.Rdata")
load(file = "Data/Rdata/Proportion-attributable-tplus1.Rdata")


# SUM EXTERNAL CASES ------------------------------------------------------
zx <- wat
diag(zx) <- 0
rowSums(zx)
colSums(I_proportion_plus1)



# T + 1 : ATTRIBUTABLE CASES TABLE ------------------------------------------------

par(mar=c(5.5,6.5,1.5,2.9)) # Margins around plot ()
color2D.matplot(round(zx, digits = 0), 
                show.values = 1,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 1.4,
                vcol = "black",
                extremes = c("white", "blue"))
xpos <- seq_len(ncol(wat)) -0.3
ypos <- seq_len(ncol(wat)) -0.4
axis(3, # specifies top border x position
     at = xpos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(x = xpos,
     labels = names(wat),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] -.15, # 0.7 lines above the top. [4] places ref to top border
     adj = c(1,1),
     xpd = T) # not sure but allows txt to overflow table
axis(2, 
     at = ypos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(y = ypos,
     labels = rev(names(wat)),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] + -0.1,  # 0.1 lines left of left border. [1] places ref to left border
     adj = 0,
     xpd = T) # not sure but allows txt to overflow table
title(main = "Water-connectivity matrix")
# SAVE 

dev.copy(png,
         file = paste(figure_path, "wat_mat.png", sep=""),
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()


# Shared border -----------------------------------------------------------

zx <- bord
diag(zx) <- 0
par(mar=c(5.5,6.5,1.5,2.9)) # Margins around plot ()
color2D.matplot(round(zx, digits = 0), 
                show.values = 1,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 1.4,
                vcol = "black",
                extremes = c("white", "blue"))
xpos <- seq_len(ncol(wat)) -0.3
ypos <- seq_len(ncol(wat)) -0.4
axis(3, # specifies top border x position
     at = xpos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(x = xpos,
     labels = names(wat),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] -.15, # 0.7 lines above the top. [4] places ref to top border
     adj = c(1,1),
     xpd = T) # not sure but allows txt to overflow table
axis(2, 
     at = ypos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(y = ypos,
     labels = rev(names(wat)),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] + -0.1,  # 0.1 lines left of left border. [1] places ref to left border
     adj = 0,
     xpd = T) # not sure but allows txt to overflow table
title(main = "Shared-border matrix")
# SAVE 

dev.copy(png,
         file = paste(figure_path, "bord_mat.png", sep=""),
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()






# BETA TABLE --------------------------------------------------------------

betas <- mcmc_out$betas_median


x <- unlist(betas)
hist(log(x))


par(mar=c(3,6.5,6,2.9)) # Margins around plot ()
color2D.matplot(log(betas), 
                show.values = TRUE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 2,
                vcol = "black",
                extremes = c("white", "blue"))
xpos <- seq_len(ncol(betas)) +0.2
ypos <- seq_len(ncol(betas)) - 0.4
axis(3, # specifies top border x position
     at = xpos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(x = xpos,
     labels = names(betas),
     srt = 45, # angle to rotate
     pos = 3, # specifies to put txt at top
     par("usr")[4] +0.7, # 0.7 lines above the top. [4] places ref to top border
     adj = 0,
     xpd = T) # not sure but allows txt to overflow table
axis(2, 
     at = ypos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(y = ypos,
     labels = rev(names(betas)),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] + -0.1,  # 0.1 lines left of left border. [1] places ref to left border
     adj = 0,
     xpd = T) # not sure but allows txt to overflow table

# SAVE 
dev.copy(png,
         file = "Plot-output/R1 - posterior table.png",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()


