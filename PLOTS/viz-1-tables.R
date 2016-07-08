# Author: Matthew Phelps
# Desc: Visualize parameters



# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")
ifelse(grepl("wrz741", getwd()),
       save.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Results",
       save.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Results")
setwd(wd.path)

library(ggplot2)
library(tidyr)
require(grid)
library(coda)
library(CholeraDataDK)
library(plotrix)

# LOAD data ---------------------------------------------------------------

load(file = "Attributable-cases-tplus1.Rdata")
load(file = "Proportion-attributable-tplus1.Rdata")
# load(file = "Attributable-cases-tplus1-m3.Rdata")
# load(file = "Proportion-attributable-tplus1-m3.Rdata")
b <- T
ifelse(b, load("sim-model-5-data-b.Rdata"), load("sim-model-5-data.Rdata"))


#load(file = "sim-model-5-data-b.Rdata")


# SUM EXTERNAL CASES ------------------------------------------------------

zx <- I_att_mean_plus1
diag(zx) <- 0
rowSums(zx)

colSums(I_proportion_plus1)



# T + 1 : ATTRIBUTABLE CASES TABLE ------------------------------------------------

par(mar=c(5.5,6.5,1.5,2.9)) # Margins around plot ()
color2D.matplot(round(I_att_mean_plus1, digits = 0), 
                show.values = TRUE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 1.4,
                vcol = "black",
                extremes = c("white", "blue"))
xpos <- seq_len(ncol(betas)) -0.3
ypos <- seq_len(ncol(betas)) - 0.4
axis(3, # specifies top border x position
     at = xpos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(x = xpos,
     labels = names(betas),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] -.15, # 0.7 lines above the top. [4] places ref to top border
     adj = c(1,1),
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
title(main = "Mean cases atrributed to each neighborhood")
# SAVE 
setwd(save.path)
dev.copy(png,
         file = "R4 - Attrib-cases-absolute-tplus1.png",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()



# T + 1 : PROPORTION ATTRIBUTABLE TABLE -------------------------------------------
par(mar=c(5.5,6.5,1.5,2.9)) # Margins around plot ()
color2D.matplot(I_proportion_plus1, 
                show.values = TRUE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 1.4,
                vcol = "black",
                extremes = c("white", "blue"))
xpos <- seq_len(ncol(betas)) -0.3
ypos <- seq_len(ncol(betas)) - 0.4
axis(1, # specifies top border x position
     at = xpos,
     labels = F, tick = FALSE, cex.axis = 0.7)
text(x = xpos,
     labels = names(betas),
     srt = 45, # angle to rotate
     pos = 2, # specifies to put txt at top
     par("usr")[1] -0.15, # 0.7 lines above the top. [4] places ref to top border
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
     xpd = T)
title(main = "Proportion caused by each neighborhood")# not sure but allows txt to overflow table


# SAVE 
setwd(save.path)
dev.copy(png,
         file = "R5 - attribt-cases-proption-tplus1.png",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()






# BETA TABLE --------------------------------------------------------------
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
setwd(save.path)
dev.copy(png,
         file = "R1 - posterior table.png",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()




plot(1:10, 1:10, main = "text(...) examples\n~~~~~~~~~~~~~~",
     sub = "R is GNU ©, but not ® ...")
mtext("«Latin-1 accented chars»: éè øØ å<Å æ<Æ", side = 3)
points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
text(6, 2, "the text is CENTERED around (x,y) = (6,2) by default",
     cex = .8)
text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by 'adj = c(0,0)'",
     adj = c(1,0))
