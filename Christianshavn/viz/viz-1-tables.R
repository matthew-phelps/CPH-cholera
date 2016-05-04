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

/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Rplot.pdf
library(ggplot2)
library(tidyr)
require(grid)
library(coda)
library(CholeraDataDK)
library(plotrix)

# LOAD data ---------------------------------------------------------------

load(file = "sim-multi-1-data.Rdata")
col_ext <- 
par(mar=c(3,6.5,6,2.9)) # Margins around plot ()
color2D.matplot(log2(betas), 
                show.values = TRUE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 2,
                vcol = "black",
                extremes = c("blue", "red"))
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

