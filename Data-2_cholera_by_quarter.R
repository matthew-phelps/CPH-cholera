# Author: Matthew Phelps
# Desc: Prepare data - get cholera by quarter and combine quarters
# Output datasets: quarter.csv

## intro
rm(list = ls())
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)

library (MASS) # used for fiting distributions
library (ggplot2)
library (stats)
library (reshape) # for renaming variables
library(plyr)

load("Rdata\\cholera_by_street.Rdata")


# Summarize each quarter -------------------------------------------------------
# summarize each quarter by day index (i.e. by week)
quarter <- ddply( street.data, .(quarter, startday.index, start.date), summarize, 
                  mensick.week = sum(male.sick, na.rm = T),
                  mendead.week = sum(male.dead, na.rm = T),
                  womensick.week = sum(female.sick, na.rm = T),
                  womendead.week = sum(female.dead, na.rm=T))

# combine male and female counts
for (i in 1:nrow(quarter)){
  quarter$sick.total.week[i] <- quarter$mensick.week[i] + quarter$womensick.week[i]
  quarter$dead.total.week[i] <- quarter$mendead.week[i] + quarter$womendead.week[i]
}


# Find week of peak in each quarter:
peak.day <- ddply(quarter, .(quarter), summarize, peakday = startday.index[which.max(sick.total.week)] )

# Find week of first case in each qurter:
start.day<- ddply (quarter, .(quarter), summarize, startday = startday.index[min(which(sick.total.week >0))])




# Normalize incidence by population ---------------------------------------
# Merge census and cholera data 
census <- read.csv ("Census - quarter.csv", sep=",", header=T, stringsAsFactors=F)
quarter <- merge(quarter, census, by.x="quarter", by.y="Quarter", all.x = T)

# Estimate 1853 population ------------------------------------------------
quarter$est.pop.1853 <- round(((3/5) * (quarter$pop1855 - quarter$pop1850) + quarter$pop1850), digits = 0)
quarter$pop1855 <- quarter$pop1850 <- NULL

quarter$normal.incidence <- quarter$sick.total.week / quarter$est.pop.1853 * 1000
quarter$normal.mortality <- quarter$dead.total.week / quarter$est.pop.1853 * 1000

quarter$normal.incidence <- round(quarter$normal.incidence, digits = 2)
quarter$normal.mortality <- round(quarter$normal.mortality, digits = 2)


rm(census, street.data)




# Add Cumulative No. infected at each week --------------------------------
quarter$week.id <- quarter$startday.index/7 # create time-step index
quarter$quarterID <- as.numeric(as.factor(quarter$quarter))
quarter$cum.sick <- 0


## Calculate the number of ppl in each compartment (S,I,R) at each time step:
# calculate cumilative number of infected in each quarter 

quarter.split <- split(quarter, f = quarter$quarter)
x1 <- quarter.split[[1]]

# Write function to pass to lapply
cumSick <- function(x1){
  x1 <- x1[order(x1$start.date), ]
  for (i in 2:nrow(x1)){
    x1$cum.sick[i] <- x1$cum.sick[i - 1] + x1$sick.total.week[i]
  }
  return (x1)
}
x2 <- lapply(quarter.split, cumSick)
quarter <- do.call(rbind.data.frame, x2)
row.names(quarter) <- NULL


# now find S and R based on the N and "cumulative sick" numbers
quarter$S <- quarter$est.pop.1853 - quarter$cum.sick # no. of susceptibles at each timestep
quarter$R <- quarter$est.pop.1853 - (quarter$S + quarter$sick.total.week)

save(quarter, file = "Rdata\\quarter_eng.Rdata") # not saving as CSV so as to discourage ppl corrupting data along the chain
write.csv(quarter,
          file = "quarter_eng.csv",
          row.names = F)
rm(start.day, peak.day)









# Prepare 1 grouping of of combined quarter NOT based on topography ----------------------------------------


# NEED to re-instate quarterID variable, or start using quarter name instead
combined.quarters <- ddply(quarter[which(quarter$quarterID == 2 | # this are quarters to be combined
                                           quarter$quarterID == 4 |
                                           quarter$quarterID == 5 |
                                           quarter$quarterID == 9 |
                                           quarter$quarterID == 12 |
                                           quarter$quarterID == 13),],
                           .(week.id), summarize,
                           sick.total.week = sum(sick.total.week),
                           dead.total.week = sum(dead.total.week),
                           pop1855 = sum(pop1855),
                           cum.sick = sum(cum.sick),
                           S = sum(S),
                           R = sum(R))
combined.quarters$quarter <- "Combined"
combined.quarters$quarterID <- 99
combined.quarters <- combined.quarters[, c(8,1,2,3,4,9,5,6,7)]

combined <- rbind(quarter[which(quarter$quarterID==1 |
                                  quarter$quarterID==3 |
                                  quarter$quarterID==6 |
                                  quarter$quarterID==7 |
                                  quarter$quarterID==8 |
                                  quarter$quarterID==10 |
                                  quarter$quarterID==11), ],
                  combined.quarters)

# renumber Quarter ID so it's sequential from 1:8
x1 <- with(combined, paste(quarterID))
combined <- within(combined, quarterID <- match(x1, unique(x1)))
rm(x1)
save(combined, file = "Rdata\\quarter_combined.Rdata")



# Combine quarters based on Topography ------------------------------------
# Provides greater discrimination between high and lower areas
topo.combined.quarters.1 <- ddply(quarter[which(quarter$quarterID == 2 |
                                                  quarter$quarterID == 9 |
                                                  quarter$quarterID == 12 |
                                                  quarter$quarterID == 13),],
                                  .(week.id), summarize,
                                  sick.total.week = sum(sick.total.week),
                                  dead.total.week = sum(dead.total.week),
                                  pop1855 = sum(pop1855),
                                  cum.sick = sum(cum.sick),
                                  S = sum(S),
                                  R = sum(R))
topo.combined.quarters.2 <- ddply(quarter[which(quarter$quarterID == 5 |
                                                  quarter$quarterID == 4),],
                                                  .(week.id), summarize,
                                                sick.total.week = sum(sick.total.week),
                                                dead.total.week = sum(dead.total.week),
                                                pop1855 = sum(pop1855),
                                                cum.sick = sum(cum.sick),
                                                S = sum(S),
                                                R = sum(R))
topo.combined.quarters.1$quarter <- "combined.lower"
topo.combined.quarters.2$quarter <- "combined.upper"
topo.combined.quarters.1$quarterID <- 99
topo.combined.quarters.2$quarterID <- 88
topo.combined.quarters.1 <- topo.combined.quarters.1[, c(8,1,2,3,4,9,5,6,7)]

topo.combined <- rbind(quarter[which(quarter$quarterID==1 |
                                       quarter$quarterID==3 |
                                       
                                       
                                       quarter$quarterID==6 |
                                       quarter$quarterID==7 |
                                       quarter$quarterID==8 |
                                       quarter$quarterID==10 |
                                       quarter$quarterID==11), ],
                       topo.combined.quarters.1,topo.combined.quarters.2 )
x1 <- with(topo.combined, paste(quarterID))
topo.combined <- within(topo.combined, quarterID <- match(x1, unique(x1)))
rm(x1)
save(topo.combined, file = "Rdata\\quarter_combined_topography.Rdata")
