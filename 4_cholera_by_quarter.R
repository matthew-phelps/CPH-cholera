# Author: Matthew Phelps
#Desc: Prepare data - get cholera by quarter and combine quarters
# output datasets: quarter.csv

## intro
rm(list = ls())
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
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
quarter <- ddply( street.data, .(quarter, startday.index), summarize, mensick.week = sum(male.sick), mendead.week = sum(male.dead), womensick.week = sum(female.sick), womendead.week = sum(female.dead, na.rm=T))

# combine male and female counts
for (i in 1:nrow(quarter)){
  quarter$sick.total.week[i] <- quarter$mensick.week[i] + quarter$womensick.week[i]
  quarter$dead.total.week[i] <- quarter$mendead.week[i] + quarter$womendead.week[i]
}

save(quarter, file = "Rdata\\incident_cases_per_week.Rd")

# Find week of peak in each quarter:
peak.day <- ddply(quarter, .(quarter), summarize, peakday = startday.index[which.max(sick.total.week)] )

# Find week of first case in each qurter:
start.day<- ddply (quarter, .(quarter), summarize, startday = startday.index[min(which(sick.total.week >0))])




# Normalize incidence by population ---------------------------------------
census <- read.csv ("Census - quarter.csv", sep=",", header=T, stringsAsFactors=F)

#merge census and cholera data 
quarter.by.week <- merge(quarter, census, by.x="quarter", by.y="Quarter")

for (i in 1:nrow(quarter.by.week)){
  quarter.by.week$normal.incidence[i] <- quarter.by.week$sick.total.week[i] / quarter.by.week$pop1855[i] * 1000
  quarter.by.week$normal.mortality[i] <- quarter.by.week$dead.total.week[i] / quarter.by.week$pop1855[i] * 1000
}

write.csv (quarter.by.week, "Incident cases per week by quarter.csv")
save(quarter.by.week, file = "Rdata\\Quarter - normailzed incidence per week.Rdata")





# Add Cumulative No. infected at each week --------------------------------

quarter.temp <- quarter.by.week
quarter.temp$week.id <- quarter.temp$startday.index/7 # create time-step index
quarter <- quarter.temp[, c(1, 13, 7, 8, 9)] # remove un-needed variables
rm(quarter.temp)

quarter$quarterID <- as.numeric(as.factor(quarter$quarter))
quarter$cum.sick <- 0


## Calculate the number of ppl in each compartment (S,I,R) at each time step:
# calculate cumilative number of infected in each quarter 
for (i in 2:208){
  
  # check to see if the current row is the same quarter as previous row
  if(quarter$quarterID[i] != quarter$quarterID[i-1]){
    quarter$cum.sick[i] <- quarter$sick.total.week[i] # if it's a differernt quarter, reset cummulative count to 0
    
    # if it's the same quarter, add the new sick to the total count of sick ppl
  } else {
    quarter$cum.sick[i] <- quarter$cum.sick[i-1] + quarter$sick.total.week[i]  
  }
}

# now find S and R based on the N and "cumulative sick" numbers
quarter$S <- quarter$pop1855 - quarter$cum.sick # no. of susceptibles at each timestep
quarter$R <- quarter$pop1855 - (quarter$S + quarter$sick.total.week)

save(quarter, file = "Rdata\\quarter_eng.Rdata") # not saving as CSV so as to discourage ppl corrupting data along the chain
#write.csv(quarter,
#          file = "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data\\quarter_eng.csv",
#          row.names = F)
rm(census, quarter.by.week, start.day, street.data)









# Prepare "merged quarter" dataset ----------------------------------------

combined.quarters <- ddply(quarter[which(quarter$quarterID == 2 |
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
combined.quarters$quarterID <- 7
combined.quarters <- combined.quarters[, c(8,1,2,3,4,9,5,6,7)]

combined <- rbind(quarter[which(quarter$quarterID==1 |
                                  quarter$quarterID==3 |
                                  quarter$quarterID==6 |
                                  quarter$quarterID==7 |
                                  quarter$quarterID==8 |
                                  quarter$quarterID==10 |
                                  quarter$quarterID==11), ],
                  combined.quarters)

save(combined, file = "Rdata\\quarter_combined.Rdata")

# quarter <- ddply( street.data, .(quarter, startday.index), summarize, mensick.week = sum(male.sick), mendead.week = sum(male.dead), womensick.week = sum(female.sick), womendead.week = sum(female.dead, na.rm=T))
?numcolwise 

