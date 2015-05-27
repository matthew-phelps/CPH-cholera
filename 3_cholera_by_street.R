#########################
# Cholera by street
#
########################

## intro
rm(list = ls())
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)

library (reshape) # for renaming variables
#library (gdata) # reading excel files


# Read in data ------------------------------------------------------------


#street.data <- read.xlsx("data/Cholera by street CPH.xlsx", sheetIndex = 1)
street.data <- read.csv ("Cholera by street CPH_eng.csv", sep=",")
head(street.data)

# convert to date format
street.data$start.date <- as.Date(street.data$start.date, "%d-%m-%Y")
street.data$end.date <- as.Date(street.data$end.date, "%d-%m-%Y")

# Remove the word "Quarter" from data for brevity's sake
street.data$quarter <- gsub("Qvarter", "", street.data$quarter)

# Recode 888 to missing data:
street.data$female.dead[street.data$female.dead==888] <- NA

# create data index
day0 <- as.Date("1853-06-12")

street.data$startday.index <-0
street.data$endday.index <-0
for (i in 1:nrow(street.data)){
    street.data$startday.index[i] <- street.data$start.date[i] - day0
    street.data$endday.index[i] <- street.data$end.date[i] - day0

}

save(street.data, file = "Rdata\\cholera_by_street.Rdata") # save as an R object so it doesn't get confused with the csv/xls files






## note to future self - move this to seperate file


# R0 for each quarter -----------------------------------------------------

#list of neighborhood names
names <- c('christianshavn', 'frimands', 'kjobmager', 'nyboder', 'norre',
           'rosenborg', 'snaren', 'annae.vester', 'annae.oester', 'strand', 'vester', 'oester')

# get cumulative cases for each quarter
for (i in 2:nrow(quarter)){
    quarter$cum.sick[1] <- quarter$sick.total.week[1]
    quarter$cum.sick[i] <- quarter$sick.total.week[i] 
    if (quarter$quarter[i] == quarter$quarter[i-1]){
        quarter$cum.sick[i] <- quarter$sick.total.week[i] + quarter$cum.sick[i-1] 
    }
}


cumulative.cases <- ggplot (quarter, aes( x = startday.index, y = log(cum.sick), group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, color = "black") +
  xlab("Day index") +
  ylab("Log cumulative cases") +
  xlim(0,65) + 
  ggtitle ("Log Cumulative cases by week by quarter")

cumulative.cases


which(is.na(quarter$womendead.))


# find the growth rate "r" during the first 50 days of each quarter's epidemic
r.christianshavn <- glm.fit (quarter$startday.index[2:7], quarter$cum.sick[2:7], family = poisson())[[1]]
r.frimands <- glm.fit (quarter$startday.index[20:25], quarter$cum.sick[20:25], family = poisson())[[1]]
r.kjobmager <- glm.fit(quarter$startday.index[34:39], quarter$cum.sick[34:39], family = poisson())[[1]]
r.klaedebo <- glm.fit(quarter$startday.index[52:57], quarter$cum.sick[52:57], family = poisson())[[1]]
r.nyboder <- glm.fit(quarter$startday.index[66:71], quarter$cum.sick[66:71], family = poisson())[[1]]
r.norre <- glm.fit(quarter$startday.index[82:87], quarter$cum.sick[82:87], family = poisson())[[1]]
r.rosenborg <- glm.fit(quarter$startday.index[98:103], quarter$cum.sick[98:103], family = poisson())[[1]]
r.snaren <- glm.fit(quarter$startday.index[115:120], quarter$cum.sick[115:120], family = poisson())[[1]]
r.annae.vester <- glm.fit(quarter$startday.index[129:134], quarter$cum.sick[129:134], family = poisson())[[1]]
r.annae.oester <- glm.fit(quarter$startday.index[145:150], quarter$cum.sick[145:150], family = poisson())[[1]]
r.strand <- glm.fit(quarter$startday.index[165:168], quarter$cum.sick[165:168], family = poisson())[[1]]
r.vester <- glm.fit(quarter$startday.index[180:184], quarter$cum.sick[180:184], family = poisson())[[1]]
r.oester <- glm.fit(quarter$startday.index[195:199], quarter$cum.sick[195:199], family = poisson())[[1]]

r <- as.data.frame(as.matrix(c(r.christianshavn, r.frimands, r.kjobmager, r.nyboder, r.norre, r.rosenborg, r.snaren, r.annae.vester, r.annae.oester, r.strand, r.vester, r.oester)))
r <- cbind( names, r)


# calculate R0 for each neighborhood based on relating R0 to r from Lipsetch 2003
serial.interval <- 3  # from pakistan data
latent.period <-1.4 # from "The incubation period of cholera: A systematic review"
f <- latent.period / serial.interval

for (i in 1:nrow(r)){
   r$R[i] <- r[i,2]^2*(1-f) * f * serial.interval^2 + r[i,2] * serial.interval + 1
}
write.csv(r, "neighborhood R values.csv")
# 
# 
# 
# r.christianshavn <- glm.fit (1:6, quarter$cum.sick[2:7], family = poisson())[[1]]
# r.frimands <- glm.fit (1:6, quarter$cum.sick[20:25], family = poisson())[[1]]
# r.kjobmager <- glm.fit(1:6, quarter$cum.sick[34:39], family = poisson())[[1]]
# r.klaedebo <- glm.fit(1:6, quarter$cum.sick[52:57], family = poisson())[[1]]
# r.nyboder <- glm.fit(1:6, quarter$cum.sick[66:71], family = poisson())[[1]]
# r.norre <- glm.fit(1:6, quarter$cum.sick[82:87], family = poisson())[[1]]
# r.rosenborg <- glm.fit(1:6, quarter$cum.sick[98:103], family = poisson())[[1]]
# r.snaren <- glm.fit(1:6, quarter$cum.sick[115:120], family = poisson())[[1]]
# r.annae.vester <- glm.fit(1:6, quarter$cum.sick[129:134], family = poisson())[[1]]
# r.annae.oester <- glm.fit(1:6, quarter$cum.sick[145:150], family = poisson())[[1]]
# r.strand <- glm.fit(1:4, quarter$cum.sick[165:168], family = poisson())[[1]]
# r.vester <- glm.fit(1:5, quarter$cum.sick[180:184], family = poisson())[[1]]
# r.oester <- glm.fit(1:5, quarter$cum.sick[195:199], family = poisson())[[1]]
# 
# r <- as.data.frame(as.matrix(c(r.christianshavn, r.frimands, r.kjobmager, r.nyboder, r.norre, r.rosenborg, r.snaren, r.annae.vester, r.annae.oester, r.strand, r.vester, r.oester)))
# r <- cbind( names, r)
# 
# 
# # calculate R0 for each neighborhood based on relating R0 to r from Lipsetch 2003
# serial.interval <- 3/7  # from pakistan data
# latent.period <-1.4/7 # from "The incubation period of cholera: A systematic review"
# f <- latent.period / serial.interval
# 
# for (i in 1:nrow(r)){
#     r$R[i] <- r[i,2]^2*(1-f) * f * serial.interval^2 + r[i,2] * serial.interval + 1
# }











multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

multiplot (men.sick, fem.sick, cols=1)

