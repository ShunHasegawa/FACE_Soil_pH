rm(list = ls(all = TRUE))

library(plyr)
library(lubridate)
library(car)

#######################################
# Dry-soil pH at each sampling events #
#######################################
dryph <- read.csv("Data/FACE_SoilpH_DrySoil.csv",
                  colClasses = c("Ring" = "factor", 
                                 "Block.A" = "NULL",
                                 "Block.B" = "NULL",
                                 "Plot" = "character",
                                 "Date" = "character"))
##################
## process data ##
##################

dryph <- within(dryph, {
  # plot number
  Plot <- factor(ldply(strsplit(Plot, split = "[.]"))[,2])
  
  # block
  block <- recode(Ring, "1:2 = 'A'; 3:4 = 'B'; 5:6 = 'C';")
  
  # date
  Date <- as.Date(dmy(paste("1", Date, sep = "-")))
    # Just assume it was 1st date of the sampling month
  
  # time
  time <- factor(as.numeric(factor(Date)))
    # number according to date
  
  # CO2 switch
  pre <- ifelse(time %in% c(1, 2), TRUE, FALSE)
  post <- ifelse(time == 1, FALSE, TRUE)
  
})

dryph <- dryph[order(dryph$Date), ]
save(dryph, file = "Output//Data/FACE_SoilpH_DrySoil.RData")
load("Output//Data/FACE_SoilpH_DrySoil.RData")

