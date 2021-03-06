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
  
  # id
  id <- factor(Ring:Plot)
  
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

#######
# Fig #
#######

## calculate Ratio of e/a for each block ##

# Ring mean
RngMean <- ddply(dryph, .(block, Ring, CO2, Date, time), summarise, Mean = mean(pH, na.rm = TRUE))

# ratio e/a
RatDF <- ddply(RngMean, .(block, Date, time),  summarise,
               Ratio = Mean[CO2 == "e"] / Mean[CO2 == "a"])

# mean for each time
RatMean <- ddply(RatDF, .(Date, time), summarise,
                 Mean = mean(Ratio, na.rm = TRUE),
                 SE = ci(Ratio)[4],
                 N = sum(!is.na(Ratio)))

# plot
theme_set(theme_bw())
p <- ggplot(RatMean, aes(x = Date, y = Mean))
p2 <- p + geom_bar(stat = "identity", fill = "gray") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE)) +
  geom_hline(yintercept = 1) +
  ylab("pH ratio (e/a)") +
  scale_x_date(breaks= date_breaks("3 month"),
               labels = date_format("%b-%y")) +
  theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))

############
# anaylsis #
############
bxplts(value = "pH", data = subsetD(dryph, post))
m1 <- lmer(pH ~ CO2 * time + (1|block) + (1|Ring) + (1|id), data = subsetD(dryph, !pre))
Anova(m1)
Anova(m1, test.statistic = "F")
