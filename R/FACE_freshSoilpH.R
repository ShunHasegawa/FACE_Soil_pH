################
# Process data #
################
phDF <- read.csv("Data//FACE_FreshSoil.pH.csv")

# make depth numeric by splitting into top, middle and bottom
depthDF <- ldply(strsplit(as.character(phDF$depth), split = c("-|cm")))
depthDF <- data.frame(apply(depthDF, 2, as.numeric))
names(depthDF) <- c("topD", "bottomD")
depthDF$midD <- rowMeans(depthDF)

phDF <- cbind(phDF, depthDF)

# organise data frame
phDF <- within(phDF, {
  time <- factor(ifelse(year == 2012, "1", "2"))
  ring <- as.factor(ring)
  year <- as.factor(year)
})


# ring mean
RngMeanPhDep <- ddply(phDF, .(year, depth, topD, midD, bottomD, ring, co2), summarise,
                      Mean = mean(ph),
                      SE = ci(ph)[4],
                      N = sum(!is.na(ph))) 


#######
# Fig #
#######
# 2012
theme_set(theme_bw())
p <- ggplot(subsetD(RngMeanPhDep, year == 2012), aes(x = -midD, y = Mean, col = ring, group = ring))
p2 <- p + geom_line(alpha = .7) +
  labs(y = "Soil pH", x = "Depth (cm)") +
  coord_flip() + 
  scale_color_manual("Ring", values = c(1:6))
ggsavePP(filename = "Output//Fig/FACE_FreshSoilPH2012", plot = p2, width = 6, height = 6)


# subset the surface layer ph (< 30 cm)
Sph <- subsetD(phDF, bottomD <= 30)

# mean for each sampling location from different depths
DepMean <- ddply(Sph, .(ring, co2, plot, year), summarise, 
                 M = mean(ph),
                 N = sum(!is.na(ph)))

# ring mean
RngMean <- ddply(DepMean, .(ring, co2, year), summarise, 
                 ph = mean(M),
                 SD = sd(M),
                 SE = ci(M)[4],
                 N = sum(!is.na(M)))

# co2 mean
CO2Mean <- ddply(RngMean, .(co2, year), summarise,
                 Mean = mean(ph),
                 SD = sd(ph),
                 SE = ci(ph)[4],
                 N = sum(!is.na(ph)))

########
# Stat #
########
# block and id
DepMean <- within(DepMean, {
  block <- recode(ring, "c(1, 2) = 'A'; c(3, 4) = 'B'; c(5, 6) = 'C'")
  id <- interaction(ring, plot)
  year <- as.factor(year)
})

DepMean$CO2 <- DepMean$co2
DepMean$time <- DepMean$year

bxplts(value = "M", data = DepMean)
m1 <- lmer(M ~ co2 * year + (1|block) + (1|ring), data = DepMean)
Anova(m1)
AnvF_pH <- Anova(m1, test.statistic = "F")
AnvF_pH

# create stat summary table for LMM with CO2 and time
Stat_CO2Time <- StatTable(AnvF_pH)

# model diagnosis
plot(allEffects(m1))
plot(m1)
qqnorm(residuals(m1))
qqline(residuals(m1))
summary(m1)

# contrast
lmeMod <- lme(M ~ co2 * year, random = ~1|block/ring, data = DepMean)
Anova(lmeMod)


cntrst<- contrast(lmeMod, 
                  a = list(year = levels(DepMean$year), co2 = "amb"),
                  b = list(year = levels(DepMean$year), co2 = "elev"))

#######
# Fig #
#######

###########
## Ratio ##
###########
# Calculate ration e/a
## Add block
RngMean$block <- recode(RngMean$ring, "c(1, 2) = 'A'; c(3, 4) = 'B'; c(5, 6) = 'C'")

## compute ratio
RatioDF <- ddply(RngMean, .(year, block), summarise,
                 eaRatio = ph[co2 == "elev"]/ph[co2 == "amb"])

## mean, SE, and N of ratio for each year
RatMeanDF <- ddply(RatioDF, .(year), summarise, 
                   Mean = mean(eaRatio),
                   SE = ci(eaRatio)[4], 
                   N = sum(!is.na(eaRatio)))

# create a plot
p <- ggplot(data = RatMeanDF, aes(x = year, y = Mean))
p + geom_bar(fill = "white", col = "black", stat = "identity") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = .5) +
  coord_cartesian(ylim=c(.9, 1.1))

##########
## Mean ##
##########

# Add P values from the above contrast
pvalsDF <- data.frame(year = c("2012", "2013"), 
                      pval = paste("italic(P)==", c(0.62, "bold(0.09)"), sep = ""))

CO2Mean <- merge(CO2Mean, pvalsDF, by = "year", all = TRUE)

# Add ymin and ymax to draw lines which connect two bars where I place
# significace symbols

CO2Mean <- ddply(CO2Mean, .(year), transform, 
                ymax = max(Mean + SE) + 0.1) 
## common max values between co2 treatment for each year
CO2Mean$ymin <- with(CO2Mean, (Mean + SE) +0.05)


# df for stat table
statDF <- StatPositionDF(StatRes = Stat_CO2Time, ytop = 6, ylength = 1, gap = .04)

# graph setting
science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       legend.position = c(.2, .85), 
                       legend.title = element_blank(),
                       legend.key = element_blank())

# create a plot
p <- ggplot(data = CO2Mean, aes(x = year, y = Mean, fill = co2))
p2 <- p + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = .7), width = 0) +
  geom_point(size = 5, position = position_dodge(width = .7), shape = 21) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_x_discrete(labels = c(expression(atop("June 2012", paste("(Pre-CO"[2], ")"))), 
                              "June 2013")) +
  labs(x = "Time", y = "Soil pH at 0-30 cm") +
  science_theme +
  # here draw lines which connect two bars for each year
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                   position = position_dodge(width = .7),
                   width = 0) +
  geom_segment(aes(x = year, xend = year, y = ymax, yend = ymax),
               position = position_dodge(width = .7)) +
  # here add P values
  geom_text(aes(x = year, y = ymax, label = pval), 
            parse = TRUE, vjust = -1, size = 2.5) +
  # here add stat table
  geom_text(data = subset(statDF, predictor != ""), 
                aes(x = 2.3, y = yval, label = predictor),
                size = 2, , hjust = 1, parse = TRUE) +
  # unless remove [" "] with predictor != "", labels will be messed up due to
  # this empty level
  geom_text(data = statDF, 
            aes(x = 2.4, y = yval, label = p), 
            size = 2, parse = TRUE)

# Use the combination of geom_segment and geom_errorbar to connect to bars where
# you place significant symbols. geom_segment doesn't work well with
# position_dodge(). It makes x position dodged but not y. so when you need to
# use position_dodge() (e.g. plotting barplot), use geom_errorbar to get
# vertical lines
ggsavePP(filename = "Output//Fig/Manuscript/FACE_SoilpH", plot = p2, width = 3.5, height = 3.5)
