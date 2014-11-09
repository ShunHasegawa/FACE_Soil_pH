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
Anova(m1, test.statistic = "F")

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
?geom_errorbar

?geom_bar

