rm(list = ls(all = TRUE))

library(plyr)
library(lubridate)
library(car)
library(gmodels)
library(ggplot2)
library(MASS)
library(scales)
library(lme4)
library(reshape)

source("R/Function.R")

##############################
# Temporal change of soil pH #
##############################
source("R/FACE_TemporalPhChange.R")

########################
# Fresh vs Dry soil pH #
########################
dryph <- read.csv("Data/FACE_Soil_pH_DryFresh.csv",
                  colClasses = c("duplicate" = "factor",
                                 "depth" = "factor"))

# mean of duplicate
MeanDryph <- ddply(dryph, .(depth, soil), summarise, ph = mean(ph, na.rm = TRUE))

# dry vs fresh
DryphCst <- cast(MeanDryph, depth ~ soil, value = "ph")
theme_set(theme_bw())
p <- ggplot(DryphCst, aes(x = dry, y = fresh))
p + geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  stat_smooth(method="lm", se = TRUE) +
  labs(x = "Dry soil pH", y =  "Fresh soil pH")

#########
# Stats #
#########
# compare 1st and 2nd polinomial
m1 <- lm(fresh ~ dry + I(dry^2), data = DryphCst)
m2 <- lm(fresh ~ dry, data = DryphCst)
anova(m1, m2)
# no significant difference so just use 1st polynomial
# (linear)
anova(m2)
summary(m2)

# Are Iterecept and slope significantly different than 0 and
# 1, respecitively? Looking at standard error for estimates,
# probably not. But test anyway.

# Actual value (fresh pH) - Predicted valu where a = 0 & b =
# 1
DryphCst$Diff <- DryphCst$fresh - DryphCst$dry
m1 <- lm(Diff ~ dry, data = DryphCst)
summary(m1)
# Intercept and slope are significantly different than 0, so
# that a and b are not 0 and 1, respectively. (also look at
# the grpah below)

# graphs
aval = c(2, 0, 2)
bval = c(.5, .5, 1)
cols <- c("blue", "red", "green")
par(mfrow = c(2,1), mar = c(3, 4, .2, .5))
xv <- seq(0, 10, 1)

# plot 1
plot(xv, xv, type = "l", ann = F)
l_ply(c(1:3), function(x) abline(aval[x], bval[x], col = cols[x]))
legend("topleft", lty = 1, col = c(1, cols), bty = "n", 
       leg = expression(a==0~~b==1,
                        a!=0~~b!=1,
                        a==0~~b!=1,
                        a!=0~~b==1))
mtext(2, text = "yv", line = 3)

# plot 2 (y - predicted value given a and b values)
yv1 <- xv - xv
yv2 <- lapply(c(1:3), function(x) xv - (aval[x] + bval[x] * xv))
plot(xv, yv1, type = "l", ylab = "y - pred.y", ylim = c(-2, 2), ann = F)
l_ply(c(1:3), function(x) lines(xv, yv2[[x]], col = cols[x]))
mtext(1, text = "xv", line = 2)
mtext(2, text = "yv - pred.yv", line = 3)
