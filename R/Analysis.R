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
source("R/FACE_SoilPhDryVsFresh.R")
