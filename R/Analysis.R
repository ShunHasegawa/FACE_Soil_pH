rm(list = ls(all = TRUE))

library(plyr)
library(lubridate)
library(car)
library(gmodels)
library(ggplot2)
library(MASS)
library(scales)
library(lme4)

source("R/Function.R")

##############################
# Temporal change of soil pH #
##############################
source("R/FACE_TemporalPhChange.R")