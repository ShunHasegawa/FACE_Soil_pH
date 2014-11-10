rm(list = ls(all = TRUE))

source("R/pckg.R")
source("R/Function.R")

##################################
# Temporal change of Dry soil pH #
##################################
source("R/FACE_TemporalPhChange.R")

#############################
# Analysis on fresh soil pH #
#############################
source("R/FACE_freshSoilpH.R")

########################
# Fresh vs Dry soil pH #
########################
source("R/FACE_SoilPhDryVsFresh.R")
