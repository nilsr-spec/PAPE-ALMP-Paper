#############################################
# 01 - Loading ACS data + first checks
# Date: 20.03.2026
# Author: Nils
# Date last edit: 20.03.2026
# Author last edit: Nils
#############################################


##################################################
# Prerequisites
##################################################

library(ipumsr)

##################################################
# 01. Import Data
##################################################

ddi <- read_ipums_ddi("data/raw/usa_00005.xml")
data <- read_ipums_micro(ddi)

##################################################
# 02. First checks for parallel trends
##################################################

# Basically, what this will have to be is collapse dataset to 16-24 mean of "employed"
# average employment probability, by state and year.
# And then plotting NY time-series of this vs. all other 49 states