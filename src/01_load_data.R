#############################################
# 01 - Loading ACS data + first checks
# Date: 20.03.2026
# Author: Nils
# Date last edit: 30.03.2026
# Author last edit: Nils
#############################################


##################################################
# Prerequisites
##################################################

library(ipumsr)
library(tidyverse)
library(fixest)
library(janitor)

##################################################
# 01. Import Data
##################################################

ddi <- read_ipums_ddi("data/raw/usa_00005.xml")
data <- read_ipums_micro(ddi)

data <- data %>% clean_names()

attributes(data$empstat)
attributes(data$stateicp)

##################################################
# 02. First checks for parallel trends
##################################################

# create treated variable for when you are in new york
data <- data %>%
  mutate(treated = case_when(
    stateicp == 13 ~ 1,
    TRUE ~ 0
  ))

# code outcome dummy variable for employment 
data <- data %>% 
  mutate(employed = case_when(
            empstat == 1 ~ 1, 
            empstat == 2 ~ 0, 
            empstat == 0 | empstat == 3 | empstat == 9 ~ NA,
          ))

table(data$employed)

# create time variable relative to 2012 for event-study
data <- data %>%
  mutate(rel_time = year - 2012)

# run event-study for NY vs XXX
ny_con_1 <- feols(employed ~ i(rel_time, treated, ref = -1)
                   | stateicp + year, # state and year fixed effects
                   vcov = "hetero" , # robust SEs, too few clusters (2)
                   weights = ~perwt, # apply ACS weights
                   data = data[data$stateicp == 13 | data$stateicp == 1,]) 

etable(ny_con_1)

# plot event-study NY vs New XXX
iplot(ny_con_1,
      main = "Event Study NY-Connecticut",
      xlab = "Years relative to treatment",
      ylab = "Effect on employment rate")

##################################################
# 03. Restrict dataset
##################################################