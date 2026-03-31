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

# for main analysis omit 25 year olds
data <- data %>% filter(age < 25)

# for now, only use from 2005 ACS onward (1% sample)
data <- data %>% filter(year > 2004)

# check coding of variables
attributes(data$empstat)
attributes(data$stateicp)

##################################################
# 02. Create necessary variables
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

# drop people not in the labor force (as we expect the effect to be with active people)
data <- data %>% filter(empstat != 3) # essentially half the sample

# create time variable relative to 2012 for event-study
data <- data %>%
  mutate(rel_time = year - 2012)

# create simple pre-post variable for DiD
data <- data %>%
  mutate(rel_time = year - 2012)

##################################################
# 03. Check for parallel trends (event-study)
##################################################

# run event-study for NY vs New Jersey
ny_nj_1 <- feols(employed ~ i(rel_time, treated, ref = -1)
                   | stateicp + year, # state and year fixed effects
                   vcov = "hetero" , # robust SEs, too few clusters (2)
                   weights = ~perwt, # apply ACS weights
                   data = data[data$stateicp == 13 | data$stateicp == 12,]) 

etable(ny_nj_1)

# plot event-study NY vs New Connecticut
iplot(ny_nj_1,
      main = "Event Study NY-NJ (full sample)",
      xlab = "Years relative to treatment",
      ylab = "Effect on employment rate")

##################################################
# 04. Restrict data to target population
##################################################

# Note: This is not necessarily how we would do it in the final, just for checking
# if we can detect any effect

# omit 16 and 17 year olds as they are also in school
data_restr <- data %>% filter(age > 17)

# restrict to people with less than a high school (equivalent) degree
data_restr <- data_restr %>% filter(educ < 6)

##################################################
# 05. Check for parallel trends on restricted sample (event-study)
##################################################

# check observations by state
data_restr %>% filter(stateicp == 13) %>% count(stateicp, year)

# run event-study for NY vs all 49 others
ny_all <- feols(employed ~ i(rel_time, treated, ref = -1)
                 | stateicp + year, # state and year fixed effects
                 vcov = "hetero" , # robust SEs, too few clusters (2)
                 weights = ~perwt, # apply ACS weights
                 data = data_restr) 

etable(ny_all)

# plot event-study NY vs all 49 others
iplot(ny_all,
      main = "Event Study NY-NJ (restricted sample)",
      xlab = "Years relative to treatment",
      ylab = "Effect on employment rate")

### try income as an alternative outcome variable with more variation

# run event-study for NY vs all 49 others
ny_all <- feols(incwage ~ i(rel_time, treated, ref = -1)
                | stateicp + year, # state and year fixed effects
                vcov = "hetero" , # robust SEs, too few clusters (2)
                weights = ~perwt, # apply ACS weights
                data = data_restr) 

etable(ny_all)

# plot event-study NY vs all 49 others
iplot(ny_all,
      main = "Event Study NY-NJ (restricted sample)",
      xlab = "Years relative to treatment",
      ylab = "Wage income")