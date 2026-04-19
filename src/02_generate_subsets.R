#############################################
# 02 - Create data subsets
# Date: 07.04.2026
# Author: Nils
# Date last edit: 18.04.2026
# Author last edit: Nils

# Note: Please refer to the README.md for more information
# on the subsets generated in this file.

# Note: The base dataset is filtered for 16-24 year olds in the labour force
#############################################


##################################################
# Prerequisites
##################################################

library(ipumsr)
library(tidyverse)
library(janitor)

base <- readRDS("data/clean/base.rds")

##################################################
# 01. Generate matching covariates
##################################################

# creating a new dataset with 2010 puma-level variables
puma_covs <- base %>% 
  filter(year %in% c(2010)) %>% # filtering for pre-treatment
  group_by(cpuma0010, statefip) %>% # collapse to puma-level
  summarise(
    # PUMA-level constants, takes the max value which is the same for everyone
    lndensity = max(log(density)),
    treated = max(treated),
    treated_city = max(treated_city),
    # weighted means for continuous variables
    across(
      c(inctot, incwage),
      ~ weighted.mean(.x, w = perwt, na.rm = TRUE),
      .names = "puma_mean_2010_{.col}"),
    # shares for binary variables
    across(
      c(employed, black, hispanic, asian, minority, female, hsdiploma, not_citizen, english),
      ~ weighted.mean(.x, w = perwt, na.rm = TRUE),
      .names = "puma_share_2010_{.col}"),
    .groups = "drop") # ungroup

# creating pre-treatment outcome trends by puma 
puma_employment_trend <- base %>%
  group_by(cpuma0010, statefip, year) %>% 
  summarise(puma_empl_rate = weighted.mean(employed, w = perwt, na.rm = TRUE),
             .groups = "drop") %>%
  filter(year %in% c(2009:2011)) %>%
  group_by(cpuma0010, statefip) %>%
  summarise(
    puma_employment_trend = coef(lm(puma_empl_rate ~ year))[2], # average outcome trend 2009-2011
    .groups = "drop")

# merge in employment trend to puma level pre-treatment covariates
puma_covs <- puma_covs %>%
  left_join(puma_employment_trend, by = c("cpuma0010", "statefip"))

rm(puma_employment_trend)

# check for successful matching (should return empty tibble!)
puma_covs %>% 
  group_by(cpuma0010) %>%
  filter(n_distinct(puma_mean_2010_inctot) != 1) %>%
  summarise(n_distinct_values = n_distinct(puma_mean_2010_inctot), .groups = "drop")

# check matching dataset for missing covariates
puma_covs %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ sum(is.nan(.))))

# save
saveRDS(puma_covs, "data/clean/puma_covs.rds")