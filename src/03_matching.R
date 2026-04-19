#############################################
# 03 - Matching
# Date: 07.04.2026
# Author: Nils
# Date last edit: 19.04.2026
# Author last edit: Nils
#############################################


##################################################
# Prerequisites
##################################################

library(ipumsr)
library(tidyverse)
library(fixest)
library(janitor)
library(MatchIt)

base <- readRDS("data/clean/base.rds")
puma_covs <- readRDS("data/clean/puma_covs.rds")

##################################################
# 01. Matching for non-NYC - states: CT, MD, MA, NJ, PY
##################################################

# filter PUMA covariates dataset to allowed states and filter out NYC + untreated NY PUMAs
upstate_matches <- puma_covs %>% 
  filter(statefip %in% c(9, 24, 25, 34, 36, 42)) %>% # CT, MD, MA, NJ, PY + NY
  filter((treated_city != "New York City") | is.na(treated_city)) %>% # filter out NYC 
  filter(!((statefip == 36) & treated == 0)) # filter out untreated PUMAs in NY

# perform matching (nearest-neighbours)
match_out_upstate <- matchit(
  treated ~ lndensity + puma_employment_trend + puma_share_2010_minority + puma_share_2010_hsdiploma,
  data = upstate_matches,
  method = "nearest",
  distance = "mahalanobis",
  ratio = 3,  # 3 controls per treated
)

# show matching result
summary(match_out_upstate, un = TRUE)  # un = TRUE shows before and after comparison

# extracting matched pumas
upstate_matched_pumas <- match.data(match_out_upstate) 

# restrict individual level dataset to relevant PUMAs
upstate <- base %>%
  filter(cpuma0010 %in% upstate_matched_pumas$cpuma0010)  # restrict to matched PUMAs

# save
saveRDS(upstate, "data/clean/upstate.rds")

##################################################
# 03. Matching for NYC - states: CA, IL, PY, NJ
##################################################

# filter PUMA covariates dataset to allowed states and filter out NYC + untreated NY PUMAs
nyc_matches <- puma_covs %>% 
  filter(statefip %in% c(6, 17, 34, 36, 42)) %>%
  filter((treated_city == "New York City") | is.na(treated_city)) %>%
  filter(!((statefip == 36) & treated == 0))

# perform matching (nearest-neighbor)
match_out_nyc <- matchit(
  treated ~ lndensity + puma_employment_trend + puma_share_2010_minority + puma_share_2010_hsdiploma,
  data = nyc_matches,
  method = "nearest",
  distance = "mahalanobis",
  ratio = 1,  # 1 control per treated
)

# show matching result
summary(match_out_nyc, un = TRUE)  # un = TRUE shows before and after comparison

# extracting matched pumas
nyc_matched_pumas <- match.data(match_out_nyc) 

# restrict individual level dataset
nyc <- base %>%
  filter(cpuma0010 %in% nyc_matched_pumas$cpuma0010)  # restrict to matched PUMAs

# save
saveRDS(nyc, "data/clean/nyc.rds")

