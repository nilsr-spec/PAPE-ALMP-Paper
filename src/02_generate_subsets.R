#############################################
# 02 - Create data subsets
# Date: 07.04.2026
# Author: Nils
# Date last edit: 07.04.2026
# Author last edit: Nils

# Note: Please refer to the README.md for more information
# on the subsets generated in this file.
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
  
##################################################
# 01. Generate baseline target-group subset, incl. matching covariates
##################################################

# count PUMA areas in new york state (123 PUMA areas in all years)
base %>%
  filter(statefip == 36) %>%
  group_by(year) %>%
  summarise(distinct_pumas_ny = n_distinct(cpuma0010))

# make sure no relevant PUMA (in NY) is never assigned to multiple cities
base %>%
  filter(!is.na(city)) %>%
  filter(statefip == 36) %>%
  distinct(cpuma0010, city) %>%
  count(cpuma0010) %>%
  filter(n > 1)

# create treated_city variable from PUMA data (STEP 1 - use IPUMS city, note: some error!)
base_match <- base %>%
  mutate(
    treated_city = case_when(
      # NYC (all boroughs)
      city %in% c(4610, 4611, 4612) ~ "New York City",
      # Yonkers
      city %in% c(7590) ~ "Yonkers",
      # Buffalo (Erie County)
      city %in% c(0890) ~ "Buffalo",
      # Rochester (Monroe County)
      city %in% c(5930) ~ "Rochester",
      # Syracuse (Onondaga County)
      city %in% c(6850) ~ "Syracuse",
      # Albany (Albany metro)
      city %in% c(0050) ~ "Albany",
      # Other PUMAs are not assigned in this round
      TRUE ~ NA_character_
      )
    ) %>% # STEP 2 - map remaining cities using manual map
  mutate(
    treated_city = case_when(
      # Keep already assigned cities the same
      !is.na(treated_city) ~ treated_city,
      # Mount Vernon / New Rochelle 
      (cpuma0010 %in% c(680) & statefip == 36) ~ "Mount Vernon / New Rochelle",
      # White Plains 
      (cpuma0010 %in% c(678) & statefip == 36) ~ "White Plains",
      # Hempstead (Nassau County)
      cpuma0010 %in% c(684, 685, 689) ~ "Hempstead",
      # Brookhaven (Suffolk County)
      cpuma0010 %in% c(695, 696, 697) ~ "Brookhaven",
      # Schenectady
      cpuma0010 %in% c(658) ~ "Schenectady",
      # Utica (Oneida County)
      cpuma0010 %in% c(636) ~ "Utica", # this seems wrong, maybe use 2000 or 2010 puma!
      # Other PUMAs are not assigned
      TRUE ~ NA_character_
    )
  )

####### Checks if mapping worked

# check treated city population (NYC should dominate)
base_match %>%
  group_by(treated_city) %>%
  summarise(pop = sum(perwt)) %>%
  arrange(desc(pop))

# check puma to city mapping explicitly
puma_city_map <- base_match %>%
  count(cpuma0010, treated_city, wt = perwt, sort = TRUE)

rm(puma_city_map)

####### 

# create treatment indicator (being in a treated city)
base_match <- base_match %>%
  mutate(treated = case_when(
    !is.na(treated_city) ~ 1,
    TRUE ~ 0
  ))

######
###### CONTINUE HERE: MUST CALCULATE (+ MERGE IN) PRE-TREAT (i.e. pre-2012) CPUMA LEVEL COVARIATES + OUTCOME TRENDS
######

# save
# saveRDS(base_match, "data/clean/base_match.rds")

##################################################
# 02. Generate subset for within-NY DiD
##################################################

# restrict to NY state
ny_did <- filter(statefip == 36)

# omit 16 and 17 year olds as they are likely still in school
ny_did <- base_match %>% filter(age > 17)

# restrict to people with less than a high school (equivalent) degree
ny_did <- ny_did %>% 
  filter(educd < 62) %>% # less than HS
  filter(educd > 1) # omit N/A

# USE MATCHING ON CPUMA-LEVEL OBSERVABLES 

# RESTRICT TO VALID CONTROL PUMAS

# save
# saveRDS(ny_did, "data/clean/ny_did.rds")

##################################################
# 02. Generate subset for cross-state PUMA
##################################################

# omit 16 and 17 year olds as they are likely still in school
eastcoast_did <- base_match %>% filter(age > 17)

# restrict to people with less than a high school (equivalent) degree
eastcoast_did <- eastcoast_did %>% 
  filter(educd < 62) %>% # less than HS
  filter(educd > 1) # omit N/A

# USE MATCHING ON CPUMA-LEVEL OBSERVABLES 

# RESTRICT TO VALID CONTROL PUMAS

# save
# saveRDS(eastcoast_did, "data/clean/eastcoast_did.rds")
