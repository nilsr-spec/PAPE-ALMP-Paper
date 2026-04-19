#############################################
# 01 - Loading ACS data + first cleaning
# Date: 20.03.2026
# Author: Nils
# Date last edit: 18.04.2026
# Author last edit: Nils
#############################################


##################################################
# Prerequisites
##################################################

library(ipumsr)
library(tidyverse)
library(janitor)

##################################################
# 01. Import Data
##################################################

ddi <- read_ipums_ddi("data/raw/usa_00004.xml")
data <- read_ipums_micro(ddi)

data <- data %>% clean_names()

# for main analysis omit under 15 and over 25 year olds (if downloaded)
base <- data %>% filter(age < 25 & age > 15)

# only use from 2005 ACS onward (1% sample)
base <- base %>% filter(year > 2004)

# check coding of key variables
attributes(base$empstat)
attributes(base$statefip)

##################################################
# 02. Create necessary variables (common to all subsets)
##################################################

# code outcome dummy variable for employment
base <- base %>% 
  mutate(employed = case_when(
    empstat == 1 ~ 1, 
    empstat == 2 ~ 0, 
    empstat == 0 | empstat == 3 | empstat == 9 ~ NA,
  ))

table(base$employed)

# keep only the people in labor force 
# (note: this means intensive margin only, assume people targeted are those looking for a job)
base <- base %>% filter(empstat %in% c(1, 2)) # essentially half the sample

# create time variable relative to 2012 for event-study
base <- base %>%
  mutate(rel_time = year - 2012)

# create simple pre-post variable for DiD
base <- base %>%
  mutate(post = case_when(
    year >= 2012 ~ 1,
    TRUE ~ 0
  ))

# keep only relevant variables
keep_vars <- c(
  "year", "statefip", "countyfip", "perwt", "puma", "cpuma0010", "density", "metro",
  "pctmetro", "met2013", "city",
  "age", "birthyr", "birthqtr", "sex", "race", "hispand", "citizen",
  "educd", "school",
  "empstat", "empstatd", "labforce", "occ2010", "wkswork2", "employed",
  "inctot", "incwage", "hhincome",
  "languaged", "speakeng",
  "rel_time", "post"
)

# Keep only the variables specified above
base <- base %>%
  select(all_of(keep_vars))

# Drop variables to keep object
rm(keep_vars)

##################################################
# 01. Prepare geographic variables, treatment indicator
##################################################

# count PUMA areas in New York state (123 PUMA areas in all years)
base %>%
  filter(statefip == 36) %>%
  group_by(year) %>%
  summarise(distinct_pumas_ny = n_distinct(cpuma0010))

# make sure no relevant PUMA (in NY) is ever assigned to multiple cities
base %>%
  filter(!is.na(city)) %>%
  filter(statefip == 36) %>%
  distinct(cpuma0010, city) %>%
  count(cpuma0010) %>%
  filter(n > 1)

# create treated_city variable from PUMA data (STEP 1 - use IPUMS city)
base <- base %>%
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
      (cpuma0010 %in% c(684, 685, 689) & statefip == 36) ~ "Hempstead",
      # Brookhaven (Suffolk County)
      (cpuma0010 %in% c(695, 696, 697) & statefip == 36) ~ "Brookhaven",
      # Schenectady
      (cpuma0010 %in% c(658) & statefip == 36) ~ "Schenectady",
      # Utica (Oneida County)
      (cpuma0010 %in% c(636) & statefip == 36) ~ "Utica", # this is inaccurate, drop Utica
      # Other PUMAs are not assigned
      TRUE ~ NA_character_
    )
  )

# drop Utica from analysis, as CPUMA0010 is too large
base <- base %>%
  filter(!(treated_city == "Utica") | is.na(treated_city)) # keeps non-Utica and n.a.

####### Checks if mapping worked ####### 

# check treated city population (NYC should dominate; don't interpret values!)
base %>%
  group_by(treated_city) %>%
  summarise(pop = sum(perwt)) %>%
  arrange(desc(pop))

# check puma to city mapping explicitly
puma_city_map <- base %>%
  count(statefip, cpuma0010, treated_city, wt = perwt, sort = TRUE)

rm(puma_city_map)

##########################################

# create treatment indicator (being in a treated city)
base <- base %>%
  mutate(treated = case_when(
    !is.na(treated_city) ~ 1,
    TRUE ~ 0
  ))

# create binary/ordinal variables to calculate shares and covariates
base <- base %>%
  mutate(
    black = case_when(
      race == 2 ~ 1, # black 
      race != 2 ~ 0, # not black
      TRUE ~ NA_integer_),
    hispanic = case_when(
      hispand %in% c(100:499) ~ 1, # hispanic
      hispand == 0 ~ 0, # not hispanic
      TRUE ~ NA_integer_),
    asian = case_when(
      race %in% c(4, 5, 6) ~ 1, # asian 
      !(race %in% c(4, 5, 6)) ~ 0, # asian 
      TRUE ~ NA_integer_),
    female = case_when(
      sex == 2 ~ 1, # women
      sex == 1 ~ 0, # men 
      TRUE ~ NA_integer_),
    hsdiploma = case_when(
      educd %in% c(62:116) ~ 1, # minimum high school diploma
      educd %in% c(2:61) ~ 0, # no high school diploma
      TRUE ~ NA_integer_),
    not_citizen = case_when(
      citizen == 3 | citizen == 4 ~ 1, # not a citizen
      citizen == 0 | citizen == 1 | citizen == 2 ~ 0, # citizen
      TRUE ~ NA_integer_),
    english = case_when(
      speakeng %in% c(2,3, 4, 5, 6) ~ 1, # speak English
      speakeng == 1 ~ 0, # 0 don't speak English
      TRUE ~ NA_integer_),
    minority = case_when(
      race != 1 | hispanic == 1 ~ 1,
      race == 1 & hispanic == 0 ~ 0,
      TRUE ~ NA_integer_
    )
  )

##################################################
# 03. Save as clean base dataset
##################################################
saveRDS(base, "data/clean/base.rds")

rm(data, ddi)
