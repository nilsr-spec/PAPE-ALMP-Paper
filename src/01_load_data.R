#############################################
# 01 - Loading ACS data + first cleaning
# Date: 20.03.2026
# Author: Nils
# Date last edit: 07.04.2026
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

ddi <- read_ipums_ddi("data/raw/usa_00004.xml")
data <- read_ipums_micro(ddi)

data <- data %>% clean_names()

# for main analysis omit 25 year olds (if downloaded)
base <- data %>% filter(age < 25)

# for now, only use from 2005 ACS onward (1% sample)
base <- base %>% filter(year > 2004)

# check coding of variables
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
## Note: we could improve this by using educd variable

table(base$employed)

# keep only people in labor force 
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
  "age", "birthyr", "birthqtr", "sex", "raced", "hispand", "citizen",
  "educd", "school",
  "empstat", "empstatd", "labforce", "occ2010", "wkswork2", "employed",
  "inctot", "incwage", "hhincome",
  "languaged", "speakeng",
  "rel_time", "post"
)

base <- base %>%
  select(all_of(keep_vars))

rm(keep_vars)

##################################################
# 03. Save as clean base dataset
##################################################

saveRDS(base, "data/clean/base.rds")