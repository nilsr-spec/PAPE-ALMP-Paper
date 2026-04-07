#############################################
# 03 - Run Regressions
# Date: 07.04.2026
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
# 01. Check for parallel trends (event-study)
##################################################

# run event-study for NY vs everyone
ny_all_1 <- feols(employed ~ i(rel_time, treated, ref = -1)
                 | statefip + year, # state and year fixed effects
                 vcov = "hetero" , # robust SEs, too few clusters (2)
                 weights = ~perwt, # apply ACS weights
                 data = base_nohs) 

etable(ny_all_1)

# plot event-study NY vs New Connecticut
iplot(ny_all_1,
      main = "Event Study NY-all (restricted sample)",
      xlab = "Years relative to treatment",
      ylab = "Effect on employment rate")