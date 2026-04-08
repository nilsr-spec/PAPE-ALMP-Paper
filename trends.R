library (ipumsr)
library (tidyr)
library(dplyr)
library (ggplot2)
library (fixest)
library(janitor)

ddi <- read_ipums_ddi("usa_00004.xml") #you can change here for the name of your file
data <- read_ipums_micro(ddi)

glimpse(data)

data <- data %>% clean_names()

attributes (data$empstat)
attributes (data$stateicp)
table (data$age)
attributes (data$city)

#Created dummy employed variable based on empstat, 1 = employed, 0 = unemployed
data <- data %>% mutate (employed =
                           case_when (empstat == 1 ~ 1,
                                      empstat == 2 ~ 0,
                                      empstat == 0 | empstat == 3 | empstat == 9 ~ NA,))

# Dataset with employment rates (weighted) for each state and each year
state_year <- data %>% group_by(year, stateicp) %>%
  summarise(emp_rate = weighted.mean(employed, perwt, na.rm = TRUE), .groups = "drop")

#subset with East Coast states (if you check attributes all up to Pennsylvania)
eastc <- data %>% filter (stateicp <= 14)

# New York, New Jersey, Connecticut
nynj <- state_year %>% filter (stateicp == 1 | stateicp == 12 | stateicp == 13)

plot_nj <- nynj %>% ggplot (aes(x = year, y = emp_rate, color = factor(stateicp))) + 
  geom_line() + geom_point()
plot_nj

#New York and Connecticut only
nycn <- state_year %>% filter (stateicp == 1 | stateicp == 13)

plot_cn <- nycn %>% ggplot (aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_point() + geom_line()
plot_cn

#Parallel trends test for Connecticut

pre_2000 <- state_year %>% filter ((year < 2011), stateicp == 13 | stateicp == 1)
pre_2000 <- pre_2000 %>% mutate(treated = as.integer(stateicp == 13))

pre_2003 <- state_year %>% filter (year %in% c(2003:2011), stateicp == 13 | stateicp == 1)
pre_2003 <- pre_2003 %>% mutate(treated = as.integer(stateicp == 13))

#Significant difference in trends (5%)
model1 <- lm(emp_rate ~ year * treated, data = pre_2000)
summary(model1)

#No significant difference in trends:))
model2 <- lm (emp_rate ~ year * treated, data = pre_2003)
summary(model2)

#Does not work - 0 degrees of freedom
model <- lm(emp_rate ~ (as.factor (year)) + treated + (as.factor (year) * treated), data = pre_2000)


#New York, Pennsylvania
nypa <- state_year %>% filter (stateicp == 13 | stateicp == 14)
plot_pa <- nypa %>% ggplot (aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_point() + geom_line()
plot_pa


# California, New York
nyca <- state_year %>% filter (stateicp == 13 | stateicp == 71)
plot_ca <- nyca %>% ggplot (aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_point() + geom_line()
plot_ca

#Illinois, New York
nyil <- state_year %>% filter (stateicp == 13 | stateicp == 21)
plot_il <- nyil %>% ggplot (aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_point() + geom_line()
plot_il

#Massachusetts, New York
nyma <- state_year %>% filter (stateicp == 13 | stateicp == 3)
plot_ma <- nyma %>% ggplot(aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_line()
plot_ma

#Washington, New York
nywa <- state_year %>% filter (stateicp == 13 | stateicp == 73)
plot_wa <- nywa %>% ggplot(aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_line()
plot_wa

#Maine, New York
nymn <- state_year %>% filter (stateicp == 13 | stateicp == 2)
plot_mn <- nymn %>% ggplot(aes(x = year, y = emp_rate, color = factor(stateicp))) +
  geom_line()
plot_mn

attributes(data$occ2010)

# Dataset with only new york and all values for occupation
ny <- data %>% filter (data$stateicp == 13)
table (ny$occ2010)


##################################################################################
##################################################################################

# EVENT STUDY PLOTS

# create treated variable for when you are in new york
data <- data %>%
  mutate(treated = case_when(
    stateicp == 13 ~ 1,
    TRUE ~ 0
  ))

# create time variable relative to 2012 for event-study
data <- data %>%
  mutate(rel_time = year - 2012)

# run event-study for NY vs XXX
ny_con_1 <- feols(employed ~ i(rel_time, treated, ref = -1)
                  | stateicp + year, # state and year fixed effects
                  vcov = "hetero" , # robust SEs, too few clusters (2)
                  weights = ~perwt, # apply ACS weights
                  data = data[data$stateicp == 13 | data$stateicp == 1,])

## FYI Pennsylvania looks more parallel than Connecticut (all coefficients are insignificant)

etable(ny_con_1)

# plot event-study NY vs New XXX
iplot(ny_con_1,
      main = "Event Study NY-Connecticut",
      xlab = "Years relative to treatment",
      ylab = "Effect on employment rate")

