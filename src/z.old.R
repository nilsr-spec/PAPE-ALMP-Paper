# create binary variables to calculate shares
base <- base %>%
  mutate (
    black = case_when (
      raced %in% c(100:177, 300:827, 850:990) ~ 0,
      raced %in% c(200:234, 830:845) ~ 1,
      TRUE ~ NA_integer_),
    hs_graduate = case_when (
      educd >= 62 ~ 1,
      educd < 62 ~ 0,
      TRUE ~ NA_integer_),
    hispanic = as.integer(hispand > 0),
    female = case_when(
      sex == 1 ~ 0, #men 
      sex == 2 ~ 1, #women
      TRUE ~ NA_integer_),
    not_citizen = case_when (
      citizen == 3 | citizen == 4 ~ 1, #not a citizen
      citizen == 0 | citizen == 1 | citizen == 2 ~ 0, #citizen
      TRUE ~ NA_integer_),
    english = case_when (
      speakeng %in% c(2,3, 4, 5, 6) ~ 1, # 1 if they speak English
      speakeng == 1 ~ 0, # 0 if they don't
      TRUE ~ NA_integer_)
  )



################################################################################
#### create an outcome variable by year by puma 

plot_data %>%
  mutate(group = ifelse(treated > 0.5, "Treated", "Control")) %>%
  group_by(year, group) %>%
  summarise(mean_empl_rate = mean(puma_empl_rate, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_empl_rate, colour = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 2012, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("Treated" = "#E63946", "Control" = "#457B9D")) +
  labs(
    title = "Employment Rate: Treated vs Matched Control Cities",
    x = "Year",
    y = "Employment Rate",
    colour = "Group"
  ) +
  theme_minimal()

# CHANGE TO NY AS JUST TREATED (NOT IN CONTROL)

# NY = treated units, PA + NJ = control units only
ny_treated <- nynjpa %>% filter(statefip == 36 & puma_mean_treated == 1)
panj_control <- nynjpa %>% filter(statefip == 42 | statefip == 34)

# Stack them back together
nynjpa_clean <- bind_rows(ny_treated, panj_control)

match_out <- matchit(
  match_formula,
  data = nynjpa_clean,
  method = "cem",
  k2k = TRUE
)

summary(match_out, un = TRUE)
matched_pumas <- match.data(match_out)

plot_data <- puma_employment %>%
  inner_join(matched_pumas, by = "cpuma0010")

plot_data %>%
  mutate(group = ifelse(puma_mean_treated > 0.5, "Treated", "Control")) %>%
  group_by(year, group) %>%
  summarise(mean_empl_rate = mean(puma_empl_rate, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_empl_rate, colour = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 2012, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("Treated" = "#E63946", "Control" = "#457B9D")) +
  labs(
    title = "Employment Rate: Treated vs Matched Control Cities",
    x = "Year",
    y = "Employment Rate",
    colour = "Group"
  ) +
  theme_minimal()