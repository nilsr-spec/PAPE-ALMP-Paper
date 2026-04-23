#############################################
# 04 - Regressions
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
library(ggfixest)
library(janitor)
library(kableExtra)

upstate <- readRDS("data/clean/upstate.rds")
nyc <- readRDS("data/clean/nyc.rds")
puma_covs <- readRDS("data/clean/puma_covs.rds")

# ggplot theme (minimalist, greyscale)
theme_clean <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    axis.line         = element_line(colour = "grey40"),
    axis.ticks        = element_line(colour = "grey40"),
    plot.title        = element_text(face = "bold", size = 14),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    strip.text        = element_text(face = "bold")
  ) 

##################################################
# 01. Regressions
##################################################

# define DiD function
run_did <- function(data) {
  feols(
    employed ~ treated:post + age + female + as.factor(race) + min_wage
    | cpuma0010 + year,
    cluster = ~cpuma0010,
    weights = ~perwt,
    data    = data)
}

# run regressions
did_upstate_full     <- run_did(upstate)
did_upstate_restricted <- run_did(upstate %>% filter(minority == 1 | hsdiploma == 0))
# did_upstate_nodiploma <- run_did(upstate %>% filter(hsdiploma == 0))

did_nyc_full         <- run_did(nyc)
did_nyc_restricted     <- run_did(nyc %>% filter(minority == 1| hsdiploma == 0))
# did_nyc_nodiploma    <- run_did(nyc %>% filter(hsdiploma == 0))

# build + export full regression table
reg_table <- etable(
  did_upstate_full, did_upstate_restricted, #did_upstate_nodiploma,
  did_nyc_full, did_nyc_restricted, #did_nyc_nodiploma,
  headers  = list("Non-NYC" = 2, "NYC" = 2),
  coefstat = "se",
  keep     = c("%treated:post", "%age", "%female"),
  order    = c("%treated:post"),
  dict     = c("employed"     = "Employment Probability",
               "treated:post" = "Treatment Effect (DiD)",
               "age"          = "Age",
               "female"       = "Female",
               "min_wage"     = "Minimum Wage (State)",
               "cpuma0010"    = "PUMA",
               "year"         = "Year",
               "G"            = "Cluster" ),
  fitstat  = ~ n + g + r2,
  digits   = 3,
  se.below = TRUE,
  title    = "Effect of ALMP on Youth Employment",
  tex      = TRUE,
  file     = "output/tables/reg_table.tex",
  replace  = TRUE
)
## NOTE: In LaTex manually 1) change model names, 2) change G to Cluster, 3) add note, 4) remove wage

##################################################
# 02. Event-studies
##################################################

# define event study functions
run_event <- function(data) {
  feols(
    employed ~ i(rel_time, treated, ref = -1) + age + female + race + min_wage
    | cpuma0010 + year, 
    cluster = ~cpuma0010, 
    weights = ~perwt, 
    data = data)
} 

# run event studies
event_upstate_full       <- run_event(upstate)
event_upstate_restricted <- run_event(upstate %>% filter(minority == 1 | hsdiploma == 0))

event_nyc_full           <- run_event(nyc)
event_nyc_restricted     <- run_event(nyc %>% filter(minority == 1 | hsdiploma == 0))

# test for joint significance 2005-2010 (full)
pre_coefs <- c("rel_time::-7:treated", "rel_time::-6:treated", "rel_time::-5:treated",
               "rel_time::-4:treated", "rel_time::-3:treated", "rel_time::-2:treated")

wald(event_upstate_full,     pre_coefs)
wald(event_upstate_restricted, pre_coefs)
wald(event_nyc_full,         pre_coefs)
wald(event_nyc_restricted,     pre_coefs)

# test for joint significance 2008-2010 (relevant pre-treatment window)
pre_coefs <- c("rel_time::-4:treated", "rel_time::-3:treated", "rel_time::-2:treated")

wald(event_upstate_full,     pre_coefs)
wald(event_upstate_restricted, pre_coefs)
wald(event_nyc_full,         pre_coefs)
wald(event_nyc_restricted,     pre_coefs)

# plot event-studies
plot_event_upstate <- ggiplot(
  list("Full" = event_upstate_full, "Restricted" = event_upstate_restricted),
  main  = "Event Study: Non-NYC",
  xlab  = "Years relative to treatment",
  ylab  = "Effect on employment rate",
  geom_style = "errorbar",
) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.4) +
  geom_point(position   = position_dodge(width = 0.4), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("Full" = "grey20", "Restricted" = "grey65")) +
  scale_fill_manual(values  = c("Full" = "grey20", "Restricted" = "grey65")) +
  theme_clean

plot_event_nyc <- ggiplot(
  list("Full" = event_nyc_full, "Restricted" = event_nyc_restricted),
  main  = "Event Study: NYC",
  xlab  = "Years relative to treatment",
  ylab  = "Effect on employment rate",
  geom_style = "errorbar"
) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.15, linewidth = 0.4) +
  geom_point(position   = position_dodge(width = 0.4), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("Full" = "grey20", "Restricted" = "grey65")) +
  scale_fill_manual(values  = c("Full" = "grey20", "Restricted" = "grey65")) +
  theme_clean

# save
ggsave("output/figures/event_upstate.pdf", plot_event_upstate, 
       width = 7, height = 4, device = cairo_pdf)
ggsave("output/figures/event_nyc.pdf", plot_event_nyc, 
       width = 7, height = 4, device = cairo_pdf)

##################################################
# 03 Raw outcome trends plots
##################################################

# function to calculate and plot the raw outcome means over time
make_trends_plot <- function(data, title) {
  data %>%
    group_by(year, treated) %>%
    summarise(
      emp_rate = weighted.mean(employed, w = perwt, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(group = ifelse(treated == 1, "Treated", "Control")) %>%
    ggplot(aes(x = year, y = emp_rate, colour = group, linetype = group)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    geom_vline(xintercept = 2011.5, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
    annotate("text", x = 2011.7, y = Inf, label = "Treatment", 
             hjust = 0, vjust = 1.5, size = 3, colour = "grey40") +
    scale_colour_manual(values  = c("Treated" = "grey20", "Control" = "grey65")) +
    scale_linetype_manual(values = c("Treated" = "solid",  "Control" = "dashed")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = title, x = NULL, y = "Employment Rate") +
    theme_clean
}

plot_trends_upstate <- make_trends_plot(upstate, "Employment Trends: Non-NYC")
plot_trends_nyc     <- make_trends_plot(nyc,     "Employment Trends: NYC")

# save
ggsave("output/figures/trend_upstate.pdf", plot_trends_upstate, 
       width = 7, height = 4, device = cairo_pdf)
ggsave("output/figures/trend_nyc.pdf", plot_trends_nyc, 
       width = 7, height = 4, device = cairo_pdf)

##################################################
# 04. Balance table (PUMA level, post-matching)
##################################################

# pull matched puma lists
upstate_pumas <- unique(upstate$cpuma0010)
nyc_pumas     <- unique(nyc$cpuma0010)

# balance table: get matching covariate values and calculate p-value of mean difference
make_balance_table <- function(puma_list, label) {
  df <- puma_covs %>%
    filter(cpuma0010 %in% puma_list)
  
  # define variable order with separator
  matching_vars <- c("lndensity", "puma_employment_trend", "lnpuma_mean_2010_incwage",
                     "puma_share_2010_minority", "puma_share_2010_hsdiploma")
  additional_vars <- c("puma_share_2010_employed",
                       "puma_share_2010_black", "puma_share_2010_hispanic",
                       "puma_share_2010_female", "puma_share_2010_not_citizen")
  
  vars <- c(matching_vars, additional_vars)
  
  map_dfr(vars, function(v) {
    treated_vals <- df %>% filter(treated == 1) %>% pull(!!sym(v))
    control_vals <- df %>% filter(treated == 0) %>% pull(!!sym(v))
    
    pooled_sd <- sqrt((var(treated_vals, na.rm = TRUE) + var(control_vals, na.rm = TRUE)) / 2)
    smd       <- (mean(treated_vals, na.rm = TRUE) - mean(control_vals, na.rm = TRUE)) / pooled_sd
    pval      <- t.test(treated_vals, control_vals)$p.value
    
    tibble(
      variable = v,
      treated  = round(mean(treated_vals, na.rm = TRUE), 3),
      control  = round(mean(control_vals, na.rm = TRUE), 3),
      smd      = round(smd, 3),
      pval     = round(pval, 3),
      sample   = label
    )
  })
}

balance_upstate <- make_balance_table(upstate_pumas, "Non-NYC")
balance_nyc     <- make_balance_table(nyc_pumas,     "NYC")

# pivot wide so NYC and Non-NYC sit side by side
balance_table <- bind_rows(balance_upstate, balance_nyc) %>%
  pivot_wider(
    names_from  = sample,
    values_from = c(treated, control, smd, pval),
    names_glue  = "{sample}_{.value}"
  ) %>%
  select(variable,
         `Non-NYC_treated`, `Non-NYC_control`, `Non-NYC_smd`, `Non-NYC_pval`,
         `NYC_treated`,     `NYC_control`, `NYC_smd`,  `NYC_pval`) %>%
  mutate(variable = recode(variable,
                           "lndensity"                   = "Log Density",
                           "puma_employment_trend"       = "Employment Trend",
                           "puma_share_2010_minority"    = "Share Minority",  # matched variable
                           "puma_share_2010_hsdiploma"   = "Share HS Diploma",
                           "lnpuma_mean_2010_incwage"     = "Log Wage Income (2010)",
                           "puma_share_2010_employed"    = "Employment Rate (2010)",
                           "puma_share_2010_black"       = "Share Black",
                           "puma_share_2010_hispanic"    = "Share Hispanic",
                           "puma_share_2010_female"      = "Share Female",
                           "puma_share_2010_not_citizen" = "Share Non-Citizen"
  ))

# convert to kable for latex export
balance_tex <- balance_table %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("", "Treated", "Control", "SMD", "p-val", 
                  "Treated", "Control", "SMD", "p-val"),
    label     = "tab_balance"
  ) %>%
  add_header_above(c(" " = 1, "Non-NYC" = 4, "NYC" = 4)) %>%
  pack_rows("Matching Variables", 1, 5) %>%          
  pack_rows("Additional Covariates", 6, 10) %>%      
  footnote(general = "Matching performed on log density, log mean wage income, employment trend, minority share and HS diploma share. Black and Hispanic shares reported separately for transparency.",
           general_title = "Note:",
           footnote_as_chunk = TRUE)

writeLines(balance_tex, "output/tables/tab_balance.tex")
## Note: In LaTeX replace / reformat footnote

##################################################
# 05. Covariate stability over time
##################################################

# covariate stability table, take means pre and post
make_stability_table <- function(data, label) {
  data %>%
    group_by(post, treated) %>%
    summarise(
      share_minority  = weighted.mean(minority,  w = perwt, na.rm = TRUE),
      share_diploma = weighted.mean(hsdiploma, w = perwt, na.rm = TRUE),
      share_female    = weighted.mean(sex == 2,  w = perwt, na.rm = TRUE),
      mean_age        = weighted.mean(age,       w = perwt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group  = ifelse(treated == 1, "Treated", "Control"),
      sample = label
    ) %>%
    mutate(post = factor(post, levels = c(0, 1), labels = c("Pre", "Post"))
    ) %>%
    select(sample, post, group, everything(), -treated)
}

stability_upstate <- make_stability_table(upstate, "Non-NYC")
stability_nyc     <- make_stability_table(nyc,     "NYC")

# make combined stability table
stability_table <- bind_rows(stability_upstate, stability_nyc) %>%
  arrange(sample, group, post) %>%
  rename(
    "Sample"          = sample,
    "Period"          = post,
    "Group"           = group,
    "Share Minority"  = share_minority,
    "Share HS-Diploma"= share_diploma,
    "Share Female"    = share_female,
    "Mean Age"        = mean_age
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# export
stability_tex <- stability_table %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption   = NULL,
    label    = NULL,         
  )

writeLines(stability_tex, "output/tables/tab_stability.tex")

##################################################
# z. Additionally: treated population by year
##################################################

nyc %>%
  filter(treated == 1) %>%
  group_by(year) %>%
  summarise(pop = sum(perwt))

upstate %>%
  filter(treated == 1) %>%
  group_by(year) %>%
  summarise(pop = sum(perwt))