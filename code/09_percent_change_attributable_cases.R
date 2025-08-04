# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Calculate percent changes and number of attributable cases.
#
# Date created: 7/25/2025

library(tidyverse)
library(readr)

# Load datasets and results
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_yearly_agg_results_df <- readRDS("results/main_text_results/fig2_dengue_yearly_ld_results.rds")
dengue_yearly_results_df <- readRDS("results/main_text_results/fig2_dengue_yearly_results.rds")

##############################
# Calculate percent change
##############################

## Calculate baseline: Recent Regional Unadjusted Incidence (2007–2008 average)
regional_incidence_all <- dengue_yearly$connected_buffered %>%
  group_by(year) %>%
  summarise(
    yearly_cases_all = sum(yearly_cases_C),
    pop_sum_all = sum(population),
    incidence_all = yearly_cases_all / pop_sum_all * 1000)

regional_incidence_yearly <- dengue_yearly$connected_buffered %>%
  group_by(year, fivekm) %>%
  summarise(
    yearly_cases = sum(yearly_cases_C),
    pop_sum = sum(population),
    incidence_avg = yearly_cases / pop_sum * 1000,
    .groups = "drop") %>%
  left_join(regional_incidence_all, by = "year")

# Reduce to just treated group in 2007 and 2008 (most conservative)
treated_pre_avg_incidence <- regional_incidence_yearly %>%
  filter(fivekm == 1, year %in% as.Date(c("2007-01-01", "2008-01-01"))) %>%
  summarise(mean_incidence_treated_pre = mean(incidence_avg)) %>%
  pull(mean_incidence_treated_pre)

# Add percent changes to results dfs
dengue_results_percent_att_df <- dengue_yearly_agg_results_df %>%
  rename(model_coefficient = estimate,
         model_coefficient_upper = upper,
         model_coefficient_lower = lower) %>%
  dplyr::select(-std_error, -p_value, -t_value) %>%
  mutate(pct_change = (model_coefficient / treated_pre_avg_incidence) * 100,
         pct_change_upper = (model_coefficient_upper / treated_pre_avg_incidence) * 100,
         pct_change_lower = (model_coefficient_lower / treated_pre_avg_incidence) * 100)

dengue_yearly_percent_results_df <- dengue_yearly_results_df %>%
  mutate(pct_change = (estimate / treated_pre_avg_incidence) * 100,
         pct_change_upper = (upper / treated_pre_avg_incidence) * 100,
         pct_change_lower = (lower / treated_pre_avg_incidence) * 100)

##############################
# Calculate attributable cases
##############################

# Step 1a: Define post-treatment years
post_years <- as.Date(c("2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01",
                        "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01",
                        "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01",
                        "2021-01-01", "2022-01-01"))

# Step 1b: Subset to treated group post-treatment
treated_post <- dengue_yearly$connected_buffered %>%
  filter(fivekm == 1, year %in% post_years)

# Step 1c: Expected cases in tx group = pre-treatment incidence x post-treatment population
treated_post <- treated_post %>%
  mutate(expected_cases = treated_pre_avg_incidence/1000 * population)

# Step 1d: Total expected cases in treated group post-treatment
total_expected_cases <- sum(treated_post$expected_cases, na.rm = TRUE)
dengue_results_percent_att_df <- dengue_results_percent_att_df %>%
  mutate(attributable_cases = (pct_change / 100) * total_expected_cases,
         attributable_cases_upper = (pct_change_upper / 100) * total_expected_cases,
         attributable_cases_lower = (pct_change_lower / 100) * total_expected_cases)

# Step 2: Total observed cases in the entire region post-paving (2009–2022)
total_observed_cases_region_post <- dengue_yearly$connected_buffered %>%
  filter(year %in% post_years) %>%
  summarise(total_cases = sum(yearly_cases_C, na.rm = TRUE)) %>%
  pull(total_cases)

# Step 3: Calculate proportion of attributable cases relative to entire region's burden
dengue_results_percent_att_df <- dengue_results_percent_att_df %>%
  mutate(
    attributable_case_prop_region = attributable_cases / total_observed_cases_region_post,
    attributable_case_prop_region_upper = attributable_cases_upper / total_observed_cases_region_post,
    attributable_case_prop_region_lower = attributable_cases_lower / total_observed_cases_region_post
  )

# Step 4: Make long so easier to view
dengue_results_percent_att_df_long <- dengue_results_percent_att_df %>%
  mutate(row = row_number()) %>%
  pivot_longer(
    -row,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    metric = variable %>%
      str_remove("_upper$") %>%
      str_remove("_lower$"),
    bound = case_when(
      str_ends(variable, "_upper") ~ "upper",
      str_ends(variable, "_lower") ~ "lower",
      variable %in% c("upper", "lower") ~ variable,
      TRUE ~ "estimate"
    )
  ) %>%
  dplyr::select(row, metric, bound, value) %>%
  pivot_wider(
    names_from = bound,
    values_from = value
  ) %>%
  dplyr::select(-row)

# Step 5: Report values in main text and store in results
View(dengue_results_percent_att_df_long)
write.csv(dengue_results_percent_att_df_long, "results/main_text_results/dengue_percent_change_attribution_results.csv", row.names=F)
View(dengue_yearly_percent_results_df)
write.csv(dengue_yearly_percent_results_df, "results/main_text_results/dengue_percent_change_yearly_results.csv", row.names=F)
