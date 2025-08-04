# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Run main models with glmmTMB.
#
# Date created: 7/25/2025

library(dplyr)
library(readr)
library(glmmTMB)
library(tibble)
library(performance)

# Load dengue panel datasets
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_yearly_cp <- readRDS("data/clean/dengue_yearly_cp_panels.rds")
dengue_biannual <- readRDS("data/clean/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/clean/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/clean/leish_biannual_panels.rds")

#########################
### dengue yearly long difference glmmTMB
#########################

dengue_df_agg_glmmTMB <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0),
         year = relevel(factor(year), ref = "2008-01-01"))

dengue_yearly_model_glmmTMB <- glmmTMB(
  incidence ~ year_binary * fivekm + sum_precip + mean_temp^2 + urban + ag +
    factor(key) + factor(year),
  data = dengue_df_agg_glmmTMB)

coefs <- summary(dengue_yearly_model_glmmTMB)$coefficients$cond
selected_terms_yearly <- c("year_binary:fivekm", "sum_precip", "mean_temp", "urban", "ag")
dengue_yearly_df_glmmTMB <- as.data.frame(coefs[selected_terms_yearly, , drop = FALSE]) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, std_error = `Std. Error`)

#########################
### dengue biannual long difference glmmTMB
#########################

dengue_df_agg_biannual_glmmTMB <- dengue_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry_glmmTMB <- filter(dengue_df_agg_biannual_glmmTMB, month == "04")
dengue_df_rainy_glmmTMB <- filter(dengue_df_agg_biannual_glmmTMB, month == "10")

dengue_biannual_model_dry_glmmTMB <- glmmTMB(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 +
    factor(key) + factor(biannual_date),
  data = dengue_df_dry_glmmTMB
)

dengue_biannual_model_rainy_glmmTMB <- glmmTMB(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 +
    factor(key) + factor(biannual_date),
  data = dengue_df_rainy_glmmTMB
)

selected_terms_biannual <- c("biannual_binary:fivekm", "sum_precip", "mean_temp", "urban", "ag")

dengue_biannual_df_glmmTMB <- bind_rows(
  summary(dengue_biannual_model_dry_glmmTMB)$coefficients$cond[selected_terms_biannual, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(model = "Dengue Biannual Dry"),
  summary(dengue_biannual_model_rainy_glmmTMB)$coefficients$cond[selected_terms_biannual, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(model = "Dengue Biannual Rainy")) %>%
  rename(estimate = Estimate, std_error = `Std. Error`)

#########################
### leish yearly long difference glmmTMB
#########################

leish_df_agg_glmmTMB <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0),
         year = relevel(factor(year), ref = "2008-01-01"))

leish_yearly_model_glmmTMB <- glmmTMB(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 +
    factor(key) + factor(year),
  data = leish_df_agg_glmmTMB
)

coefs <- summary(leish_yearly_model_glmmTMB)$coefficients$cond
leish_yearly_df_glmmTMB <- as.data.frame(coefs[selected_terms_yearly, , drop = FALSE]) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, std_error = `Std. Error`)

#########################
### leish biannual long difference glmmTMB
#########################

leish_df_agg_biannual_glmmTMB <- leish_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

leish_df_dry_glmmTMB <- filter(leish_df_agg_biannual_glmmTMB, month == "04")
leish_df_rainy_glmmTMB <- filter(leish_df_agg_biannual_glmmTMB, month == "10")

leish_biannual_model_dry_glmmTMB <- glmmTMB(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 +
    factor(key) + factor(biannual_date),
  data = leish_df_dry_glmmTMB
)

leish_biannual_model_rainy_glmmTMB <- glmmTMB(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 +
    factor(key) + factor(biannual_date),
  data = leish_df_rainy_glmmTMB
)

leish_biannual_df_glmmTMB <- bind_rows(
  summary(leish_biannual_model_dry_glmmTMB)$coefficients$cond[selected_terms_biannual, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(model = "Leish Biannual Dry"),
  
  summary(leish_biannual_model_rainy_glmmTMB)$coefficients$cond[selected_terms_biannual, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(model = "Leish Biannual Rainy")
) %>%
  rename(estimate = Estimate, std_error = `Std. Error`)

#########################
# save output
#########################

# STable6
all_glmmTMB_models <- bind_rows(
  dengue_yearly_df_glmmTMB %>% mutate(model = "Dengue Yearly"),
  dengue_biannual_df_glmmTMB,
  leish_yearly_df_glmmTMB %>% mutate(model = "Leish Yearly"),
  leish_biannual_df_glmmTMB
)

summary_table_glmmTMB <- all_glmmTMB_models %>%
  mutate(
    p_value = 2 * pnorm(-abs(estimate / std_error)),
    star = if_else(p_value < 0.05, "*", ""),
    estimate_se = sprintf("%.2f%s (%.2f)", estimate, star, std_error)
  ) %>%
  dplyr::select(term, model, estimate_se) %>%
  tidyr::pivot_wider(names_from = model, values_from = estimate_se)

summary_table_glmmTMB <- summary_table_glmmTMB[c(1,6,4:5,2:3),]
summary_table_glmmTMB
saveRDS(summary_table_glmmTMB, "results/supplementary_text_results/stable6_summary_table_glmmTMB.rds")

# STable7
saveRDS(dengue_yearly_model_glmmTMB, "results/supplementary_text_results/stable7_dengue_yearly_model_glmmTMB.rds")
