# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Run main models with fixest.
#
# Date created: 7/25/2025

library(fixest)
library(dplyr)
library(readr)
library(ggplot2)

# Load dengue panel datasets
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_yearly_cp <- readRDS("data/clean/dengue_yearly_cp_panels.rds")
dengue_biannual <- readRDS("data/clean/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/clean/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/clean/leish_biannual_panels.rds")

#########################
### dengue yearly main specification
#########################

dengue_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_yearly_results_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_results_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))

dengue_yearly_results_df <- dengue_yearly_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

#########################
### dengue yearly long difference main specification
#########################

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg)

dengue_yearly_agg_results_df <- as.data.frame(dengue_yearly_agg_model$coeftable)["year_binary:fivekm", ]
colnames(dengue_yearly_agg_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_agg_results_df <- dengue_yearly_agg_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

#########################
### dengue biannual main specification
#########################

dengue_biannual_model <- feols(
  incidence ~ i(biannual_date, fivekm, ref = '2008-04-01') + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = dengue_biannual$connected_buffered)

#########################
### dengue biannual long difference main specification
#########################

# data set up
dengue_df_agg_biannual <- dengue_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry <- filter(dengue_df_agg_biannual, month == "04")
dengue_df_rainy <- filter(dengue_df_agg_biannual, month == "10")

# run models
dengue_biannual_agg_model_dry <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = dengue_df_dry)

dengue_biannual_agg_model_rainy <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = dengue_df_rainy)

# store main results
dengue_biannual_results_df <- as.data.frame(rbind(dengue_biannual_agg_model_dry$coeftable["biannual_binary:fivekm", ],
                                          dengue_biannual_agg_model_rainy$coeftable["biannual_binary:fivekm", ]))
colnames(dengue_biannual_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_biannual_results_df <- dengue_biannual_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error,
         rainy = c("Dry", "Rainy"))

#########################
### leish yearly model main specification
#########################

leish_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = leish_yearly$connected_buffered)

leish_yearly_results_df <- as.data.frame(leish_yearly_model$coeftable)[1:22, ]
colnames(leish_yearly_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_yearly_results_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                          seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))

leish_yearly_results_df <- leish_yearly_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

#########################
### leish long difference main specification
#########################

leish_df_agg <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

leish_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = leish_df_agg)

leish_yearly_agg_results_df <- as.data.frame(leish_yearly_agg_model$coeftable)[5, ]
colnames(leish_yearly_agg_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')

leish_yearly_agg_results_df <- leish_yearly_agg_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

#########################
### leish biannual main specification
#########################

leish_biannual_model <- feols(
  incidence ~ i(biannual_date, fivekm, ref = '2008-04-01') + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = leish_biannual$connected_buffered)

#########################
### leish biannual long difference main specification
#########################

leish_df_agg_biannual <- leish_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

leish_df_dry <- filter(leish_df_agg_biannual, month == "04")
leish_df_rainy <- filter(leish_df_agg_biannual, month == "10")

leish_biannual_agg_model_dry <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = leish_df_dry)

leish_biannual_agg_model_rainy <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = leish_df_rainy)

leish_biannual_results_df <- as.data.frame(rbind(leish_biannual_agg_model_dry$coeftable[5, ],
                                         leish_biannual_agg_model_rainy$coeftable[5, ]))

colnames(leish_biannual_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_biannual_results_df <- leish_biannual_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error,
         rainy = c("Dry", "Rainy"))

#########################
### save output
#########################

saveRDS(dengue_yearly_results_df, "results/main_text_results/fig2_dengue_yearly_results.rds")
saveRDS(dengue_biannual_results_df, "results/main_text_results/fig2_dengue_biannual_ld_results.rds")
saveRDS(dengue_yearly_agg_results_df, "results/main_text_results/fig2_dengue_yearly_ld_results.rds")
saveRDS(leish_yearly_results_df, "results/main_text_results/fig2_leish_yearly_results.rds")
saveRDS(leish_biannual_results_df, "results/main_text_results/fig2_leish_biannual_ld_results.rds")
saveRDS(leish_yearly_agg_results_df, "results/main_text_results/fig2_leish_yearly_ld_results.rds")

table1 <- fixest::etable(dengue_yearly_agg_model,
                         dengue_biannual_agg_model_dry,
                         dengue_biannual_agg_model_rainy,
                         leish_yearly_agg_model,
                         leish_biannual_agg_model_dry,
                         leish_biannual_agg_model_rainy, 
                         digits = 3,
                         signif.code = c("*" = 0.05))
saveRDS(table1, "results/main_text_results/table1_main.rds")

stable1 <- fixest::etable(dengue_yearly_model, leish_yearly_model, 
                          digits = 3,
                          signif.code = c("*" = 0.05))
saveRDS(stable1, "results/supplementary_text_results/stable1_yearly_estimates.rds")
