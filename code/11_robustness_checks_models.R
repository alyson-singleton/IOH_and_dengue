# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Run supplementary models for robustness checks.
#
# Date created: 7/25/2025

library(fixest)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(tibble)

# Load dengue panel datasets
dengue_yearly <- readRDS("data/analysis_ready_data/dengue_yearly_panels.rds")
dengue_yearly_cp <- readRDS("data/analysis_ready_data/dengue_yearly_cp_panels.rds")
dengue_biannual <- readRDS("data/analysis_ready_data/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/analysis_ready_data/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/analysis_ready_data/leish_biannual_panels.rds")

# -----------------------------------------------------------
# SFig S3 General Robustness Checks -------------------------
# -----------------------------------------------------------

#########################
## main specification
#########################

dengue_yearly_model_main <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_main <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg)

#########################
## no Puerto Maldonado
#########################

dengue_yearly_model_no_PM <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered_no_pm)

dengue_df_agg_no_pm <- dengue_yearly$connected_buffered_no_pm %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_no_PM <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_no_pm)

#########################
## population weighting
#########################

dengue_yearly_model_pop_weight <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  weights = ~population,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_df_agg_pop_weight <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_pop_weight <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  weights = ~population,
  vcov = ~clust,
  data = dengue_df_agg_pop_weight)

#########################
## changing the spatial boundary delineating treatment and control groups (onekm and tenkm)
#########################

dengue_df_agg_connected <- dengue_yearly$connected %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_model_onekm <- feols(
  incidence ~ i(year, onekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected)

dengue_ld_model_onekm <- feols(
  incidence ~ year_binary * onekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_connected)

dengue_yearly_model_tenkm <- feols(
  incidence ~ i(year, tenkm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected)

dengue_ld_model_tenkm <- feols(
  incidence ~ year_binary * tenkm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_connected)

#########################
## changing the buffer zone size between treatment and control groups
#########################

dengue_yearly_model_no_buffer <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected)

dengue_df_agg_no_buffer <- dengue_yearly$connected %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_no_buffer <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_no_buffer)

dengue_df_yearly_buffer_bigger <- dengue_yearly$connected_buffered[which(dengue_yearly$connected_buffered$all_cutoffs %in% c(1,2,5,6,7,0)),]
dengue_yearly_model_buffer_bigger <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_yearly_buffer_bigger)

dengue_df_agg_bigger_buffer <- dengue_df_yearly_buffer_bigger %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_buffer_bigger <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_bigger_buffer)

#########################
## confirmed and probable cases
#########################

dengue_yearly_model_cp <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly_cp$connected_buffered)

dengue_df_agg_cp <- dengue_yearly_cp$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_cp <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_cp)

#########################
## units with at least one dengue case during study period
#########################

dengue_yearly_model_w_dengue <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data =  dengue_yearly$buffered_no_zero_case_keys)

dengue_df_agg_w_dengue <- dengue_yearly$buffered_no_zero_case_keys %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_w_dengue <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg_w_dengue)

#########################
## no land use variables
#########################

dengue_yearly_model_no_land_use <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_ld_model_no_land_use <- feols(
  incidence ~ year_binary * fivekm + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg)

#########################
## store results
#########################

model_list <- list(
  "No PM" = dengue_yearly_model_no_PM,
  "No LUC" = dengue_yearly_model_no_land_use,
  "Pop weight" = dengue_yearly_model_pop_weight,
  "1km" = dengue_yearly_model_onekm,
  "10km" = dengue_yearly_model_tenkm,
  "No buffer" = dengue_yearly_model_no_buffer,
  "Big buffer" = dengue_yearly_model_buffer_bigger,
  "Conf & prob cases" = dengue_yearly_model_cp,
  "Units w dengue" = dengue_yearly_model_w_dengue,
  "Main" = dengue_yearly_model_main)

general_robustness_sfig3_df <- imap_dfr(model_list, ~ {
  as.data.frame(.x$coeftable[1:22,]) %>%
    mutate(Model = .y)
})

general_robustness_sfig3_df <- general_robustness_sfig3_df %>%
  setNames(c('estimate', 'std_error', 't_value', 'p_value', 'Model')) %>%
  mutate(
    year = rep(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year") %>%
                 append(seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year")),
               times = length(model_list)),
    estimate = as.numeric(estimate),
    std_error = as.numeric(std_error),
    upper = estimate + 1.96 * std_error,
    lower = estimate - 1.96 * std_error,
    Model = factor(Model, levels = c('No PM', 'No LUC', 'Pop weight', '1km', '10km',
                                     'No buffer', 'Big buffer', 'Conf & prob cases',
                                     'Units w dengue', 'Main')))

saveRDS(general_robustness_sfig3_df, "results/supplementary_models/sfig3_general_robustness_df.rds")

# -----------------------------------------------------------
# STable S3 Quadratic Precip --------------------------------
# -----------------------------------------------------------

# biannual data set up
dengue_df_agg_biannual <- dengue_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry <- filter(dengue_df_agg_biannual, month == "04")
dengue_df_rainy <- filter(dengue_df_agg_biannual, month == "10")

leish_df_agg <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

leish_df_agg_biannual <- leish_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

leish_df_dry <- filter(leish_df_agg_biannual, month == "04")
leish_df_rainy <- filter(leish_df_agg_biannual, month == "10")

#run models
dengue_yearly_ld_model_quad <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_df_agg)

dengue_biannual_ld_model_dry_quad <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = dengue_df_dry)

dengue_biannual_ld_model_rainy_quad<- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = dengue_df_rainy)

leish_yearly_ld_model_quad <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + year,
  vcov = ~clust,
  data = leish_df_agg)

leish_biannual_ld_model_dry_quad <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = leish_df_dry)

leish_biannual_ld_model_rainy_quad <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip^2 + mean_temp^2 | key + biannual_date,
  vcov = ~clust,
  data = leish_df_rainy)

#update this**
#saveRDS(general_robustness_sfig3_df, "results/supplementary_models/sfig3_general_robustness_df.rds")

# -----------------------------------------------------------
# SFig S4 Change Treatment Year -----------------------------
# -----------------------------------------------------------

#########################
## tx year = 2007
#########################

dengue_yearly_model_2007 <- feols(
  incidence ~ i(year, fivekm, ref = '2006-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_yearly_2007_df <- as.data.frame(dengue_yearly_model_2007$coeftable)[1:22, ]
colnames(dengue_yearly_2007_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_2007_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2005-01-01"), by = "year"),
                           seq(as.Date("2007-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_2007_df <- dengue_yearly_2007_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

#########################
## tx year = 2008
#########################

dengue_yearly_model_2008 <- feols(
  incidence ~ i(year, fivekm, ref = '2007-01-01') + urban + ag + sum_precip + mean_temp^2 | key + year,
  vcov = ~clust,
  data = dengue_yearly$connected_buffered)

dengue_yearly_2008_df <- as.data.frame(dengue_yearly_model_2008$coeftable)[1:22, ]
colnames(dengue_yearly_2008_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_2008_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2006-01-01"), by = "year"),
                                seq(as.Date("2008-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_2008_df <- dengue_yearly_2008_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

saveRDS(dengue_yearly_2007_df, "results/supplementary_models/sfig4_dengue_yearly_2007_df.rds")
saveRDS(dengue_yearly_2008_df, "results/supplementary_models/sfig4_dengue_yearly_2008_df.rds")
