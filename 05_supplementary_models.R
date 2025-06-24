library(fixest)
library(dplyr)
library(readr)
library(ggplot2)

#########################
## main specification
#########################

dengue_yearly_model_main <- dengue_yearly_model

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_main <- dengue_yearly_agg_model

#########################
## no Puerto Maldonado
#########################

dengue_yearly_model_no_PM <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_yearly$connected_buffered_no_pm)

dengue_df_agg_no_pm <- dengue_yearly$connected_buffered_no_pm %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_no_PM <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg_no_pm)

#########################
## population weighting
#########################

dengue_yearly_model_pop_weight <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  weights = ~population,
  vcov = ~cluster,
  data = dengue_yearly$connected_buffered)

dengue_df_agg_pop_weight <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_pop_weight <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  weights = ~population,
  vcov = ~cluster,
  data = dengue_df_agg_pop_weight)

#########################
## changing the spatial boundary delineating treatment and control groups (onekm and tenkm)
#########################
dengue_yearly_model_onekm <- feols(
  incidence ~ i(year, onekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_yearly$connected_buffered)

dengue_ld_model_onekm <- feols(
  incidence ~ year_binary * onekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg)

dengue_yearly_model_tenkm <- feols(
  incidence ~ i(year, tenkm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_yearly$connected_buffered)

dengue_ld_model_tenkm <- feols(
  incidence ~ year_binary * tenkm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg)

#########################
## changing the buffer zone size between treatment and control groups
#########################
dengue_yearly_model_no_buffer <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_yearly$connected)

dengue_df_agg_no_buffer <- dengue_yearly$connected %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_no_buffer <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg_no_buffer)

dengue_df_yearly_buffer_bigger <- dengue_yearly$connected_buffered[which(dengue_yearly$connected_buffered$all_cutoffs %in% c(1,2,5,6,7,0)),]
dengue_yearly_model_buffer_bigger <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_yearly_buffer_bigger)

dengue_df_agg_bigger_buffer <- dengue_df_yearly_buffer_bigger %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_buffer_bigger <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg_bigger_buffer)

#########################
## confirmed and probable cases
#########################

dengue_yearly_model_cp <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_yearly_cp$connected_buffered)

dengue_df_agg_cp <- dengue_yearly_cp$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_ld_model_cp <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg_cp)

#########################
## quadratic temp and precip terms
#########################


#########################
## store results
#########################


