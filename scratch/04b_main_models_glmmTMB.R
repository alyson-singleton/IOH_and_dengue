library(dplyr)
library(readr)
library(glmmTMB)

#########################
### dengue long difference
#########################
dengue_df_agg <- dengue_df_agg %>%
  mutate(
    key = relevel(factor(key), ref = "your_ref_key"),
    year = relevel(factor(year), ref = "2008-01-01")
  )

dengue_df_agg_glmmTMB <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0),
         key = relevel(factor(key), ref = "84"),
         year = relevel(factor(year), ref = "2008-01-01"))

dengue_yearly_model_glmmTMB <- glmmTMB(incidence ~ 0 + year_binary * fivekm + sum_precip + mean_temp + urban + ag + factor(key) + factor(year),
                                       data = dengue_df_agg_glmmTMB)
summary(dengue_yearly_model_glmmTMB)

coefs <- summary(model_best_cases)$coefficients$cond
dengue_yearly_df <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  filter(grepl("fivekm:year", term)) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`) %>%
  mutate(year = gsub("fivekm:year", "", term),
         year = as.Date(year))
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))

#########################
### dengue long difference
#########################

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  #vcov = ~cluster,
  data = dengue_df_agg)

dengue_yearly_agg_model
dengue_yearly_agg_df <- as.data.frame(dengue_yearly_agg_model$coeftable)[5, ]
colnames(dengue_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_agg_df <- dengue_yearly_agg_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)
dengue_yearly_agg_df

#########################
### leish long difference
#########################

leish_df_agg <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

leish_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = leish_df_agg)

leish_yearly_agg_model
leish_yearly_agg_df <- as.data.frame(leish_yearly_agg_model$coeftable)[5, ]
colnames(leish_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_yearly_agg_df <- leish_yearly_agg_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)
leish_yearly_agg_df

#########################
### save output
#########################

saveRDS(dengue_yearly_df, "model_results/dengue_yearly_model_results.rds")
saveRDS(dengue_biannual_df, "model_results/dengue_biannual_ld_results.rds")
saveRDS(dengue_yearly_agg_df, "model_results/dengue_yearly_ld_results.rds")
saveRDS(leish_yearly_df, "model_results/leish_yearly_model_results.rds")
saveRDS(leish_biannual_df, "model_results/leish_biannual_ld_results.rds")
saveRDS(leish_yearly_agg_df, "model_results/leish_yearly_ld_results.rds")