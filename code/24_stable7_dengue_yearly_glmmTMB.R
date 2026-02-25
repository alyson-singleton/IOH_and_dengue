# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S7.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load results
dengue_yearly_model_glmmTMB <- read_rds("results/supplementary_text_results/stable7_dengue_yearly_model_glmmTMB.rds")

#####################
## STable 7
#####################

coefs <- summary(dengue_yearly_model_glmmTMB)$coefficients$cond

dengue_yearly_df_full <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  ) %>%
  mutate(
    star = if_else(p_value < 0.05, "*", ""),
    formatted = sprintf("%.2f%s (%.2f)", estimate, star, std_error),
    term_clean = case_when(
      grepl("factor\\(key\\)", term) ~ gsub("factor\\(key\\)", "Unit FE: ", term),
      grepl("factor\\(year\\)", term) ~ gsub("factor\\(year\\)", "Year FE: ", term),
      TRUE ~ term
    )
  )%>%
  mutate(formatted = ifelse(is.na(estimate), "Dropped (collinear)", 
                            sprintf("%.2f%s (%.2f)", estimate, star, std_error)))

# Move main variables to the top
main_vars <- c("(Intercept)", "year_binary:fivekm", "year_binary", "fivekm", "urban", "ag", 
               "sum_precip_centered", "mean_temp_centered", "mean_temp_centered_sq")
dengue_yearly_df_full <- dengue_yearly_df_full %>%
  mutate(
    term_rank = match(term, main_vars),
    key_num = ifelse(grepl("^Unit FE: ", term_clean),
                     as.numeric(gsub("Unit FE: ", "", term_clean)),
                     NA),
    year_str = ifelse(grepl("^Year FE: ", term_clean),
                      term_clean,
                      NA)
  ) %>%
  arrange(
    is.na(term_rank), term_rank, # Main vars first
    is.na(key_num), key_num, # Then order Key FEs numerically
    year_str # Then Year FEs alphabetically
  )

stable7 <- dengue_yearly_df_full %>%
  dplyr::select(Term = term_clean, `Estimate (SE)` = formatted)
stable7 <- stable7[c(1,2,4,3,5:9,10:103),]
stable7[2:9,1] <- c("5km x Post-2008 Yearly", "5km", "Post-2008 Yearly", "Urban", "Agriculture", 
                    "Precipitation", "Temperature", "Temperature^2")
stable7

write.csv(stable7, "figures/stable7.csv")
