# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S5.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)
library(fixest)

# Load models
dengue_yearly_agg_model <- read_rds("results/supplementary_text_results/stable5_dengue_yearly_agg_model.rds")
leish_yearly_agg_model <- read_rds("results/supplementary_text_results/stable5_leish_yearly_agg_model.rds")

#####################
## STable 5
#####################

# Extract fixed effects from both models
fe_vals_dengue <- fixef(dengue_yearly_agg_model)
fe_vals_leish <- fixef(leish_yearly_agg_model)

# Create labeled tibble for dengue
fe_df_dengue <- bind_rows(
  tibble(unit = names(fe_vals_dengue$key), effect_dengue = fe_vals_dengue$key, fixed_effect = "key"),
  tibble(unit = names(fe_vals_dengue$year), effect_dengue = fe_vals_dengue$year, fixed_effect = "year")
) %>%
  mutate(
    fe_label = case_when(
      fixed_effect == "key"  ~ paste0("Unit FE: ", unit),
      fixed_effect == "year" ~ paste0("Year FE: ", unit)
    )
  )

# Create labeled tibble for leishmaniasis
fe_df_leish <- bind_rows(
  tibble(unit = names(fe_vals_leish$key), effect_leish = fe_vals_leish$key, fixed_effect = "key"),
  tibble(unit = names(fe_vals_leish$year), effect_leish = fe_vals_leish$year, fixed_effect = "year")
) %>%
  mutate(
    fe_label = case_when(
      fixed_effect == "key"  ~ paste0("Unit FE: ", unit),
      fixed_effect == "year" ~ paste0("Year FE: ", unit)
    )
  ) %>%
  dplyr::select(fe_label, effect_leish)

# Join dengue and leish fixed effects by label
stable5 <- fe_df_dengue %>%
  left_join(fe_df_leish, by = "fe_label") %>%
  dplyr::select(Term = fe_label, Dengue_FE = effect_dengue, Leish_FE = effect_leish)

colnames(stable5) <- c("Term", "Dengue Intercept", "Leish Intercept")
stable5[,c(2:3)] <- round(stable5[,c(2:3)], 2)

# Export
write.csv(stable5, "figures/stable5.csv", row.names = F)
