# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Conduct permutation inference analysis.
#
# Date created: 7/29/2025

library(fixest)
library(dplyr)

#####################
# Permutation Inference Analysis
#####################

# main specification aggregated effect estimate (for comparison)
dengue_yearly <- readRDS("data/analysis_ready_data/dengue_yearly_panels.rds")

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) == as.Date("2012-01-01"), 0, 1))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~clust,
  data = dengue_df_agg)
dengue_yearly_model_agg_effect_est <- coeftable(dengue_yearly_agg_model)[5,1]

#####################
# Full (randomize treatment across clusts and years)
#####################

permuted_effects_stor_full <- c()
for(i in 1:1000){
  set.seed(i)
  print(i)
  
  dengue_df_yearly_permuted_full  <- transform(dengue_df_agg, fivekm = sample(fivekm))
  dengue_yearly_model_permuted_full <- feols(incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
                                             vcov = ~clust,
                                             data = dengue_df_yearly_permuted_full)
  permuted_effect_est_full <- coeftable(dengue_yearly_model_permuted_full)[6,1]
  permuted_effects_stor_full <- c(permuted_effects_stor_full, permuted_effect_est_full)
}

#####################
# Block (randomize treatment across clusts but maintain temporal structure)
# run through each clust and randomly assign in or out of treatment (using derived probabilities)
#####################

dengue_df_yearly_permuted_block <- dengue_df_agg
permuted_effects_stor_block <- c()
for(i in 1:1000){
  set.seed(i)
  print(i)
  
  prob_1 <- length(which(dengue_df_agg$fivekm==1))/length(dengue_df_agg$fivekm)
  #randomly assign treatment to each spatial unit
  for(k in 1:length(unique(dengue_df_agg$key))){
    random_treatment <- rbinom(1, 1, prob_1)
    dengue_df_yearly_permuted_block$fivekm[which(dengue_df_yearly_permuted_block$key==k)] <- if (random_treatment==1) {rep(1,15)} else {rep(0,15)}
  }
  dengue_yearly_model_permuted_block <- feols(incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
                                              vcov = ~clust,
                                              data = dengue_df_yearly_permuted_block)
  permuted_effect_est_block <- coeftable(dengue_yearly_model_permuted_block)[5,1]
  permuted_effects_stor_block <- c(permuted_effects_stor_block, permuted_effect_est_block)
}

#####################
# Within (randomly assign the years that are flagged as treated within the keys that are truly part of the treatment group)
#####################

dengue_df_yearly_permuted_within <- dengue_yearly$connected_buffered

treated_keys <- dengue_df_yearly_permuted_within %>%
  filter(fivekm == 1) %>%
  pull(key) %>%
  unique()

years_available <- sort(unique(dengue_df_yearly_permuted_within$year))

permuted_effects_stor_within <- c()

for (i in 1:1000) {
  set.seed(i)
  print(i)
  
  permuted_df <- dengue_df_yearly_permuted_within %>%
    mutate(year_binary = 0)
  
  for (k in treated_keys) {
    #get the years this key appears in
    key_years <- permuted_df %>%
      filter(key == k) %>%
      pull(year)
    
    #randomly choose which of those years are treated
    n_years <- length(key_years)
    n_treated_years <- sample(1:(n_years - 1), 1)  # random number of treated years (not all or none)
    treated_years <- sample(key_years, n_treated_years)
    
    #assign 1 to those years
    permuted_df$year_binary[
      permuted_df$key == k & permuted_df$year %in% treated_years
    ] <- 1
  }
  
  permuted_df_agg <- permuted_df %>%
    filter(as.Date(year) > as.Date("2007-01-01"))
  
  model <- feols(
    incidence ~ year_binary:fivekm + urban + ag + sum_precip + mean_temp | key + year,
    vcov = ~clust,
    data = permuted_df)
  permuted_effect_est_within <- coeftable(model)["year_binary:fivekm", "Estimate"]

  permuted_effects_stor_within <- c(permuted_effects_stor_within, permuted_effect_est_within)
}


#####################
# store results
#####################
permuted_effects_df <- data.frame(
  effect = c(permuted_effects_stor_full,
             permuted_effects_stor_block,
             permuted_effects_stor_within),
  group = rep(c("full", "block", "within"), each = 1000))

write_rds(permuted_effects_df, "results/supplementary_models/sfig5_permuted_effects_df.rds")
