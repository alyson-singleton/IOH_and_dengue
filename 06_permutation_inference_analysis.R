
#####################
# Permutation Inference Analysis
#####################

# main specification aggregated effect estimate (for comparison)
dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  data = dengue_df_agg)

dengue_yearly_model_agg_effect_est <- coeftable(dengue_yearly_agg_model)[5,1]

#####################
# Full (randomize treatment across clusters and years)
#####################

permuted_effects_stor_full <- c()
for(i in 1:1000){
  set.seed(i)
  dengue_df_yearly_permuted_full  <- transform(dengue_df_agg, fivekm = sample(fivekm))
  dengue_yearly_model_permuted_full <- feols(incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
                                             vcov = ~cluster,
                                             data = dengue_df_yearly_permuted_full)
  permuted_effect_est_full <- coeftable(dengue_yearly_model_permuted_full)[6,1]
  permuted_effects_stor_full <- c(permuted_effects_stor_full, permuted_effect_est_full)
}

#####################
# Block (randomize treatment across clusters but maintain temporal structure)
# run through each cluster and randomly assign in or out of treatment (using derived probabilities)
#####################

dengue_df_yearly_permuted_block <- dengue_df_agg
permuted_effects_stor_block <- c()
for(i in 1:1000){
  set.seed(i)
  prob_1 <- length(which(dengue_df_agg$fivekm==1))/length(dengue_df_agg$fivekm)
  #randomly assign treatment to each spatial unit
  for(k in 1:length(unique(dengue_df_agg$cluster))){
    random_treatment <- rbinom(1, 1, prob_1)
    dengue_df_yearly_permuted_block$fivekm[which(dengue_df_yearly_permuted_block$cluster==k)] <- if (random_treatment==1) {rep(1,15)} else {rep(0,15)}
  }
  dengue_yearly_model_permuted_block <- feols(incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
                                              vcov = ~cluster,
                                              data = dengue_df_yearly_permuted_block)
  permuted_effect_est_block <- coeftable(dengue_yearly_model_permuted_block)[5,1]
  permuted_effects_stor_block <- c(permuted_effects_stor_block, permuted_effect_est_block)
}

#####################
# Within (randomly assign the years that are flagged as treated within the districts that are truly part of the treatment group)
#####################

dengue_df_yearly_permuted_within <- dengue_df_agg
permuted_effects_stor_within <- c()
for(i in 1:1000){
  set.seed(i)
  #randomly assign treatment to each spatial unit
  for(k in 1:length(unique(dengue_df_agg$cluster))){
    #random_treatment <- rbinom(1, 1, prob_1)
    dengue_df_yearly_permuted_within$year_binary[which(dengue_df_yearly_permuted_within$cluster==k)] <- sample(c(0,rep(1,14)))
  }
  dengue_yearly_model_permuted_within <- feols(incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
                                               vcov = ~cluster,
                                               data = dengue_df_yearly_permuted_within)
  permuted_effect_est_within <- coeftable(dengue_yearly_model_permuted_within)[5,1]
  permuted_effects_stor_within <- c(permuted_effects_stor_within, permuted_effect_est_within)
}

#####################
# store results
#####################