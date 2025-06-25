# Permutation Inference Analysis

### aggregated data (treatment)
dengue_df_agg <- dengue_df_yearly_buffered
dengue_df_agg <- dengue_df_agg[which(as.Date(dengue_df_agg$year) > '2007-01-01'),]
dengue_df_agg$year_binary <- ifelse(as.Date(dengue_df_agg$year) > '2008-01-01',1,0)

# main specification aggregated effect estimate (for comparison)
dengue_yearly_model_agg <- fepois(incidence ~ year_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, 
                                  vcov = "cluster", 
                                  data = dengue_df_agg)
dengue_yearly_model_agg_effect_est <- coeftable(dengue_yearly_model_agg)[5,1]
dengue_yearly_model_agg_effect_est <- exp(dengue_yearly_model_agg_effect_est) - 1

#####################
## SFig 4a
#####################

# Full (randomize treatment across clusters and years)
permuted_effects_stor_full <- c()
for(i in 1:1000){
  set.seed(i)
  dengue_df_yearly_permuted_full  <- transform(dengue_df_agg, fivekm = sample(fivekm))
  dengue_yearly_model_permuted_full <- fepois(incidence ~ year_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, 
                                              vcov = "cluster", 
                                              data = dengue_df_yearly_permuted_full)
  permuted_effect_est_full <- coeftable(dengue_yearly_model_permuted_full)[6,1]
  permuted_effects_stor_full <- c(permuted_effects_stor_full, permuted_effect_est_full)
}

#####################
## SFig 4b
#####################

# Block (randomize treatment across clusters but maintain temporal structure)
# run through each cluster and randomly assign in or out of treatment (using derived probabilities)
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
  dengue_yearly_model_permuted_block <- fepois(incidence ~ year_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, 
                                               vcov = "cluster", 
                                               data = dengue_df_yearly_permuted_block)
  permuted_effect_est_block <- coeftable(dengue_yearly_model_permuted_block)[5,1]
  permuted_effects_stor_block <- c(permuted_effects_stor_block, permuted_effect_est_block)
}

#####################
## SFig 4c
#####################

# Within (randomly assign the years that are flagged as treated within the districts that are truly part of the treatment group)
dengue_df_yearly_permuted_within <- dengue_df_agg
permuted_effects_stor_within <- c()
for(i in 1:1000){
  set.seed(i)
  #randomly assign treatment to each spatial unit
  for(k in 1:length(unique(dengue_df_agg$cluster))){
    #random_treatment <- rbinom(1, 1, prob_1)
    dengue_df_yearly_permuted_within$year_binary[which(dengue_df_yearly_permuted_within$cluster==k)] <- sample(c(0,rep(1,14)))
  }
  dengue_yearly_model_permuted_within <- fepois(incidence ~ year_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, 
                                                vcov = "cluster", 
                                                data = dengue_df_yearly_permuted_within)
  permuted_effect_est_within <- coeftable(dengue_yearly_model_permuted_within)[5,1]
  permuted_effects_stor_within <- c(permuted_effects_stor_within, permuted_effect_est_within)
}