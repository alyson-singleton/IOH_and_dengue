# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Run models for distance heterogeneity analysis and bootstrapping.
#
# Date created: 7/25/2025

library(fixest)
library(tidyverse)
library(readr)

### load data
dengue_yearly <- readRDS("data/analysis_ready_data/dengue_yearly_panels.rds")

### build aggregated data
dengue_df_agg <- dengue_yearly$connected
dengue_df_agg <- dengue_df_agg[which(as.Date(dengue_df_agg$year) > '2007-01-01'),]
dengue_df_agg$year_binary <- ifelse(as.Date(dengue_df_agg$year) > '2008-01-01', 1, 0)

## bootstrap to get more conservative standard errors
treatment_col_names <- c('onekm','fivekm','tenkm','fifteenkm','twentykm','thirtykm')
std_error_stor <- c()
for(j in 1:5){
  #build control and treatment groups for each distance subgroup
  dengue_df_yearly_0s <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(6,7,0)),] #same control group each time
  dengue_df_yearly_1s <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(j)),] #loop through each distance group
  
  for(k in 1:1000){
    # sample key ids
    set.seed(k)
    dengue_df_yearly_sample0s <- sample(dengue_df_yearly_0s$key,
                                        length(unique(dengue_df_yearly_0s$key)),replace=T)
    dengue_df_yearly_sample1s <- sample(dengue_df_yearly_1s$key,
                                        length(unique(dengue_df_yearly_1s$key)),replace=T)
    ids_to_pull <- c(dengue_df_yearly_sample0s,dengue_df_yearly_sample1s)
    
    # build new dataset
    dengue_df_yearly_boot <- c()
    for(i in ids_to_pull) {
      og_rows_associated_w_id_i <- dengue_df_agg[which(dengue_df_agg$key == i),]
      dengue_df_yearly_boot <- rbind(dengue_df_yearly_boot, og_rows_associated_w_id_i)
    }
    
    #run model
    col_name <- treatment_col_names[j]
    dengue_yearly_model_boot <- feols(as.formula(paste0("incidence ~ year_binary*", 
                                                        col_name, 
                                                        "+ urban + ag + sum_precip + mean_temp | key + year")), 
                                      vcov = ~clust, 
                                      data = dengue_df_yearly_boot)
    
    #store standard error
    std_error <- coeftable(dengue_yearly_model_boot)[5,2]
    std_error_stor <- c(std_error_stor,std_error)
  }
}

# create datasets to run aggregated models
dengue_df_yearly_1km <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(1,6,7,0)),] #<1km
dengue_df_yearly_5km <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(2,6,7,0)),] #>1km, <5km
dengue_df_yearly_10km <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(3,6,7,0)),] #>5km, <10km
dengue_df_yearly_15km <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(4,6,7,0)),] #>10km, <15km
dengue_df_yearly_20km <- dengue_df_agg[which(dengue_df_agg$all_cutoffs %in% c(5,6,7,0)),] #>15km, <20km

# run models
dengue_yearly_model_1km <- feols(
  incidence ~ year_binary*onekm + urban + ag + sum_precip + mean_temp | key + year, 
  vcov = ~clust, 
  data = dengue_df_yearly_1km)
dengue_yearly_model_5km <- feols(
  incidence ~ year_binary*fivekm + urban + ag + sum_precip + mean_temp | key + year, 
  vcov = ~clust, 
  data = dengue_df_yearly_5km)
dengue_yearly_model_10km <- feols(
  incidence ~ year_binary*tenkm + urban + ag + sum_precip + mean_temp | key + year, 
  vcov = ~clust, 
  data = dengue_df_yearly_10km)
dengue_yearly_model_15km <- feols(
  incidence ~ year_binary*fifteenkm + urban + ag + sum_precip + mean_temp | key + year, 
  vcov = ~clust, 
  data = dengue_df_yearly_15km)
dengue_yearly_model_20km <- feols(
  incidence ~ year_binary*twentykm + urban + ag + sum_precip + mean_temp | key + year, 
  vcov = ~clust, 
  data = dengue_df_yearly_20km)

# link output together
dengue_distance_het_results_df <- as.data.frame(rbind(dengue_yearly_model_1km$coeftable[5,],
                                                      dengue_yearly_model_5km$coeftable[5,],
                                                      dengue_yearly_model_10km$coeftable[5,],
                                                      dengue_yearly_model_15km$coeftable[5,],
                                                      dengue_yearly_model_20km$coeftable[5,]))

dengue_distance_het_results_df <- dengue_distance_het_results_df %>%
  mutate(Cutoff = factor(c("1km", "5km", "10km", "15km", "20km"), 
                         levels = c("1km", "5km", "10km", "15km", "20km"))) %>%
  setNames(c("estimate", "std_error", "t_value", "p_value", "Cutoff")) %>%
  mutate(estimate = as.numeric(estimate))

# replace analytic standard errors w bootstrapped estimates
dengue_distance_het_results_df$std_error_boot <- c(mean(std_error_stor[1:1000]),
                                                   mean(std_error_stor[1001:2000]),
                                                   mean(std_error_stor[2001:3000]),
                                                   mean(std_error_stor[3001:4000]),
                                                   mean(std_error_stor[4001:5000]))

dengue_distance_het_results_df <- dengue_distance_het_results_df %>%
  mutate(upper = estimate + 1.96 * std_error_boot,
         lower = estimate - 1.96 * std_error_boot)

# store output
saveRDS(dengue_distance_het_results_df, "results/supplementary_models/dengue_distance_het_results_df.rds")
