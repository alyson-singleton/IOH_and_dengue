# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build final, analysis-ready panel datasets 
# (each RDS file is a list of datasets for main and sensitivity analyses).
#
# Date created: 7/23/2025

library(dplyr)
library(readr)
library(rlang)

process_case_data <- function(df, case_col, date_col) {
  date_col <- enquo(date_col)
  
  df %>%
    mutate(
      !!date_col := as.factor(!!date_col),
      incidence = (!!sym(case_col)) / population * 1000
    ) %>%
    filter(complete.cases(.), 
           incidence < 500, # remove extreme outliers (n=1, most likely due to inaccurate pop data)
           !key %in% c(101, 105, 106)) %>% # remove facilities without population data & that only existed pre tx
    {
      df_clean <- .
      
      keys_not_connected <- c(43, 88:98) #see Fig1 for visualization
      
      keys_with_cases <- df_clean %>%
        group_by(key) %>%
        summarise(total_cases = sum(!!sym(case_col), na.rm = TRUE)) %>%
        filter(total_cases > 0) %>%
        pull(key)
      
      df_clean <- df_clean %>%
        mutate(key_connected = !key %in% keys_not_connected,
               key_w_dengue = key %in% keys_with_cases)
      
      list(
        full = df_clean,
        no_pm = filter(df_clean, clust != 1),
        buffered = filter(df_clean, all_cutoffs %in% c(0,1,2,4,5,6,7)),
        buffered_no_pm = filter(df_clean, all_cutoffs %in% c(0,1,2,4,5,6,7), clust != 1),
        
        connected = filter(df_clean, key_connected==T),
        connected_no_pm = filter(df_clean, key_connected==T, clust != 1),
        connected_buffered = filter(df_clean, key_connected==T, all_cutoffs %in% c(0,1,2,4,5,6,7)),
        connected_buffered_no_pm = filter(df_clean, key_connected==T, all_cutoffs %in% c(0,1,2,4,5,6,7), clust != 1),
        
        no_zero_case_keys = filter(df_clean, key_w_dengue==T),
        buffered_no_zero_case_keys = filter(df_clean, key_w_dengue==T, all_cutoffs %in% c(0,1,2,4,5,6,7))
      )
    }
}

###################
### dengue data ###
###################

# yearly
dengue_df_yearly_raw <- read.csv("data/merged_data/dengue_yearly_merged_dataset.csv")
dengue_yearly <- process_case_data(dengue_df_yearly_raw, "yearly_cases_C", year)
saveRDS(dengue_yearly, "data/analysis_ready_data/dengue_yearly_panels.rds")

# yearly (conf & prob)
dengue_df_yearly_raw_cp <- read.csv("data/merged_data/dengue_yearly_merged_dataset.csv")
dengue_yearly_cp <- process_case_data(dengue_df_yearly_raw_cp, "yearly_cases_CP", year)
saveRDS(dengue_yearly_cp, "data/analysis_ready_data/dengue_yearly_cp_panels.rds")

# biannual
dengue_df_biannual_raw <- read.csv("data/merged_data/dengue_biannual_merged_dataset.csv")
dengue_biannual <- process_case_data(dengue_df_biannual_raw, "biannual_cases_C", biannual_date)
saveRDS(dengue_biannual, "data/analysis_ready_data/dengue_biannual_panels.rds")

###################
### leish data ####
###################

# yearly
leish_df_yearly_raw <- read.csv("data/merged_data/leish_yearly_merged_dataset.csv")
leish_yearly <- process_case_data(leish_df_yearly_raw, "yearly_cases_C", year)
saveRDS(leish_yearly, "data/analysis_ready_data/leish_yearly_panels.rds")

# biannual
leish_df_biannual_raw <- read.csv("data/merged_data/leish_biannual_merged_dataset.csv")
leish_biannual <- process_case_data(leish_df_biannual_raw, "biannual_cases_C", biannual_date)
saveRDS(leish_biannual, "data/analysis_ready_data/leish_biannual_panels.rds")
