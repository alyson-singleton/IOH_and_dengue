library(dplyr)
library(readr)

vert_line_date <- as.Date('2008-01-01')

################################
### process case data helper ###
################################

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
      
      keys_not_connected <- c(43, 78, 88:98) #see fig** for visualization
      
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
        no_pm = filter(df_clean, cluster != 1), #change this to specific keys eventually
        buffered = filter(df_clean, all_cutoffs %in% c(0,1,2,4,5,6,7)),
        buffered_no_pm = filter(df_clean, all_cutoffs %in% c(0,1,2,4,5,6,7), cluster != 1),
        
        connected = filter(df_clean, key_connected==T),
        connected_no_pm = filter(df_clean, key_connected==T, cluster != 1),
        connected_buffered = filter(df_clean, key_connected==T, all_cutoffs %in% c(0,1,2,4,5,6,7)),
        connected_buffered_no_pm = filter(df_clean, key_connected==T, all_cutoffs %in% c(0,1,2,4,5,6,7), cluster != 1),
        
        no_zero_case_keys = filter(df_clean, key_w_dengue==T),
        buffered_no_zero_case_keys = filter(df_clean, key_w_dengue==T, all_cutoffs %in% c(0,1,2,4,5,6,7))
      )
    }
}

###################
### dengue data ###
###################

# yearly
dengue_df_yearly_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_0km/dengue_yearly_full_dataset_c.csv")
dengue_yearly <- process_case_data(dengue_df_yearly_raw, "yearly_cases", year)

# biannual
dengue_df_biannual_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_0km/dengue_biannual_full_dataset_c.csv")
dengue_biannual <- process_case_data(dengue_df_biannual_raw, "biannual_cases", biannual_date)

###################
### leish data ####
###################

# yearly
leish_df_yearly_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_0km/leish_yearly_full_dataset_c.csv")
leish_yearly <- process_case_data(leish_df_yearly_raw, "yearly_cases", year)

# biannual
leish_df_biannual_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_0km/leish_biannual_full_dataset_c.csv")
leish_biannual <- process_case_data(leish_df_biannual_raw, "biannual_cases", biannual_date)
