library(dplyr)
library(readr)

vert_line_date <- as.Date('2008-01-01')

################################
### process case data helper ###
################################

process_case_data <- function(df, case_col, date_col, drop_pm = TRUE) {
  df %>%
    mutate(
      {{ date_col }} := as.factor({{ date_col }}),
      incidence = (!!sym(case_col)) / population, # cases + 1 so 0's dont go to infinity when logged
      incidence_plus_1 = (!!sym(case_col)+1) / population, # cases + 1 so 0's dont go to infinity when logged
      cases_plus_1 = (!!sym(case_col)+1), # cases + 1 so 0's dont go to infinity when logged
      urban = urban + 0.001, # land use vars + 0.001 so 0's dont go to infinity when logged
      ag = ag + 0.001
    ) %>%
    filter(complete.cases(.), incidence < 0.5) %>% # remove extreme outliers (n=1, most likely due to inaccurate pop data)
    {
      list(
        full = .,
        no_pm = if (drop_pm) filter(., cluster != 1) else .,
        buffered = filter(., all_cutoffs %in% c(0,1,2,4,5,6,7)),
        buffered_no_pm = filter(., all_cutoffs %in% c(0,1,2,4,5,6,7), cluster != 1)
      )
    }
}

###################
### dengue data ###
###################

# yearly
dengue_df_yearly_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_yearly_full_dataset_c.csv")
dengue_yearly <- process_case_data(dengue_df_yearly_raw, "yearly_cases", year)

# biannual
dengue_df_biannual_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_biannual_full_dataset_c.csv")
dengue_biannual <- process_case_data(dengue_df_biannual_raw, "biannual_cases", biannual_date)

###################
### leish data ####
###################

# yearly
leish_df_yearly_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/leish_yearly_full_dataset_c.csv")
leish_yearly <- process_case_data(leish_df_yearly_raw, "yearly_cases", year)

# biannual
leish_df_biannual_raw <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/leish_biannual_full_dataset_c.csv")
leish_biannual <- process_case_data(leish_df_biannual_raw, "biannual_cases", biannual_date)
