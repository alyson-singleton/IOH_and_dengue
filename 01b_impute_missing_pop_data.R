# ID ----------------------------------------------------------------------
## Aly Singleton

#read in required packages
require(readxl)
require(tidyverse)
library(ggplot2)
library(showtext)
library(purrr)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

#################################### 
### load worldpop data (downloaded from google earth engine)
#################################### 

population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_0km/mdd_population_yearly.csv")
population_mdd <- population_mdd[,c(2:22,24)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'key')
population_mdd$`2021-01-01` <- NA
population_mdd$`2022-01-01` <- NA
population_mdd$`2023-01-01` <- NA
population_mdd_long <- population_mdd %>%
  pivot_longer(cols = c(1:21,23:25), 
               names_to = "year", 
               values_to = "population")
population_mdd_long$year <- as.Date(population_mdd_long$year)

#################################### 
### load cleaned diresa pop data
#################################### 

cleaned_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_all_years_final.csv")
cleaned_diresa_pop <- cleaned_diresa_pop[,c(2:19)]
colnames(cleaned_diresa_pop)[c(6:14)] <- c(as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))
colnames(cleaned_diresa_pop)[c(15:18)] <- c(as.character(seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by="years")))
cleaned_diresa_pop <- cleaned_diresa_pop[,c(1,6:18)]

# change into long format and group into clusters 
cleaned_diresa_pop <- cleaned_diresa_pop %>%
  pivot_longer(cols = c(2:14), 
               names_to = "year", 
               values_to = "population")%>%
  group_by(key,year) %>%
  summarize(population=sum(population, na.rm=T),
            year=max(year))
cleaned_diresa_pop$year <- as.Date(cleaned_diresa_pop$year)

#full join with worldpop data
all_pop <- full_join(cleaned_diresa_pop, population_mdd_long, by=c('key','year'))
colnames(all_pop) <- c('key', 'year', 'diresapop', 'worldpop')

#change zeros to NAs
all_pop$diresapop[which(all_pop$diresapop==0)] <- NA
all_pop$diresapop[which(is.nan(all_pop$diresapop))] <- NA

#get average ratio between the diresa and worldpop for EACH KEY
all_pop$individual_ratio <- all_pop$worldpop/all_pop$diresapop
all_pop <- all_pop %>%
  group_by(key) %>%
  mutate(average_ratio = mean(individual_ratio, na.rm=T))
all_pop$worldpop_adjusted <- all_pop$worldpop/all_pop$average_ratio

#fill in the blanks, keep the "known" values
all_pop$adjusted_pop <- all_pop$diresapop
#all_pop$adjusted_pop[which(is.na(all_pop$adjusted_pop))] <- all_pop$worldpop_adjusted[which(is.na(all_pop$adjusted_pop))]

#################################### 
## Fix edge cases with monotonicity
#################################### 

### NO DIRESA DATA
#clusters that dont have any worldpop/diresa overlap at all just use worldpop
all_pop_keys_wo_diresapop_2000_2020 <- all_pop %>%
  filter(year %in% seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year")) %>%
  group_by(key) %>%
  summarize(diresapop_sum = sum(diresapop, na.rm=TRUE), .groups = "drop")
keys_wo_diresa_2000_2020 <- all_pop_keys_wo_diresapop_2000_2020$key[all_pop_keys_wo_diresapop_2000_2020$diresapop_sum == 0]

all_pop$adjusted_pop[all_pop$key %in% keys_wo_diresa_2000_2020] <- all_pop$worldpop[all_pop$key %in% keys_wo_diresa_2000_2020]

#throw out 2023
all_pop <- all_pop[-which(all_pop$year=="2023-01-01"),]

### MONOTONIC INTERPOLATION: 2000–2008
# Scompute year-over-year growth ratios (forward direction)
worldpop_ratios_long <- population_mdd_long %>%
  filter(year >= as.Date("2000-01-01") & year <= as.Date("2009-01-01")) %>%
  arrange(key, year) %>%
  group_by(key) %>%
  mutate(growth_ratio = dplyr::lead(population) / population) %>%
  filter(year <= as.Date("2008-01-01")) %>%
  ungroup()

# get anchor pop_2009
diresa_2009 <- all_pop %>%
  filter(year == as.Date("2009-01-01")) %>%
  mutate(pop_2009 = if_else(is.na(diresapop), worldpop_adjusted, diresapop)) %>%
  dplyr::select(key, pop_2009)

# combine and compute backcasted values per key
backcast_values <- worldpop_ratios_long %>%
  left_join(diresa_2009, by = "key") %>%
  arrange(key, desc(year)) %>%
  group_by(key) %>%
  mutate(adjusted_pop = accumulate(growth_ratio, 
                                   .init = first(pop_2009), 
                                   .f = function(prev, ratio) prev / ratio)[-1]) %>%
  ungroup() %>%
  dplyr::select(key, year, adjusted_pop)

# merge into all_pop
all_pop <- all_pop %>%
  left_join(backcast_values, by = c("key", "year"), suffix = c("", "_backcasted")) %>%
  mutate(adjusted_pop = if_else(year <= as.Date("2008-01-01") & !is.na(adjusted_pop_backcasted),
                                adjusted_pop_backcasted,
                                adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_backcasted)

### MONOTONIC INTERPOLATION: 2018–2019
# get WorldPop data for 2017–2019 and compute growth ratios
worldpop_ratios_post2017 <- population_mdd_long %>%
  filter(year %in% as.Date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"))) %>%
  arrange(key, year) %>%
  group_by(key) %>%
  mutate(growth_ratio = dplyr::lead(population) / population) %>%
  filter(year %in% as.Date(c("2018-01-01", "2019-01-01"))) %>%
  ungroup()

# get DIRESA 2017 as anchor
anchor_2017 <- all_pop %>%
  filter(year == as.Date("2017-01-01")) %>%
  dplyr::select(key, adjusted_pop) %>%
  rename(pop_2017 = adjusted_pop)

# join anchor and compute forward
forward_2018_2019 <- worldpop_ratios_post2017 %>%
  left_join(anchor_2017, by = "key") %>%
  arrange(key, year) %>%
  group_by(key) %>%
  mutate(adjusted_pop = accumulate(growth_ratio,
                                   .init = first(pop_2017),
                                   .f = function(prev, ratio) prev * ratio)[-1]) %>%
  ungroup() %>%
  dplyr::select(key, year, adjusted_pop)

# link
all_pop <- all_pop %>%
  left_join(forward_2018_2019, by = c("key", "year"), suffix = c("", "_fwd")) %>%
  mutate(adjusted_pop = if_else(year %in% as.Date(c("2018-01-01", "2019-01-01")) & !is.na(adjusted_pop_fwd),
                                adjusted_pop_fwd,
                                adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_fwd)

# Linear interpolation for key 99 using 2009 DIRESA anchor and WorldPop shape
key99_interpolated <- population_mdd_long %>%
  filter(key == 99, year <= as.Date("2020-01-01")) %>%
  arrange(year) %>%
  mutate(
    pop_2009 = all_pop %>%
      filter(key == 99, year == as.Date("2009-01-01")) %>%
      pull(diresapop),
    worldpop_2009 = population[year == as.Date("2009-01-01")],
    interp_factor = pop_2009 / worldpop_2009,
    adjusted_pop = population * interp_factor
  ) %>%
  dplyr::select(key, year, adjusted_pop)
all_pop <- all_pop %>%
  left_join(key99_interpolated, by = c("key", "year"), suffix = c("", "_interp")) %>%
  mutate(adjusted_pop = if_else(key == 99 & is.na(adjusted_pop), adjusted_pop_interp, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_interp)

# Interpolate 2009–2013 for key 13 using DIRESA-adjusted 2008 and 2014 with WorldPop scaling
key13_interp <- population_mdd_long %>%
  filter(key == 13, year %in% as.Date(c("2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01"))) %>%
  arrange(year) %>%
  left_join(
    all_pop %>% filter(key == 13, year == as.Date("2008-01-01")) %>% dplyr::select(key, year, adj_2008 = adjusted_pop),
    by = c("key", "year" = "year")
  ) %>%
  left_join(
    all_pop %>% filter(key == 13, year == as.Date("2014-01-01")) %>% dplyr::select(key, year, adj_2014 = adjusted_pop),
    by = c("key", "year" = "year")
  ) %>%
  fill(adj_2008, adj_2014, .direction = "downup") %>%
  mutate(
    interp_factor = (population - population[year == as.Date("2008-01-01")]) /
      (population[year == as.Date("2014-01-01")] - population[year == as.Date("2008-01-01")]),
    adjusted_pop = adj_2008 + interp_factor * (adj_2014 - adj_2008)
  ) %>%
  filter(year >= as.Date("2009-01-01") & year <= as.Date("2013-01-01")) %>%
  dplyr::select(key, year, adjusted_pop)
all_pop <- all_pop %>%
  left_join(key13_interp, by = c("key", "year"), suffix = c("", "_interp")) %>%
  mutate(adjusted_pop = if_else(key == 13 & is.na(adjusted_pop), adjusted_pop_interp, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_interp)

# Interpolate 2009–2010 for key 28 using DIRESA-adjusted 2008 and 2011 with WorldPop scaling
key28_interp <- population_mdd_long %>%
  filter(key == 28, year %in% as.Date(c("2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01"))) %>%
  arrange(year) %>%
  left_join(
    all_pop %>% filter(key == 28, year == as.Date("2008-01-01")) %>% dplyr::select(key, year, adj_2008 = adjusted_pop),
    by = c("key", "year" = "year")
  ) %>%
  left_join(
    all_pop %>% filter(key == 28, year == as.Date("2011-01-01")) %>% dplyr::select(key, year, adj_2011 = adjusted_pop),
    by = c("key", "year" = "year")
  ) %>%
  fill(adj_2008, adj_2011, .direction = "downup") %>%
  mutate(
    interp_factor = (population - population[year == as.Date("2008-01-01")]) /
      (population[year == as.Date("2011-01-01")] - population[year == as.Date("2008-01-01")]),
    adjusted_pop = adj_2008 + interp_factor * (adj_2011 - adj_2008)
  ) %>%
  filter(year >= as.Date("2009-01-01") & year <= as.Date("2010-01-01")) %>%
  dplyr::select(key, year, adjusted_pop)

all_pop <- all_pop %>%
  left_join(key28_interp, by = c("key", "year"), suffix = c("", "_interp")) %>%
  mutate(adjusted_pop = if_else(key == 28 & is.na(adjusted_pop), adjusted_pop_interp, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_interp)

# Interpolate 2014–2019 for key 103 using DIRESA-adjusted 2013 and 2020 with WorldPop scaling
key103_interp <- population_mdd_long %>%
  filter(key == 103, year %in% as.Date(c("2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"))) %>%
  arrange(year) %>%
  left_join(
    all_pop %>% filter(key == 103, year == as.Date("2013-01-01")) %>% dplyr::select(key, year, adj_2013 = adjusted_pop),
    by = c("key", "year" = "year")
  ) %>%
  left_join(
    all_pop %>% filter(key == 103, year == as.Date("2020-01-01")) %>% dplyr::select(key, year, adj_2020 = worldpop),
    by = c("key", "year" = "year")
  ) %>%
  fill(adj_2013, adj_2020, .direction = "downup") %>%
  mutate(
    interp_factor = (population - population[year == as.Date("2013-01-01")]) /
      (population[year == as.Date("2020-01-01")] - population[year == as.Date("2013-01-01")]),
    adjusted_pop = adj_2013 + interp_factor * (adj_2020 - adj_2013)
  ) %>%
  filter(year >= as.Date("2014-01-01") & year <= as.Date("2019-01-01")) %>%
  dplyr::select(key, year, adjusted_pop)

all_pop <- all_pop %>%
  left_join(key103_interp, by = c("key", "year"), suffix = c("", "_interp")) %>%
  mutate(adjusted_pop = if_else(key == 103 & is.na(adjusted_pop), adjusted_pop_interp, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_interp)


# Interpolate 2020 for key 100 using adjusted 2019 and DIRESA 2021 with WorldPop scaling
# Get WorldPop values for 2019–2020
wp_2019_2020 <- population_mdd_long %>%
  filter(key == 100, year %in% as.Date(c("2019-01-01", "2020-01-01"))) %>%
  arrange(year)

wp_2019 <- wp_2019_2020$population[wp_2019_2020$year == as.Date("2019-01-01")]
wp_2020 <- wp_2019_2020$population[wp_2019_2020$year == as.Date("2020-01-01")]

adj_2019 <- all_pop %>%
  filter(key == 100, year == as.Date("2019-01-01")) %>%
  pull(adjusted_pop)

diresa_2021 <- all_pop %>%
  filter(key == 100, year == as.Date("2021-01-01")) %>%
  pull(diresapop)

interp_factor <- (wp_2020 - wp_2019) / (wp_2019)  # relative change from 2019 to 2020
adj_2020 <- adj_2019 + interp_factor * (diresa_2021 - adj_2019)

key100_interp <- tibble(
  key = 100,
  year = as.Date("2020-01-01"),
  adjusted_pop = adj_2020
)

all_pop <- all_pop %>%
  left_join(key100_interp, by = c("key", "year"), suffix = c("", "_interp")) %>%
  mutate(adjusted_pop = if_else(key == 100 & is.na(adjusted_pop), adjusted_pop_interp, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_interp)




### LINEARLY INTERPOLATE: 2021-2022
# Fill 2021–2022 for keys with DIRESA missing in those years but with WorldPop through 2020
pop_2021_2022 <- all_pop %>%
  filter(year %in% as.Date(c("2021-01-01", "2022-01-01"))) %>%
  group_by(key) %>%
  summarise(diresa_missing = all(is.na(diresapop)), .groups = "drop") %>%
  filter(diresa_missing) %>%
  pull(key) %>%
  { 
    keys_missing <- . 
    worldpop_recent <- population_mdd_long %>%
      filter(key %in% keys_missing,
             year %in% as.Date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"))) %>%
      arrange(key, year) %>%
      group_by(key) %>%
      mutate(growth_ratio = dplyr::lead(population) / population) %>%
      filter(!is.na(growth_ratio)) %>%
      summarise(avg_growth = mean(growth_ratio), .groups = "drop")
    
    pop_2020 <- all_pop %>%
      filter(key %in% keys_missing, year == as.Date("2020-01-01")) %>%
      dplyr::select(key, adjusted_pop) %>%
      rename(pop_2020 = adjusted_pop)
    
    pop_2020 %>%
      left_join(worldpop_recent, by = "key") %>%
      mutate(
        `2021-01-01` = pop_2020 * avg_growth,
        `2022-01-01` = pop_2020 * avg_growth^2
      ) %>%
      pivot_longer(cols = starts_with("202"), names_to = "year", values_to = "adjusted_pop") %>%
      mutate(year = as.Date(year))
  }

# Merge into all_pop
all_pop <- all_pop %>%
  left_join(pop_2021_2022, by = c("key", "year"), suffix = c("", "_extrap")) %>%
  mutate(adjusted_pop = if_else(!is.na(adjusted_pop_extrap), adjusted_pop_extrap, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_extrap)

### LINEARLY INTERPOLATE: 2020-2022
# Fill 2020–2022 for keys with DIRESA missing in those years but with WorldPop through 2020
pop_2020_2022 <- all_pop %>%
  filter(year %in% as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))) %>%
  group_by(key) %>%
  summarise(diresa_missing = all(is.na(diresapop)), .groups = "drop") %>%
  filter(diresa_missing) %>%
  pull(key) %>%
  { 
    keys_missing <- . 
    worldpop_recent <- population_mdd_long %>%
      filter(key %in% keys_missing,
             year %in% as.Date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"))) %>%
      arrange(key, year) %>%
      group_by(key) %>%
      mutate(growth_ratio = dplyr::lead(population) / population) %>%
      filter(!is.na(growth_ratio)) %>%
      summarise(avg_growth = mean(growth_ratio), .groups = "drop")
    
    pop_2019 <- all_pop %>%
      filter(key %in% keys_missing, year == as.Date("2019-01-01")) %>%
      dplyr::select(key, adjusted_pop) %>%
      rename(pop_2019 = adjusted_pop)
    
    pop_2019 %>%
      left_join(worldpop_recent, by = "key") %>%
      mutate(
        `2020-01-01` = pop_2019 * avg_growth,
        `2021-01-01` = pop_2019 * avg_growth^2,
        `2022-01-01` = pop_2019 * avg_growth^3
      ) %>%
      pivot_longer(cols = starts_with("202"), names_to = "year", values_to = "adjusted_pop") %>%
      mutate(year = as.Date(year))
  }

# Merge into all_pop
all_pop <- all_pop %>%
  left_join(pop_2020_2022, by = c("key", "year"), suffix = c("", "_extrap")) %>%
  mutate(adjusted_pop = if_else(!is.na(adjusted_pop_extrap), adjusted_pop_extrap, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_extrap)

#reduce to final columns
adjusted_diresa_pop <- all_pop[,c('key', 'year', 'adjusted_pop')]
colnames(adjusted_diresa_pop) <- c('key', 'year', 'population')

#export imputed population data
write.csv(adjusted_diresa_pop, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final_0km.csv")

#################################### 
## SCRATCH
#################################### 
#individual ratios
#all_pop$individual_ratio <- NA
#for(i in 1:dim(all_pop)[1]){
#  all_pop$individual_ratio[i] <- all_pop$worldpop[i+1]/all_pop$worldpop[i]
#}


#remove these in the end (few dengue cases and no pop data, all past/non stable 101 105 106)
