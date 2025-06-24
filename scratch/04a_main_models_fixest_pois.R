library(fixest)
library(dplyr)
library(readr)

theme_stor <- theme(plot.title = element_text(size=20),
                    plot.title.position = "plot",
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=12),
                    axis.title.y=element_text(size=12,angle=0, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=10),
                    axis.title.x=element_text(size=14),
                    axis.text.x=element_text(size=12),
                    axis.text = element_text(size=14),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=12),
                    legend.position = "right",
                    strip.text.x = element_text(size = 12))

#########################
### dengue yearly model pois
#########################

dengue_yearly_model <- feglm(
  yearly_cases ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | key,
  vcov = "cluster",
  offset= ~log(population),
  family = "pois",
  data = dengue_yearly$full)
summary(dengue_yearly_model)
dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df <- dengue_yearly_df %>%
  rownames_to_column("rownames") %>%
  mutate(year = as.Date(str_extract(rownames, "\\d{4}-\\d{2}-\\d{2}"))) %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate_trans = exp(estimate)-1,
         upper = (exp(upper_log))-1,
         lower = (exp(lower_log))-1)

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate_trans), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    labels = scales::percent,
    limits = c(-2, 100)
  )

#############################
## option 3: poisson w pre tx as one group
#############################
  
dengue_yearly_collapsed <- dengue_yearly$full %>%
  mutate(
    year = as.Date(year),  # ensure year is Date
    year_group = case_when(
      year < as.Date("2008-01-01") ~ "pre",
      TRUE ~ as.character(year)
    ),
    year_group = factor(
      year_group,
      levels = c("pre", as.character(sort(unique(year[year >= as.Date("2008-01-01")]))))
    )
  )

dengue_yearly_model <- feglm(
  incidence ~ i(year_group, fivekm, ref = "2008-01-01") + log(urban) + log(ag) + sum_precip + mean_temp | key + cluster,
  vcov = "twoway",
  offset = ~log(population),
  family = "pois",
  data = dengue_yearly_collapsed
)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:15, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')

dengue_yearly_df <- dengue_yearly_df %>%
  rownames_to_column("rownames") %>%
  filter(grepl("year_group", rownames)) %>%
  mutate(
    year = ifelse(grepl("pre", rownames), "pre", str_extract(rownames, "\\d{4}-\\d{2}-\\d{2}")),
    year = ifelse(year == "pre", "2007-01-01", year),  # pseudo-date for plotting
    year = as.Date(year),
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    estimate_trans = exp(estimate) - 1,
    upper = exp(upper_log) - 1,
    lower = exp(lower_log) - 1)

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept = 0), colour = 'red', linewidth = 0.4) +
  geom_errorbar(aes(x = year, ymax = upper, ymin = lower), width = 0, linewidth = 0.5) +
  geom_vline(aes(xintercept = as.Date("2008-01-01")), linetype = 'dashed', linewidth = 0.4) +
  geom_point(aes(x = as.Date("2008-01-01"), y = 0), size = 3, shape = 21, fill = 'white') +
  geom_point(aes(year, estimate_trans), size = 3, shape = 21, fill = 'white') +
  xlab("Year") + 
  ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    labels = scales::percent,
    breaks = c(-2, 0, 10, 25, 50, 100, 200),
    limits = c(-2, 250))

#############################
# option 4: two pre tx periods
#############################

dengue_yearly_collapsed <- dengue_yearly$full %>%
  mutate(
    year = as.Date(year),  # ensure year is Date
    year_group = case_when(
      year < as.Date("2004-01-01") ~ "pre_early",
      year < as.Date("2008-01-01") ~ "pre_late",
      TRUE ~ as.character(year)
    ),
    year_group = factor(
      year_group,
      levels = c("pre_early", "pre_late", as.character(sort(unique(year[year >= as.Date("2008-01-01")]))))
    )
  )

dengue_yearly_model <- feglm(
  yearly_cases ~ i(year_group, fivekm, ref = "2008-01-01") + log(urban) + log(ag) + sum_precip + mean_temp | key,
  vcov = "cluster",
  offset = ~log(population),
  family = "pois",
  data = dengue_yearly_collapsed
)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:15, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')

dengue_yearly_df <- dengue_yearly_df %>%
  rownames_to_column("rownames") %>%
  filter(grepl("year_group", rownames)) %>%
  mutate(
    year = case_when(
      grepl("pre_early", rownames) ~ "2002-12-31",  # midpoint of 2000–2003
      grepl("pre_late", rownames) ~ "2006-12-31",   # midpoint of 2004–2007
      TRUE ~ str_extract(rownames, "\\d{4}-\\d{2}-\\d{2}")
    ),
    year = as.Date(year),
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    estimate_trans = exp(estimate) - 1,
    upper = exp(upper_log) - 1,
    lower = exp(lower_log) - 1
  )

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept = 0), colour = 'red', linewidth = 0.4) +
  geom_errorbar(aes(x = year, ymax = upper, ymin = lower), width = 0, linewidth = 0.5) +
  geom_vline(aes(xintercept = as.Date("2008-01-01")), linetype = 'dashed', linewidth = 0.4) +
  geom_point(aes(x = as.Date("2008-01-01"), y = 0), size = 3, shape = 21, fill = 'white') +
  geom_point(aes(year, estimate_trans), size = 3, shape = 21, fill = 'white') +
  xlab("Year") + 
  ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),
    labels = scales::percent,
    limits = c(-2, 70))

# all_zero <- dengue_yearly$buffered %>%
#   group_by(cluster) %>%
#   summarize(total_cases = sum(yearly_cases))

#########################
### dengue leish long difference biannual model
#########################

dengue_df_agg_biannual <- dengue_biannual$full %>%
  #filter(as.Date(biannual_date) > as.Date("2006-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry <- filter(dengue_df_agg_biannual, month == "04")
dengue_df_rainy <- filter(dengue_df_agg_biannual, month == "10")

dengue_biannual_agg_model_dry <- fepois(
  incidence ~ biannual_binary * onekm | key,
  #offset = ~log(population),
  vcov = "cluster",
  #family = "pois",
  data = dengue_df_dry)
dengue_biannual_agg_model_dry

dengue_biannual_agg_model_rainy <- fepois(
  incidence ~ biannual_binary * onekm | key,
  #offset = ~log(population),
  vcov = "cluster",
  #family = "pois",
  data = dengue_df_rainy)
dengue_biannual_agg_model_rainy

dengue_biannual_df <- as.data.frame(rbind(dengue_biannual_agg_model_dry$coeftable[2, ],
                            dengue_biannual_agg_model_rainy$coeftable[2, ]))
colnames(dengue_biannual_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_biannual_df <- dengue_biannual_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate_trans = exp(estimate) - 1,
         upper = exp(upper_log) - 1,
         lower = exp(lower_log) - 1,
         rainy = c("Dry", "Rainy"))
dengue_biannual_df

#########################
### long difference model
#########################

dengue_df_agg <- dengue_yearly$full %>%
  filter(as.Date(year) > as.Date("2006-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feglm(
  yearly_cases ~ year_binary * fivekm | key,
  offset = ~log(population),
  vcov = "cluster",
  family = "poisson",
  data = dengue_df_agg)

dengue_yearly_agg_model
dengue_yearly_agg_df <- as.data.frame(dengue_yearly_agg_model$coeftable)[2, ]
colnames(dengue_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_agg_df <- dengue_yearly_agg_df %>%
  mutate(estimate = exp(estimate),
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)
dengue_yearly_agg_df

#########################
### leish yearly model
#########################

leish_yearly_model <- feols(
  log(incidence) ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year,
  vcov = "cluster",
  data = leish_yearly$buffered)

leish_yearly_df <- as.data.frame(leish_yearly_model$coeftable)[1:22, ]
colnames(leish_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                          seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
leish_yearly_df <- leish_yearly_df %>%
  mutate(upper = exp(estimate + 1.96 * std_error) - 1,
         lower = exp(estimate - 1.96 * std_error) - 1,
         estimate = exp(estimate) - 1)

#########################
### leish long difference biannual model 
#########################

leish_df_agg_biannual <- leish_biannual$buffered %>%
  filter(as.Date(biannual_date) > as.Date("2006-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-04-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

leish_df_dry <- filter(leish_df_agg_biannual, month == "04")
leish_df_rainy <- filter(leish_df_agg_biannual, month == "10")

leish_biannual_agg_model_dry <- fepois(
  incidence ~ biannual_binary * fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date,
  vcov = "cluster",
  data = leish_df_dry)

leish_biannual_agg_model_rainy <- fepois(
  incidence ~ biannual_binary * fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date,
  vcov = "cluster",
  data = leish_df_rainy)

leish_biannual_df <- bind_rows(
  as.data.frame(leish_biannual_agg_model_dry$coeftable[5, ]),
  as.data.frame(leish_biannual_agg_model_rainy$coeftable[5, ]))

colnames(leish_biannual_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_biannual_df <- leish_biannual_df %>%
  mutate(upper = exp(estimate + 1.96 * std_error) - 1,
         lower = exp(estimate - 1.96 * std_error) - 1,
         estimate = exp(estimate) - 1,
         rainy = c("Dry", "Rainy"))

saveRDS(dengue_yearly_df, "outputs/dengue_yearly_model_results.rds")
saveRDS(dengue_biannual_df, "outputs/dengue_biannual_ld_results.rds")
saveRDS(dengue_yearly_agg_df, "outputs/dengue_yearly_ld_results.rds")
saveRDS(leish_yearly_df, "outputs/leish_yearly_model_results.rds")
saveRDS(leish_biannual_df, "outputs/leish_biannual_ld_results.rds")

