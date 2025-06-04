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
### dengue yearly model
#########################
dengue_yearly$buffered <- dengue_yearly$buffered %>%
  mutate(
    temp_c = mean_temp - 273.15,
    temp_c_scaled = temp_c - mean(temp_c),
    precip_scaled = sum_precip
  )

##option 0: original
dengue_yearly_model <- fepois(incidence_plus_1 ~ i(year, fivekm, ref = '2008-01-01') + sum_precip + mean_temp + log(urban) + log(ag) | cluster + year , 
                                vcov = "cluster", 
                                #fixef.rm = "none",
                                data = dengue_yearly$buffered)
#summary(dengue_yearly_model)
dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)-1),
         upper = (exp(upper_log)-1),
         lower = (exp(lower_log)-1))

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(labels = scales::percent)

##option 1: feols, log(normal) with incidence+1
dengue_yearly_model <- feols(
  log(incidence_plus_1) ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year,
  vcov = "cluster",
  #fixef.rm = "none",
  data = dengue_yearly$buffered)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)-1),
         upper = (exp(upper_log)-1),
         lower = (exp(lower_log)-1))

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(labels = scales::percent)

##option 1a: feols, log(normal) with incidence+1 but no all the time zeroes
dengue_yearly_model <- feols(
  log(incidence_plus_1) ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year,
  vcov = "cluster",
  #fixef.rm = "none",
  data = dengue_yearly_buffered_wo_zeroes)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)-1),
         upper = (exp(upper_log)-1),
         lower = (exp(lower_log)-1))

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(labels = scales::percent)

## option 2: fepois w yearly cases
dengue_yearly_model <- fepois(
  yearly_cases ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster,
  vcov = "cluster",
  offset = ~log(population),
  #fixef.rm = "none",
  data = dengue_yearly_buffered_wo_zero_years)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)-1),
         upper = (exp(upper_log)-1),
         lower = (exp(lower_log)-1))

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(labels = scales::percent)

## option 3: fenegbin w yearly cases, no FEs
dengue_yearly_model <- femlm(
  yearly_cases ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp + offset(log(population)),
  #vcov = "cluster",
  #fixef.rm = "none",
  family = "negbin",
  data = dengue_yearly$buffered_no_pm)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)),
         upper = (exp(upper_log)),
         lower = (exp(lower_log)))

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  scale_y_continuous(labels = scales::percent)

## option 4: fenegbin w yearly cases + 1
dengue_yearly_model <- femlm(
  cases_plus_1 ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp + offset(log(population)) | cluster + year,
  vcov = "cluster",
  family = "negbin",
  data = dengue_yearly$buffered_no_pm)

dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(upper_log = estimate + 1.96 * std_error,
         lower_log = estimate - 1.96 * std_error,
         estimate = (exp(estimate)-1),
         upper = (exp(upper_log)-1),
         lower = (exp(lower_log)-1))

fig2a <- ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(size=20),
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
fig2a

# all_zero <- dengue_yearly$buffered %>%
#   group_by(cluster) %>%
#   summarize(total_cases = sum(yearly_cases))

#########################
### dengue leish long difference biannual model
#########################

dengue_df_agg_biannual <- dengue_biannual$buffered %>%
  filter(as.Date(biannual_date) > as.Date("2006-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-04-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry <- filter(dengue_df_agg_biannual, month == "04")
dengue_df_rainy <- filter(dengue_df_agg_biannual, month == "10")

dengue_biannual_agg_model_dry <- fepois(
  incidence ~ biannual_binary * fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date,
  vcov = "cluster",
  data = dengue_df_dry)

dengue_biannual_agg_model_rainy <- fepois(
  incidence ~ biannual_binary * fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date,
  vcov = "cluster",
  data = dengue_df_rainy)

dengue_biannual_df <- bind_rows(
  as.data.frame(dengue_biannual_agg_model_dry$coeftable[5, ]),
  as.data.frame(dengue_biannual_agg_model_rainy$coeftable[5, ]))
colnames(dengue_biannual_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_biannual_df <- dengue_biannual_df %>%
  mutate(upper = exp(estimate + 1.96 * std_error) - 1,
         lower = exp(estimate - 1.96 * std_error) - 1,
         estimate = exp(estimate) - 1,
         rainy = c("Dry", "Rainy"))

#########################
### long difference model
#########################

dengue_df_agg <- dengue_yearly$buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- fepois(
  incidence ~ year_binary * fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year,
  vcov = "cluster",
  data = dengue_df_agg)

dengue_yearly_agg_df <- as.data.frame(dengue_yearly_agg_model$coeftable)[5, ]
colnames(dengue_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_agg_df <- dengue_yearly_agg_df %>%
  mutate(estimate = exp(estimate),
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

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

