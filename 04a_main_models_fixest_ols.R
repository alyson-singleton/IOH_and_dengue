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
### dengue yearly 
#########################

dengue_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  #weights = ~population,
  data = dengue_yearly$connected_buffered)
summary(dengue_yearly_model)
dengue_yearly_df <- as.data.frame(dengue_yearly_model$coeftable)[1:22, ]
colnames(dengue_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000)

ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor

#########################
### dengue biannual 
#########################

dengue_df_agg_biannual <- dengue_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

dengue_df_dry <- filter(dengue_df_agg_biannual, month == "04")
dengue_df_rainy <- filter(dengue_df_agg_biannual, month == "10")

dengue_biannual_agg_model_dry <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp | key + biannual_date,
  vcov = ~cluster,
  #weights = ~population,
  data = dengue_df_dry)
dengue_biannual_agg_model_dry

dengue_biannual_agg_model_rainy <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp | key + biannual_date,
  vcov = ~cluster,
  #weights = ~population,
  data = dengue_df_rainy)
dengue_biannual_agg_model_rainy

dengue_biannual_df <- as.data.frame(rbind(dengue_biannual_agg_model_dry$coeftable[5, ],
                                          dengue_biannual_agg_model_rainy$coeftable[5, ]))
colnames(dengue_biannual_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_biannual_df <- dengue_biannual_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000,
         rainy = c("Dry", "Rainy"))
dengue_biannual_df

#########################
### dengue long difference
#########################

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  #weights = ~population,
  data = dengue_df_agg)

dengue_yearly_agg_model
dengue_yearly_agg_df <- as.data.frame(dengue_yearly_agg_model$coeftable)[5, ]
colnames(dengue_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
dengue_yearly_agg_df <- dengue_yearly_agg_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000)
dengue_yearly_agg_df

#########################
### leish yearly model
#########################

leish_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  #weights = ~population,
  data = leish_yearly$connected_buffered)

leish_yearly_df <- as.data.frame(leish_yearly_model$coeftable)[1:22, ]
colnames(leish_yearly_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                          seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
leish_yearly_df <- leish_yearly_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000)

ggplot(leish_yearly_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("% change\ndengue\nincidence\nby year\nrelative\nto 2008") + 
  theme_bw() +
  theme_stor +
  ylim(-0.02,0.043)

#########################
### leish biannual model 
#########################

leish_df_agg_biannual <- leish_biannual$connected_buffered %>%
  filter(as.Date(biannual_date) > as.Date("2007-10-01")) %>%
  mutate(biannual_binary = if_else(as.Date(biannual_date) > as.Date("2008-10-01"), 1, 0),
         month = format(as.Date(biannual_date), "%m"))

leish_df_dry <- filter(leish_df_agg_biannual, month == "04")
leish_df_rainy <- filter(leish_df_agg_biannual, month == "10")

leish_biannual_agg_model_dry <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp | key + biannual_date,
  vcov = ~cluster,
  #weights = ~population,
  data = leish_df_dry)

leish_biannual_agg_model_rainy <- feols(
  incidence ~ biannual_binary * fivekm + urban + ag + sum_precip + mean_temp | key + biannual_date,
  vcov = ~cluster,
  #weights = ~population,
  data = leish_df_rainy)

leish_biannual_df <- as.data.frame(rbind(leish_biannual_agg_model_dry$coeftable[5, ],
                                         leish_biannual_agg_model_rainy$coeftable[5, ]))

colnames(leish_biannual_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_biannual_df <- leish_biannual_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000,
         rainy = c("Dry", "Rainy"))
leish_biannual_df

#########################
### leish long difference
#########################

leish_df_agg <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

leish_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip + mean_temp | key + year,
  vcov = ~cluster,
  #weights = ~population,
  data = leish_df_agg)

leish_yearly_agg_model
leish_yearly_agg_df <- as.data.frame(leish_yearly_agg_model$coeftable)[5, ]
colnames(leish_yearly_agg_df) <- c('estimate', 'std_error', 't_value', 'p_value')
leish_yearly_agg_df <- leish_yearly_agg_df %>%
  mutate(estimate = estimate*1000,
         upper = estimate + 1.96 * std_error*1000,
         lower = estimate - 1.96 * std_error*1000)
leish_yearly_agg_df

#########################
### save output
#########################

saveRDS(dengue_yearly_df, "outputs/dengue_yearly_model_results.rds")
saveRDS(dengue_biannual_df, "outputs/dengue_biannual_ld_results.rds")
saveRDS(dengue_yearly_agg_df, "outputs/dengue_yearly_ld_results.rds")
saveRDS(leish_yearly_df, "outputs/leish_yearly_model_results.rds")
saveRDS(leish_biannual_df, "outputs/leish_biannual_ld_results.rds")

