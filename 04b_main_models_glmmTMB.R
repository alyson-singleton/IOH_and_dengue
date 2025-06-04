library(fixest)
library(dplyr)
library(readr)
library(glmmTMB)

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
dengue_yearly$buffered$year <- relevel(as.factor(dengue_yearly$buffered$year), ref = "2008-01-01")

##option 0: original
dengue_yearly_model <- fepois(incidence_plus_1 ~ i(year, fivekm, ref = '2008-01-01') + sum_precip + mean_temp + log(urban) + log(ag) | cluster + year , 
                              vcov = "cluster", 
                              data = dengue_yearly$buffered)

dengue_yearly$buffered$treat_post <- dengue_yearly$buffered$treat * dengue_yearly$buffered$post

#glmm model match original
model_match_original <- glmmTMB(cases_plus_1 ~ fivekm * factor(year) + factor(cluster) + sum_precip + mean_temp + log(urban) + log(ag) + offset(log(population)),
                                family = poisson(),
                                data = dengue_yearly$buffered)
summary(model_match_original)

coefs <- summary(model_match_original)$coefficients$cond
dengue_yearly_df <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  filter(grepl("fivekm:factor\\(year\\)", term)) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`) %>%
  mutate(year = gsub("fivekm:factor\\(year\\)", "", term),
         year = as.Date(year))
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    estimate = (exp(estimate) - 1),
    upper = (exp(upper_log) - 1),
    lower = (exp(lower_log) - 1))

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

#glmm model best w cases
model_best_cases <- glmmTMB(yearly_cases ~ fivekm * year + sum_precip + mean_temp + log(urban) + log(ag) + offset(log(population)),
                                family = nbinom2(),
                                data = dengue_yearly$no_pm)
summary(model_best_cases)

coefs <- summary(model_best_cases)$coefficients$cond
dengue_yearly_df <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  filter(grepl("fivekm:year", term)) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`) %>%
  mutate(year = gsub("fivekm:year", "", term),
         year = as.Date(year))
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    estimate = (exp(estimate) - 1),
    upper = (exp(upper_log) - 1),
    lower = (exp(lower_log) - 1))

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

#glmm log_normal_model
log_normal_model <- glmmTMB(log(incidence_plus_1) ~ fivekm * as.factor(year) + as.factor(cluster) + sum_precip + mean_temp + log(urban) + log(ag),
                                data = dengue_yearly$buffered)
summary(log_normal_model)
coefs <- summary(log_normal_model)$coefficients$cond
sigma2 <- sigma(log_normal_model)^2
dengue_yearly_df <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  filter(grepl("fivekm:as\\.factor\\(year\\)", term)) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`) %>%
  mutate(year = gsub("fivekm:as\\.factor\\(year\\)", "", term),
         year = as.Date(year))
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    # estimate = (exp(estimate + 0.5 * sigma2) - 1),
    # upper = (exp(upper_log + 0.5 * sigma2) - 1),
    # lower = (exp(lower_log + 0.5 * sigma2) - 1)
    estimate = (exp(estimate) - 1),
    upper = (exp(upper_log) - 1),
    lower = (exp(lower_log) - 1)
  )

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

#glmm log_normal_model without zeroes
dengue_yearly_buffered_wo_zeroes_ids <- dengue_yearly$buffered %>%
  group_by(cluster) %>%
  summarize(total_cases = sum(yearly_cases))
dengue_yearly_buffered_wo_zeroes_ids <- dengue_yearly_buffered_wo_zeroes_ids$cluster[-which(dengue_yearly_buffered_wo_zeroes_ids$total_cases==0)]
dengue_yearly_buffered_wo_zeroes <- dengue_yearly$buffered %>%
  filter(cluster %in% dengue_yearly_buffered_wo_zeroes_ids)

dengue_yearly_buffered_wo_zero_years <- dengue_yearly$buffered %>%
  filter(!year %in% c(as.Date("2003-01-01"),as.Date("2004-01-01"),as.Date("2006-01-01")))

log_normal_model <- glmmTMB(log(incidence_plus_1) ~ fivekm * as.factor(year) + as.factor(cluster) + sum_precip + mean_temp + log(urban) + log(ag),
                            data = dengue_yearly_buffered_wo_zeroes)
summary(log_normal_model)
coefs <- summary(log_normal_model)$coefficients$cond
sigma2 <- sigma(log_normal_model)^2
dengue_yearly_df <- as.data.frame(coefs) %>%
  rownames_to_column("term") %>%
  filter(grepl("fivekm:as\\.factor\\(year\\)", term)) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`) %>%
  mutate(year = gsub("fivekm:as\\.factor\\(year\\)", "", term),
         year = as.Date(year))
dengue_yearly_df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by = "year"),
                           seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by = "year"))
dengue_yearly_df <- dengue_yearly_df %>%
  mutate(
    upper_log = estimate + 1.96 * std_error,
    lower_log = estimate - 1.96 * std_error,
    # estimate = (exp(estimate + 0.5 * sigma2) - 1),
    # upper = (exp(upper_log + 0.5 * sigma2) - 1),
    # lower = (exp(lower_log + 0.5 * sigma2) - 1)
    estimate = (exp(estimate) - 1),
    upper = (exp(upper_log) - 1),
    lower = (exp(lower_log) - 1)
  )

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
## all aggregated models, show long difference models end up with similar estimates (new sup table)
