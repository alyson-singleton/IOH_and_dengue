#########################
### dengue yearly model
#########################

dengue_yearly_model <- fenegbin(incidence ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, 
                                vcov = "cluster", 
                                data = dengue_df_yearly_buffered)
df <- as.data.frame(dengue_yearly_model$coeftable)
df <- df[1:22,]
colnames(df) <- c('estimate', 'std_error', 't_value', 'p_value')
df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by="year"),
             seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by="year"))
df$upper <- df$estimate+1.96*df$std_error
df$lower <- df$estimate-1.96*df$std_error
df$estimate <- exp(df$estimate)-1
df$upper <- exp(df$upper)-1
df$lower <- exp(df$lower)-1

#########################
### dengue biannual model
#########################

### aggregated data (treatment)
dengue_df_agg_biannual <- dengue_df_biannual_buffered
dengue_df_agg_biannual <- dengue_df_agg_biannual[which(as.Date(dengue_df_biannual_buffered$biannual_date) > '2006-10-01'),]
dengue_df_agg_biannual$biannual_binary <- ifelse(as.Date(dengue_df_agg_biannual$biannual_date) > '2008-04-01',1, 0)
dengue_df_agg_biannual$month <- format(as.Date(dengue_df_agg_biannual$biannual_date), "%m")
dengue_df_agg_biannual_dry <- dengue_df_agg_biannual[which(dengue_df_agg_biannual$month == "04"),]
dengue_df_agg_biannual_rainy <- dengue_df_agg_biannual[which(dengue_df_agg_biannual$month == "10"),]

### aggregated models
dengue_biannual_agg_model_dry <- fepois(incidence ~ biannual_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date, vcov = "cluster", data = dengue_df_agg_biannual_dry)
summary(dengue_biannual_agg_model_dry)

dengue_biannual_agg_model_rainy <- fepois(incidence ~ biannual_binary*fivekm + log(urban) + log(ag) + sum_precip + mean_temp | cluster + biannual_date, vcov = "cluster", data = dengue_df_agg_biannual_rainy)
summary(dengue_biannual_agg_model_rainy)

df <- as.data.frame(rbind(dengue_biannual_agg_model_dry$coeftable[5,],
                          dengue_biannual_agg_model_rainy$coeftable[5,]))
colnames(df) <- c('estimate', 'std_error', 't_value', 'p_value')
df$upper <- df$estimate+1.96*df$std_error
df$lower <- df$estimate-1.96*df$std_error
df$estimate <- exp(df$estimate)-1
df$upper <- exp(df$upper)-1
df$lower <- exp(df$lower)-1
df$rainy <- c("Dry","Rainy")