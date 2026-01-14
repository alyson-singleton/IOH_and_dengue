# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Run main models with fixest.
#
# Date created: 1/13/2026

library(fixest)
library(dplyr)
library(readr)
library(ggplot2)

# Load dengue panel datasets
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_yearly_cp <- readRDS("data/clean/dengue_yearly_cp_panels.rds")
dengue_biannual <- readRDS("data/clean/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/clean/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/clean/leish_biannual_panels.rds")

#########################
### dengue yearly main specification diagnostics
#########################

dengue_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip_centered + 
    mean_temp_centered + mean_temp_centered_sq | key + year,
  vcov = ~clust,
  data = leish_yearly$connected_buffered)

# 1) Identification screening: dropped coefficients / collinearity flags
collinearity(dengue_yearly_model)

# 2) Numeric collinearity diagnostics on the within (demeaned) design
X  <- model.matrix(dengue_yearly_model)
f  <- dengue_yearly_model$fixef_id           # per-observation FE identifiers
Xw <- fixest::demean(X, f = f)

# drop near-constant columns after demeaning
sd_cols <- apply(Xw, 2, sd, na.rm = TRUE)
Xw <- Xw[, sd_cols > 0, drop = FALSE]

# Xw column names can differ (e.g., "mean_temp_centered_sq" vs transformed names)
# Keep those that match
keep_idx <- colnames(Xw)
Xw_sub <- Xw[, keep_idx, drop = FALSE]

# Compute VIFs manually (no dependency on car::vif, and avoids dummy outcome hacks)
vif_within <- sapply(seq_len(ncol(Xw_sub)), function(j){
  y <- Xw_sub[, j]
  Xj <- Xw_sub[, -j, drop = FALSE]
  # If only 1 covariate exists, VIF is 1
  if(ncol(Xj) == 0) return(1)
  r2 <- summary(lm(y ~ Xj))$r.squared
  1 / (1 - r2)
})
names(vif_within) <- colnames(Xw_sub)

vif_within
summary(vif_within)
max(vif_within)

# 3) Residual diagnostics (within residuals)
df_diag <- dengue_yearly$connected_buffered %>%
  mutate(resid  = resid(dengue_yearly_model),
         fitted = fitted(dengue_yearly_model))

# Residual vs fitted
ggplot(df_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Residuals vs fitted values (within residuals)") +
  theme_minimal()

# Residual distribution
ggplot(df_diag, aes(x = resid)) +
  geom_histogram(bins = 50, color = "white") +
  labs(x = "Residuals",
       y = "Count",
       title = "Distribution of model residuals") +
  theme_minimal()

qqnorm(df_diag$resid)
qqline(df_diag$resid)

