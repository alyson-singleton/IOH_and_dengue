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
library(patchwork)

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
  data = dengue_yearly$connected_buffered)

# 1) Any dropped coefficients?
collinearity(dengue_yearly_model)

# 2) Numeric collinearity diagnostics on within (demeaned) covariates
dengue_yearly_model_matrix <- model.matrix(dengue_yearly_model)
fes <- dengue_yearly_model$fixef_id
demeaned_values <- fixest::demean(dengue_yearly_model_matrix, f = fes)

# compute VIFs after demeaning via fixed effects
dengue_yearly_vif_within <- sapply(seq_len(ncol(demeaned_values)), function(j){ #j is each covariate/column
  y <- demeaned_values[, j]
  Xj <- demeaned_values[, -j, drop = FALSE]
  r2 <- summary(lm(y ~ Xj))$r.squared #pull r2
  1 / (1 - r2) #calculate "vif"
})

names(dengue_yearly_vif_within) <- colnames(demeaned_values)
dengue_yearly_vif_within
summary(dengue_yearly_vif_within)
max(dengue_yearly_vif_within)

# 3) Residual diagnostics (within residuals)
dengue_yearly_df_diag <- dengue_yearly$connected_buffered %>%
  mutate(resid  = resid(dengue_yearly_model),
         fitted = fitted(dengue_yearly_model))

# Residual vs fitted
sfig13a <- ggplot(dengue_yearly_df_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Dengue yearly model (Eq. 1)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
sfig13a

# Residual distribution
sfig13b <- ggplot(df_diag, aes(x = resid)) +
  geom_histogram(bins = 75, color = "white") +
  labs(x = "Residuals",
       y = "Count",
       title = "") +
  theme_minimal()
sfig13b

#########################
### dengue agg main specification diagnostics
#########################

dengue_df_agg <- dengue_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

dengue_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip_centered + 
    mean_temp_centered + mean_temp_centered_sq | key + year,
  vcov = ~clust,
  data = dengue_df_agg)

# 1) Any dropped coefficients?
collinearity(dengue_yearly_agg_model)

# 2) Numeric collinearity diagnostics on within (demeaned) covariates
dengue_agg_model_matrix <- model.matrix(dengue_yearly_agg_model)
fes <- dengue_yearly_agg_model$fixef_id
demeaned_values <- fixest::demean(dengue_agg_model_matrix, f = fes)

# compute VIFs after demeaning via fixed effects
dengue_agg_vif_within <- sapply(seq_len(ncol(demeaned_values)), function(j){ #j is each covariate/column
  y <- demeaned_values[, j]
  Xj <- demeaned_values[, -j, drop = FALSE]
  r2 <- summary(lm(y ~ Xj))$r.squared #pull r2
  1 / (1 - r2) #calculate "vif"
})

names(dengue_agg_vif_within) <- colnames(demeaned_values)
dengue_agg_vif_within
summary(dengue_agg_vif_within)
max(dengue_agg_vif_within)

# 3) Residual diagnostics (within residuals)
dengue_agg_df_diag <- dengue_df_agg %>%
  mutate(resid  = resid(dengue_yearly_agg_model),
         fitted = fitted(dengue_yearly_agg_model))

# Residual vs fitted
sfig13c <- ggplot(dengue_agg_df_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Dengue aggregated model (Eq. 2)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
sfig13c

# Residual distribution
sfig13d <- ggplot(dengue_agg_df_diag, aes(x = resid)) +
  geom_histogram(bins = 75, color = "white") +
  labs(x = "Residuals",
       y = "Count",
       title = "") +
  theme_minimal() 
sfig13d

#########################
### leish yearly main specification diagnostics
#########################

leish_yearly_model <- feols(
  incidence ~ i(year, fivekm, ref = '2008-01-01') + urban + ag + sum_precip_centered + 
    mean_temp_centered + mean_temp_centered_sq | key + year,
  vcov = ~clust,
  data = leish_yearly$connected_buffered)

# 1) Any dropped coefficients?
collinearity(leish_yearly_model)

# 2) Numeric collinearity diagnostics on within (demeaned) covariates
leish_yearly_model_matrix <- model.matrix(leish_yearly_model)
fes <- leish_yearly_model$fixef_id
demeaned_values <- fixest::demean(leish_yearly_model_matrix, f = fes)

# compute VIFs after demeaning via fixed effects
leish_yearly_vif_within <- sapply(seq_len(ncol(demeaned_values)), function(j){ #j is each covariate/column
  y <- demeaned_values[, j]
  Xj <- demeaned_values[, -j, drop = FALSE]
  r2 <- summary(lm(y ~ Xj))$r.squared #pull r2
  1 / (1 - r2) #calculate "vif"
})

names(leish_yearly_vif_within) <- colnames(leish_yearly_vif_within)
vif_within
summary(vif_within)
max(vif_within)

# 3) Residual diagnostics (within residuals)
leish_yearly_df_diag <- leish_yearly$connected_buffered %>%
  mutate(resid  = resid(leish_yearly_model),
         fitted = fitted(leish_yearly_model))

# Residual vs fitted
sfig13e <- ggplot(leish_yearly_df_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Leish yearly model (Eq. 1)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
sfig13e

# Residual distribution
sfig13f <- ggplot(leish_yearly_df_diag, aes(x = resid)) +
  geom_histogram(bins = 50, color = "white") +
  labs(x = "Residuals",
       y = "Count",
       title = "") +
  theme_minimal()
sfig13f

#########################
### leish agg main specification diagnostics
#########################

leish_df_agg <- leish_yearly$connected_buffered %>%
  filter(as.Date(year) > as.Date("2007-01-01")) %>%
  mutate(year_binary = if_else(as.Date(year) > as.Date("2008-01-01"), 1, 0))

leish_yearly_agg_model <- feols(
  incidence ~ year_binary * fivekm + urban + ag + sum_precip_centered + mean_temp_centered + 
    mean_temp_centered_sq | key + year,
  vcov = ~clust,
  data = leish_df_agg)

# 1) Any dropped coefficients?
collinearity(leish_yearly_agg_model)

# 2) Numeric collinearity diagnostics on within (demeaned) covariates
leish_agg_model_matrix <- model.matrix(leish_yearly_agg_model)
fes <- leish_yearly_agg_model$fixef_id
demeaned_values <- fixest::demean(leish_agg_model_matrix, f = fes)

# compute VIFs after demeaning via fixed effects
leish_agg_vif_within <- sapply(seq_len(ncol(demeaned_values)), function(j){ #j is each covariate/column
  y <- demeaned_values[, j]
  Xj <- demeaned_values[, -j, drop = FALSE]
  r2 <- summary(lm(y ~ Xj))$r.squared #pull r2
  1 / (1 - r2) #calculate "vif"
})

names(leish_agg_vif_within) <- colnames(demeaned_values)
leish_agg_vif_within
summary(leish_agg_vif_within)
max(leish_agg_vif_within)

# 3) Residual diagnostics (within residuals)
leish_agg_df_diag <- leish_df_agg %>%
  mutate(resid  = resid(leish_yearly_agg_model),
         fitted = fitted(leish_yearly_agg_model))

# Residual vs fitted
sfig13g <- ggplot(leish_agg_df_diag, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Leish aggregated model (Eq. 2)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
sfig13g

# Residual distribution
sfig13h <- ggplot(leish_agg_df_diag, aes(x = resid)) +
  geom_histogram(bins = 50, color = "white") +
  labs(x = "Residuals",
       y = "Count",
       title = "") +
  theme_minimal()
sfig13h

#########################
### all plots together
#########################
dengue_diag_panel <-
  (sfig13a | sfig13c) /
  (sfig13b | sfig13d) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
dengue_diag_panel
ggsave("sfig13.pdf", plot=dengue_diag_panel, path="figures/", width = 8, height = 6, units="in", device = "pdf")

leish_diag_panel <-
  (sfig13e | sfig13g) /
  (sfig13f | sfig13h) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
leish_diag_panel
ggsave("sfig14.pdf", plot=leish_diag_panel, path="figures/", width = 8, height = 6, units="in", device = "pdf")

