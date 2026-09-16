# ============================================
# Script 5: Robustness Checks for $8k Threshold Model
# Complete script with all robustness checks
# ============================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

setwd("~/Documents/GitHub/wine-export-market-analysis")

# ============================================
# Load data and create $8k threshold variable
# ============================================

regression_data <- read_csv("data/market_share_clean.csv") %>%
  left_join(read_csv("data/gdp_per_capita.csv") %>% 
              select(country_code, year, gdp_per_capita),
            by = c("country_code", "year")) %>%
  left_join(read_csv("data/population.csv") %>% 
              select(country_code, year, population),
            by = c("country_code", "year")) %>%
  mutate(
    log_gdp_per_capita = log(gdp_per_capita),
    log_market_size = log(total_imports_usd),
    eu_member = ifelse(country_code %in% c("AUT", "BEL", "BGR", "HRV", "CYP", 
                                           "CZE", "DNK", "EST", "FIN", "FRA", 
                                           "DEU", "GRC", "HUN", "IRL", "ITA", 
                                           "LVA", "LTU", "LUX", "MLT", "NLD", 
                                           "POL", "PRT", "ROU", "SVK", "SVN", 
                                           "ESP", "SWE"), 1, 0),
    # Create $8k threshold dummy
    high_gdp_8k = ifelse(gdp_per_capita > 8000, 1, 0),
    high_gdp_interaction_8k = high_gdp_8k * log_gdp_per_capita
  ) %>%
  filter(!is.na(italian_market_share_pct),
         !is.na(log_gdp_per_capita),
         !is.na(log_market_size),
         total_imports_usd > 10000000)

panel_data <- pdata.frame(regression_data, index = c("country_code", "year"))

# ============================================
# BASELINE MODELS (for comparison)
# ============================================

# Model 1: Linear fixed effects (from Script 3)
baseline_fe <- plm(italian_market_share_pct ~ 
                     log_gdp_per_capita + 
                     log_market_size,
                   data = panel_data,
                   model = "within")

print("=== BASELINE LINEAR MODEL ===")
print(coeftest(baseline_fe, vcov = vcovHC(baseline_fe, type = "HC1")))

# Model 2: Threshold model with $8k (from Script 4)
threshold_8k <- plm(italian_market_share_pct ~ 
                      log_market_size + 
                      high_gdp_interaction_8k + 
                      high_gdp_8k,
                    data = panel_data,
                    model = "within")

print("=== THRESHOLD MODEL ($8k) ===")
print(coeftest(threshold_8k, vcov = vcovHC(threshold_8k, type = "HC1")))

# ============================================
# ROBUSTNESS CHECK 1: Remove Outliers
# Drop observations with market share > 60%
# ============================================

regression_data_clean <- regression_data %>%
  filter(italian_market_share_pct <= 60)

panel_data_clean <- pdata.frame(regression_data_clean, 
                                index = c("country_code", "year"))

# Linear model without outliers
baseline_no_outliers <- plm(italian_market_share_pct ~ 
                              log_gdp_per_capita + 
                              log_market_size,
                            data = panel_data_clean,
                            model = "within")

# Threshold model without outliers
threshold_no_outliers <- plm(italian_market_share_pct ~ 
                               log_market_size + 
                               high_gdp_interaction_8k + 
                               high_gdp_8k,
                             data = panel_data_clean,
                             model = "within")

print("=== ROBUSTNESS CHECK 1: REMOVE OUTLIERS ===")
print(paste0("Observations dropped: ", nrow(regression_data) - nrow(regression_data_clean)))
print("Baseline model (no outliers):")
print(coeftest(baseline_no_outliers, vcov = vcovHC(baseline_no_outliers, type = "HC1")))
print("Threshold model (no outliers):")
print(coeftest(threshold_no_outliers, vcov = vcovHC(threshold_no_outliers, type = "HC1")))

# ============================================
# ROBUSTNESS CHECK 2: Log-Log Specification
# Transform dependent variable to log
# ============================================

regression_data_loglog <- regression_data %>%
  mutate(log_italian_share = log(italian_market_share_pct + 0.1))

panel_data_loglog <- pdata.frame(regression_data_loglog, 
                                 index = c("country_code", "year"))

# Linear log-log
baseline_loglog <- plm(log_italian_share ~ 
                         log_gdp_per_capita + 
                         log_market_size,
                       data = panel_data_loglog,
                       model = "within")

# Threshold log-log
threshold_loglog <- plm(log_italian_share ~ 
                          log_market_size + 
                          high_gdp_interaction_8k + 
                          high_gdp_8k,
                        data = panel_data_loglog,
                        model = "within")

print("=== ROBUSTNESS CHECK 2: LOG-LOG SPECIFICATION ===")
print("Baseline log-log:")
print(coeftest(baseline_loglog, vcov = vcovHC(baseline_loglog, type = "HC1")))
print("Threshold log-log:")
print(coeftest(threshold_loglog, vcov = vcovHC(threshold_loglog, type = "HC1")))

# ============================================
# ROBUSTNESS CHECK 3: EU vs Non-EU Subsamples
# Test if threshold holds in different regions
# ============================================

# EU subsample
regression_data_eu <- regression_data %>% filter(eu_member == 1)
panel_data_eu <- pdata.frame(regression_data_eu, index = c("country_code", "year"))

baseline_eu <- plm(italian_market_share_pct ~ 
                     log_gdp_per_capita + 
                     log_market_size,
                   data = panel_data_eu,
                   model = "within")

threshold_eu <- plm(italian_market_share_pct ~ 
                      log_market_size + 
                      high_gdp_interaction_8k + 
                      high_gdp_8k,
                    data = panel_data_eu,
                    model = "within")

# Non-EU subsample
regression_data_non_eu <- regression_data %>% filter(eu_member == 0)
panel_data_non_eu <- pdata.frame(regression_data_non_eu, 
                                 index = c("country_code", "year"))

baseline_non_eu <- plm(italian_market_share_pct ~ 
                         log_gdp_per_capita + 
                         log_market_size,
                       data = panel_data_non_eu,
                       model = "within")

threshold_non_eu <- plm(italian_market_share_pct ~ 
                          log_market_size + 
                          high_gdp_interaction_8k + 
                          high_gdp_8k,
                        data = panel_data_non_eu,
                        model = "within")

print("=== ROBUSTNESS CHECK 3: EU SUBSAMPLE ===")
print(paste0("N (EU): ", nrow(regression_data_eu)))
print("Baseline EU:")
print(coeftest(baseline_eu, vcov = vcovHC(baseline_eu, type = "HC1")))
print("Threshold EU:")
print(coeftest(threshold_eu, vcov = vcovHC(threshold_eu, type = "HC1")))

print("=== ROBUSTNESS CHECK 3: NON-EU SUBSAMPLE ===")
print(paste0("N (Non-EU): ", nrow(regression_data_non_eu)))
print("Baseline Non-EU:")
print(coeftest(baseline_non_eu, vcov = vcovHC(baseline_non_eu, type = "HC1")))
print("Threshold Non-EU:")
print(coeftest(threshold_non_eu, vcov = vcovHC(threshold_non_eu, type = "HC1")))

# ============================================
# ROBUSTNESS CHECK 4: Alternative Thresholds
# Test $6k and $10k to show $8k is robust
# ============================================

# $6k threshold
regression_data_6k <- regression_data %>%
  mutate(
    high_gdp_6k = ifelse(gdp_per_capita > 6000, 1, 0),
    high_gdp_interaction_6k = high_gdp_6k * log_gdp_per_capita
  )

panel_data_6k <- pdata.frame(regression_data_6k, index = c("country_code", "year"))

threshold_6k <- plm(italian_market_share_pct ~ 
                      log_market_size + 
                      high_gdp_interaction_6k + 
                      high_gdp_6k,
                    data = panel_data_6k,
                    model = "within")

# $10k threshold
regression_data_10k <- regression_data %>%
  mutate(
    high_gdp_10k = ifelse(gdp_per_capita > 10000, 1, 0),
    high_gdp_interaction_10k = high_gdp_10k * log_gdp_per_capita
  )

panel_data_10k <- pdata.frame(regression_data_10k, index = c("country_code", "year"))

threshold_10k <- plm(italian_market_share_pct ~ 
                       log_market_size + 
                       high_gdp_interaction_10k + 
                       high_gdp_10k,
                     data = panel_data_10k,
                     model = "within")

print("=== ROBUSTNESS CHECK 4: ALTERNATIVE THRESHOLDS ===")
print("$6k threshold:")
print(coeftest(threshold_6k, vcov = vcovHC(threshold_6k, type = "HC1")))
print("$8k threshold (main):")
print(coeftest(threshold_8k, vcov = vcovHC(threshold_8k, type = "HC1")))
print("$10k threshold:")
print(coeftest(threshold_10k, vcov = vcovHC(threshold_10k, type = "HC1")))

# ============================================
# COMPARISON TABLES
# ============================================

# TABLE 1: Main models (baseline vs threshold)
stargazer(baseline_fe, threshold_8k, 
          baseline_no_outliers, threshold_no_outliers,
          type = "text",
          title = "Robustness Checks: Main Results",
          dep.var.labels = "Italian Market Share (%)",
          column.labels = c("Linear FE", "Threshold $8k", 
                            "Linear (No Outliers)", "Threshold (No Outliers)"),
          covariate.labels = c("Log(GDP per capita)", 
                               "Log(Market Size)",
                               "High GDP × Log(GDP)",
                               "High GDP (>$8k)"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Country fixed effects with robust standard errors")

# TABLE 2: Log-log specification
stargazer(baseline_loglog, threshold_loglog,
          type = "text",
          title = "Robustness: Log-Log Specification",
          dep.var.labels = "Log(Italian Market Share)",
          column.labels = c("Linear Log-Log", "Threshold Log-Log"),
          covariate.labels = c("Log(GDP per capita)", 
                               "Log(Market Size)",
                               "High GDP × Log(GDP)",
                               "High GDP (>$8k)"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Country fixed effects with robust standard errors. DV in logs.")

# TABLE 3: EU vs Non-EU
stargazer(baseline_eu, threshold_eu, baseline_non_eu, threshold_non_eu,
          type = "text",
          title = "Robustness: EU vs. Non-EU Markets",
          dep.var.labels = "Italian Market Share (%)",
          column.labels = c("EU Linear", "EU Threshold", 
                            "Non-EU Linear", "Non-EU Threshold"),
          covariate.labels = c("Log(GDP per capita)", 
                               "Log(Market Size)",
                               "High GDP × Log(GDP)",
                               "High GDP (>$8k)"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Country fixed effects with robust standard errors")

# ============================================
# CREATE THRESHOLD SENSITIVITY TABLE
# ============================================

threshold_comparison <- data.frame(
  Threshold = c("$6,000", "$8,000", "$10,000"),
  Interaction_Coef = c(
    coef(threshold_6k)["high_gdp_interaction_6k"],
    coef(threshold_8k)["high_gdp_interaction_8k"],
    coef(threshold_10k)["high_gdp_interaction_10k"]
  ),
  Std_Error = c(
    sqrt(diag(vcovHC(threshold_6k, type = "HC1")))["high_gdp_interaction_6k"],
    sqrt(diag(vcovHC(threshold_8k, type = "HC1")))["high_gdp_interaction_8k"],
    sqrt(diag(vcovHC(threshold_10k, type = "HC1")))["high_gdp_interaction_10k"]
  )
)

threshold_comparison <- threshold_comparison %>%
  mutate(
    T_Stat = Interaction_Coef / Std_Error,
    P_Value = 2 * pt(-abs(T_Stat), df = 500),
    Significant = ifelse(P_Value < 0.01, "***",
                         ifelse(P_Value < 0.05, "**",
                                ifelse(P_Value < 0.1, "*", "")))
  )

print("=== THRESHOLD SENSITIVITY ANALYSIS ===")
print(threshold_comparison)

# ============================================
# SAVE RESULTS
# ============================================

saveRDS(list(
  baseline = baseline_fe,
  threshold_8k = threshold_8k,
  no_outliers = threshold_no_outliers,
  loglog = threshold_loglog,
  eu = threshold_eu,
  non_eu = threshold_non_eu
), "outputs/robustness_models.rds")

write_csv(threshold_comparison, "outputs/threshold_sensitivity.csv")

print("=== ALL ROBUSTNESS CHECKS COMPLETE ===")
print("Results saved to outputs/ folder")