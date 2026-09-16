# ============================================
# Script 4: Advanced Analysis & Robustness Checks
# Heterogeneous Effects + Threshold Analysis + Robustness
# ============================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

setwd("~/Documents/GitHub/wine-export-market-analysis")

# ============================================
# Load data
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
                                           "ESP", "SWE"), 1, 0)
  ) %>%
  filter(!is.na(italian_market_share_pct),
         !is.na(log_gdp_per_capita),
         !is.na(log_market_size),
         total_imports_usd > 10000000)

panel_data <- pdata.frame(regression_data, index = c("country_code", "year"))

# ============================================
# BASELINE MODEL (from Script 3)
# ============================================

baseline_fe <- plm(italian_market_share_pct ~ 
                     log_gdp_per_capita + 
                     log_market_size,
                   data = panel_data,
                   model = "within")

# ============================================
# APPROACH 1: HETEROGENEOUS EFFECTS
# Does the GDP effect differ by market size?
# H: Wealth matters MORE in small markets (less competition)
# ============================================

# Create interaction term
model_interaction <- plm(italian_market_share_pct ~ 
                           log_gdp_per_capita * log_market_size,
                         data = panel_data,
                         model = "within")

# Results with robust standard errors
interaction_results <- coeftest(model_interaction, vcov = vcovHC(model_interaction, type = "HC1"))

print("=== HETEROGENEOUS EFFECTS MODEL ===")
print(interaction_results)

# Calculate marginal effects at different market sizes
# Small market (25th percentile)
small_market_size <- quantile(regression_data$log_market_size, 0.25)
# Medium market (50th percentile)
medium_market_size <- quantile(regression_data$log_market_size, 0.50)
# Large market (75th percentile)
large_market_size <- quantile(regression_data$log_market_size, 0.75)

# Marginal effect of GDP = β_gdp + β_interaction * market_size
coef_gdp <- coef(model_interaction)["log_gdp_per_capita"]
coef_interaction <- coef(model_interaction)["log_gdp_per_capita:log_market_size"]

marginal_effects <- data.frame(
  Market_Size = c("Small (25th percentile)", "Medium (Median)", "Large (75th percentile)"),
  Log_Market_Size = c(small_market_size, medium_market_size, large_market_size),
  Market_Size_USD_Millions = round(exp(c(small_market_size, medium_market_size, large_market_size)) / 1e6, 0),
  Marginal_Effect_GDP = c(
    coef_gdp + coef_interaction * small_market_size,
    coef_gdp + coef_interaction * medium_market_size,
    coef_gdp + coef_interaction * large_market_size
  )
)

print("=== MARGINAL EFFECT OF GDP AT DIFFERENT MARKET SIZES ===")
print(marginal_effects)

# ============================================
# APPROACH 2: THRESHOLD ANALYSIS
# Is there a GDP threshold where Italian wine penetration jumps?
# ============================================

# Test multiple thresholds
thresholds_to_test <- c(5000, 8000, 10000, 20000, 30000)

threshold_results <- data.frame()

for (threshold in thresholds_to_test) {
  # Create high GDP dummy
  temp_data <- regression_data %>%
    mutate(
      high_gdp = ifelse(gdp_per_capita > threshold, 1, 0),
      high_gdp_interaction = high_gdp * log_gdp_per_capita
    )
  
  temp_panel <- pdata.frame(temp_data, index = c("country_code", "year"))
  
  # Run model with threshold
  model_temp <- plm(italian_market_share_pct ~ 
                      log_market_size + 
                      high_gdp_interaction + 
                      high_gdp,
                    data = temp_panel,
                    model = "within")
  
  # Extract coefficient for high_gdp_interaction
  coef_high <- coef(model_temp)["high_gdp_interaction"]
  se_high <- sqrt(diag(vcovHC(model_temp, type = "HC1")))["high_gdp_interaction"]
  
  threshold_results <- rbind(threshold_results, 
                             data.frame(
                               Threshold_USD = threshold,
                               Coefficient = coef_high,
                               Std_Error = se_high,
                               T_Stat = coef_high / se_high
                             ))
}

print("=== THRESHOLD ANALYSIS RESULTS ===")
print(threshold_results)

# Select best threshold (highest t-statistic)
best_threshold <- threshold_results$Threshold_USD[which.max(abs(threshold_results$T_Stat))]

# Run final threshold model with best threshold
regression_data_threshold <- regression_data %>%
  mutate(
    high_gdp = ifelse(gdp_per_capita > best_threshold, 1, 0),
    high_gdp_interaction = high_gdp * log_gdp_per_capita
  )

panel_data_threshold <- pdata.frame(regression_data_threshold, 
                                    index = c("country_code", "year"))

model_threshold <- plm(italian_market_share_pct ~ 
                         log_market_size + 
                         high_gdp_interaction + 
                         high_gdp,
                       data = panel_data_threshold,
                       model = "within")

threshold_final_results <- coeftest(model_threshold, vcov = vcovHC(model_threshold, type = "HC1"))

print(paste0("=== FINAL THRESHOLD MODEL (Threshold = $", best_threshold, ") ==="))
print(threshold_final_results)