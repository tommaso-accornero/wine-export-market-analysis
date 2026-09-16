# ============================================
# Script 3: Panel Regression Analysis
# Research Question: What determines Italian wine market share?
# ============================================

library(tidyverse)
library(plm)        # Panel regression
library(lmtest)     # Robust standard errors
library(sandwich)   # Robust covariance matrices
library(stargazer)  # Regression tables
library(countrycode)

setwd("~/Documents/GitHub/wine-export-market-analysis")

# ============================================
# Load and merge data
# ============================================

# Load market share data
market_share <- read_csv("data/market_share_clean.csv")

# Load economic data
gdp_data <- read_csv("data/gdp_per_capita.csv")
pop_data <- read_csv("data/population.csv")

# ============================================
# Create EU membership dummy variable
# EU members as of 2018-2024
# ============================================

eu_members <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", 
  "SVN", "ESP", "SWE"
)

# ============================================
# Prepare regression dataset
# Merge all data sources
# ============================================

regression_data <- market_share %>%
  # Join with GDP data
  left_join(
    gdp_data %>% select(country_code, year, gdp_per_capita),
    by = c("country_code", "year")
  ) %>%
  # Join with population data
  left_join(
    pop_data %>% select(country_code, year, population),
    by = c("country_code", "year")
  ) %>%
  # Create new variables
  mutate(
    # Log transformations (for interpretation as elasticities)
    log_gdp_per_capita = log(gdp_per_capita),
    log_market_size = log(total_imports_usd),
    log_population = log(population),
    
    # EU membership dummy
    eu_member = ifelse(country_code %in% eu_members, 1, 0),
    
    # Geographic region
    region = countrycode(country_code, origin = "iso3c", destination = "region"),
    
    # Create region dummies for key regions
    europe = ifelse(region == "Europe & Central Asia", 1, 0),
    asia = ifelse(region %in% c("East Asia & Pacific", "South Asia"), 1, 0),
    americas = ifelse(region %in% c("North America", "Latin America & Caribbean"), 1, 0)
  ) %>%
  # Remove rows with missing values
  filter(
    !is.na(italian_market_share_pct),
    !is.na(log_gdp_per_capita),
    !is.na(log_market_size),
    total_imports_usd > 10000000  # At least $10M market (filter out tiny markets)
  )

# Check sample size
nrow(regression_data)
length(unique(regression_data$country_code))

# Summary statistics
summary(regression_data %>% select(italian_market_share_pct, log_gdp_per_capita, 
                                   log_market_size, eu_member))

# ============================================
# MODEL 1: Pooled OLS (baseline)
# Ignores panel structure - treats all observations as independent
# ============================================

model_ols <- lm(italian_market_share_pct ~ 
                  log_gdp_per_capita + 
                  log_market_size + 
                  eu_member,
                data = regression_data)

# Results with robust standard errors (heteroskedasticity-robust)
coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))

# ============================================
# MODEL 2: Fixed Effects (Within Estimator)
# Controls for time-invariant country characteristics
# This is the main model for the paper
# ============================================

# Convert to panel data object
panel_data <- pdata.frame(regression_data, 
                          index = c("country_code", "year"))

# Fixed effects regression
# Country fixed effects control for all time-invariant factors
# (geography, culture, language, historical ties, etc.)
model_fe <- plm(italian_market_share_pct ~ 
                  log_gdp_per_capita + 
                  log_market_size,
                data = panel_data,
                model = "within",  # Country fixed effects
                effect = "individual")

summary(model_fe)

# Robust standard errors for fixed effects
coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC1"))

# ============================================
# MODEL 3: Random Effects (alternative specification)
# Assumes country effects are uncorrelated with regressors
# ============================================

model_re <- plm(italian_market_share_pct ~ 
                  log_gdp_per_capita + 
                  log_market_size + 
                  eu_member,
                data = panel_data,
                model = "random")

summary(model_re)

# ============================================
# MODEL 4: Pooled OLS with region dummies
# Alternative to fixed effects when FE drops time-invariant vars
# ============================================

model_regions <- lm(italian_market_share_pct ~ 
                      log_gdp_per_capita + 
                      log_market_size + 
                      eu_member + 
                      asia + 
                      americas,
                    data = regression_data)

coeftest(model_regions, vcov = vcovHC(model_regions, type = "HC1"))

# ============================================
# Hausman Test: FE vs. RE
# Tests whether fixed effects or random effects is more appropriate
# ============================================

phtest(model_fe, model_re)

# If p < 0.05: Use Fixed Effects (country-specific effects correlated with X)
# If p > 0.05: Random Effects is consistent (but FE is always safe)

# ============================================
# Model Diagnostics
# ============================================

# R-squared for fixed effects model
summary(model_fe)$r.squared

# Number of countries and observations
pdim(panel_data)

# Check for serial correlation
pbgtest(model_fe)

# ============================================
# Regression Table for Paper/Abstract
# Compare all models side-by-side
# ============================================

stargazer(model_ols, model_regions, model_fe, model_re,
          type = "text",
          title = "Determinants of Italian Wine Market Share",
          dep.var.labels = "Italian Market Share (%)",
          covariate.labels = c("Log(GDP per capita)", 
                               "Log(Market Size)", 
                               "EU Member",
                               "Asia",
                               "Americas"),
          column.labels = c("Pooled OLS", "OLS + Regions", 
                            "Fixed Effects", "Random Effects"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Robust standard errors in parentheses")

# ============================================
# Key Findings Summary
# ============================================

# Extract coefficients from main model (Fixed Effects)
fe_results <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC1"))

# Create results summary
results_summary <- data.frame(
  Variable = c("Log(GDP per capita)", "Log(Market Size)"),
  Coefficient = fe_results[, "Estimate"],
  Std_Error = fe_results[, "Std. Error"],
  P_Value = fe_results[, "Pr(>|t|)"],
  Significance = ifelse(fe_results[, "Pr(>|t|)"] < 0.01, "***",
                        ifelse(fe_results[, "Pr(>|t|)"] < 0.05, "**",
                               ifelse(fe_results[, "Pr(>|t|)"] < 0.1, "*", "")))
)

print(results_summary)

# ============================================
# Interpretation for Abstract
# ============================================

# Calculate marginal effects at means
mean_gdp <- mean(regression_data$log_gdp_per_capita, na.rm = TRUE)
mean_size <- mean(regression_data$log_market_size, na.rm = TRUE)

coef_gdp <- coef(model_fe)["log_gdp_per_capita"]
coef_size <- coef(model_fe)["log_market_size"]

# Interpretation examples
# Save for later use in abstract writing
interpretation <- list(
  gdp_effect = paste0(
    "A 10% increase in GDP per capita is associated with a ",
    round(coef_gdp * 0.1, 2), 
    " percentage point increase in Italian market share"
  ),
  size_effect = paste0(
    "A 10% increase in market size is associated with a ",
    round(coef_size * 0.1, 2),
    " percentage point change in Italian market share"
  ),
  r_squared = paste0(
    "The model explains ",
    round(summary(model_fe)$r.squared[1] * 100, 1),
    "% of within-country variation"
  ),
  n_countries = length(unique(regression_data$country_code)),
  n_obs = nrow(regression_data)
)

print(interpretation)

# ============================================
# Save regression results
# ============================================

# Save model object for later use
saveRDS(model_fe, "outputs/regression_model_fe.rds")

# Save results summary as CSV
write_csv(results_summary, "outputs/regression_results.csv")

# Save interpretation text
writeLines(unlist(interpretation), "outputs/regression_interpretation.txt")

