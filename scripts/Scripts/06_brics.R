# ============================================
# Script 6B: BRICS Partners vs Core Analysis
# Test if PARTNERS show different threshold effect than CORE
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
    
    # $8k threshold variables
    high_gdp_8k = ifelse(gdp_per_capita > 8000, 1, 0),
    high_gdp_interaction_8k = high_gdp_8k * log_gdp_per_capita,
    
    # BRICS Core (founding + 2010)
    brics_core = ifelse(country_code %in% c(
      "BRA", "RUS", "IND", "CHN", "ZAF"
    ), 1, 0),
    
    # BRICS Partners ONLY (exclude core)
    brics_partners = ifelse(country_code %in% c(
      "THA",  # Thailand
      "IDN",  # Indonesia
      "MYS",  # Malaysia
      "VNM"   # Vietnam
    ), 1, 0),
    
    # Neither BRICS nor Partner
    non_brics = ifelse(brics_core == 0 & brics_partners == 0, 1, 0)
  ) %>%
  filter(!is.na(italian_market_share_pct),
         !is.na(log_gdp_per_capita),
         !is.na(log_market_size),
         total_imports_usd > 10000000)

# Create panel data
panel_data <- pdata.frame(regression_data, index = c("country_code", "year"))

# ============================================
# Check which countries are in each group
# ============================================

print("=== BRICS CORE COUNTRIES ===")
brics_core_countries <- regression_data %>%
  filter(brics_core == 1) %>%
  distinct(country_code, country_name) %>%
  arrange(country_name)
print(brics_core_countries)

print("=== BRICS PARTNERS (NOT CORE) ===")
brics_partners_countries <- regression_data %>%
  filter(brics_partners == 1) %>%
  distinct(country_code, country_name) %>%
  arrange(country_name)
print(brics_partners_countries)

print("=== NON-BRICS (NOT CORE, NOT PARTNER) ===")
non_brics_sample <- regression_data %>%
  filter(non_brics == 1) %>%
  distinct(country_code, country_name) %>%
  arrange(country_name)
print(paste0("Total Non-BRICS countries: ", nrow(non_brics_sample)))
print(head(non_brics_sample, 10))

# ============================================
# SUBSAMPLE ANALYSIS: Three Groups
# ============================================

# 1. BRICS CORE subsample
regression_data_core <- regression_data %>% filter(brics_core == 1)
panel_data_core <- pdata.frame(regression_data_core, index = c("country_code", "year"))

# 2. BRICS PARTNERS subsample
regression_data_partners <- regression_data %>% filter(brics_partners == 1)
panel_data_partners <- pdata.frame(regression_data_partners, index = c("country_code", "year"))

# 3. NON-BRICS subsample
regression_data_non_brics <- regression_data %>% filter(non_brics == 1)
panel_data_non_brics <- pdata.frame(regression_data_non_brics, index = c("country_code", "year"))

# ============================================
# LINEAR MODELS: All Three Groups
# ============================================

print("=== LINEAR MODEL: BRICS CORE ===")
model_core_linear <- plm(italian_market_share_pct ~ 
                           log_gdp_per_capita + 
                           log_market_size,
                         data = panel_data_core,
                         model = "within")
print(paste0("N (BRICS Core): ", nrow(regression_data_core)))
print(coeftest(model_core_linear, vcov = vcovHC(model_core_linear, type = "HC1")))

print("=== LINEAR MODEL: BRICS PARTNERS ===")
model_partners_linear <- plm(italian_market_share_pct ~ 
                               log_gdp_per_capita + 
                               log_market_size,
                             data = panel_data_partners,
                             model = "within")
print(paste0("N (BRICS Partners): ", nrow(regression_data_partners)))
print(coeftest(model_partners_linear, vcov = vcovHC(model_partners_linear, type = "HC1")))

print("=== LINEAR MODEL: NON-BRICS ===")
model_non_brics_linear <- plm(italian_market_share_pct ~ 
                                log_gdp_per_capita + 
                                log_market_size,
                              data = panel_data_non_brics,
                              model = "within")
print(paste0("N (Non-BRICS): ", nrow(regression_data_non_brics)))
print(coeftest(model_non_brics_linear, vcov = vcovHC(model_non_brics_linear, type = "HC1")))

# ============================================
# THRESHOLD MODELS: All Three Groups
# ============================================

print("=== THRESHOLD MODEL: BRICS CORE ===")
model_core_threshold <- plm(italian_market_share_pct ~ 
                              log_market_size + 
                              high_gdp_interaction_8k +
                              high_gdp_8k,
                            data = panel_data_core,
                            model = "within")
print(coeftest(model_core_threshold, vcov = vcovHC(model_core_threshold, type = "HC1")))

print("=== THRESHOLD MODEL: BRICS PARTNERS ===")
model_partners_threshold <- plm(italian_market_share_pct ~ 
                                  log_market_size + 
                                  high_gdp_interaction_8k +
                                  high_gdp_8k,
                                data = panel_data_partners,
                                model = "within")
print(coeftest(model_partners_threshold, vcov = vcovHC(model_partners_threshold, type = "HC1")))

print("=== THRESHOLD MODEL: NON-BRICS ===")
model_non_brics_threshold <- plm(italian_market_share_pct ~ 
                                   log_market_size + 
                                   high_gdp_interaction_8k +
                                   high_gdp_8k,
                                 data = panel_data_non_brics,
                                 model = "within")
print(coeftest(model_non_brics_threshold, vcov = vcovHC(model_non_brics_threshold, type = "HC1")))

# ============================================
# COMPARISON TABLES
# ============================================

# TABLE 1: Linear Models - Three Groups
stargazer(model_core_linear, model_partners_linear, model_non_brics_linear,
          type = "text",
          title = "Linear Models: BRICS Core vs Partners vs Non-BRICS",
          dep.var.labels = "Italian Market Share (%)",
          column.labels = c("BRICS Core", "BRICS Partners", "Non-BRICS"),
          covariate.labels = c("Log(GDP per capita)", 
                               "Log(Market Size)"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Country fixed effects with robust standard errors")

# TABLE 2: Threshold Models - Three Groups
stargazer(model_core_threshold, model_partners_threshold, model_non_brics_threshold,
          type = "text",
          title = "Threshold Models: BRICS Core vs Partners vs Non-BRICS",
          dep.var.labels = "Italian Market Share (%)",
          column.labels = c("BRICS Core", "BRICS Partners", "Non-BRICS"),
          covariate.labels = c("Log(Market Size)",
                               "High GDP × Log(GDP)",
                               "High GDP (>$8k)"),
          omit.stat = c("f", "ser"),
          digits = 2,
          notes = "Country fixed effects with robust standard errors")

# ============================================
# KEY COMPARISON: Threshold Effect Across Groups
# ============================================

threshold_effect_comparison <- data.frame(
  Group = c("BRICS Core (5)", "BRICS Partners (4)", "Non-BRICS"),
  N = c(
    nrow(regression_data_core),
    nrow(regression_data_partners),
    nrow(regression_data_non_brics)
  ),
  Countries = c(
    paste(brics_core_countries$country_name, collapse = ", "),
    paste(brics_partners_countries$country_name, collapse = ", "),
    paste0(nrow(non_brics_sample), " countries")
  ),
  Threshold_Coef = c(
    coef(model_core_threshold)["high_gdp_interaction_8k"],
    coef(model_partners_threshold)["high_gdp_interaction_8k"],
    coef(model_non_brics_threshold)["high_gdp_interaction_8k"]
  ),
  Std_Error = c(
    sqrt(diag(vcovHC(model_core_threshold, type = "HC1")))["high_gdp_interaction_8k"],
    sqrt(diag(vcovHC(model_partners_threshold, type = "HC1")))["high_gdp_interaction_8k"],
    sqrt(diag(vcovHC(model_non_brics_threshold, type = "HC1")))["high_gdp_interaction_8k"]
  )
)

threshold_effect_comparison <- threshold_effect_comparison %>%
  mutate(
    T_Stat = Threshold_Coef / Std_Error,
    P_Value = 2 * pt(-abs(T_Stat), df = 500),
    Significant = ifelse(P_Value < 0.01, "***",
                         ifelse(P_Value < 0.05, "**",
                                ifelse(P_Value < 0.1, "*", "")))
  )

print("=== THRESHOLD EFFECT COMPARISON: CORE vs PARTNERS vs NON-BRICS ===")
print(threshold_effect_comparison %>% select(Group, N, Threshold_Coef, T_Stat, P_Value, Significant))

# ============================================
# DESCRIPTIVE: Partners by Threshold Status (2024)
# ============================================

partners_2024 <- regression_data %>%
  filter(brics_partners == 1, year == 2024) %>%
  select(country_name, gdp_per_capita, total_imports_usd, italian_market_share_pct) %>%
  mutate(
    gdp_category = case_when(
      gdp_per_capita < 6000 ~ "Below (<$6k)",
      gdp_per_capita >= 6000 & gdp_per_capita < 8000 ~ "Approaching ($6-8k)",
      gdp_per_capita >= 8000 & gdp_per_capita < 12000 ~ "Crossed ($8-12k)",
      gdp_per_capita >= 12000 ~ "Established (>$12k)"
    ),
    gdp_per_capita = round(gdp_per_capita, 0),
    market_size_M = round(total_imports_usd / 1e6, 0),
    italian_share = round(italian_market_share_pct, 1)
  ) %>%
  arrange(gdp_per_capita)

print("=== BRICS PARTNERS: THRESHOLD STATUS IN 2024 ===")
print(partners_2024)

# ============================================
# SAVE RESULTS
# ============================================

write_csv(threshold_effect_comparison, "outputs/brics_partners_threshold_comparison.csv")
write_csv(partners_2024, "outputs/brics_partners_2024.csv")

saveRDS(list(
  core_linear = model_core_linear,
  partners_linear = model_partners_linear,
  non_brics_linear = model_non_brics_linear,
  core_threshold = model_core_threshold,
  partners_threshold = model_partners_threshold,
  non_brics_threshold = model_non_brics_threshold
), "outputs/brics_partners_analysis.rds")

print("=== BRICS PARTNERS ANALYSIS COMPLETE ===")
print("")
print("KEY QUESTION: Is threshold effect significant for BRICS Partners?")
print("Look at the 'THRESHOLD EFFECT COMPARISON' table above!")
