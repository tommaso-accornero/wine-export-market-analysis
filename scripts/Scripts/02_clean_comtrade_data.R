# ============================================
# Script 2: Clean UN Comtrade Wine Data
# Using 2018-2024 (7 years of complete data)
# ============================================

library(tidyverse)

setwd("~/Documents/GitHub/wine-export-market-analysis")

# ============================================
# Load the raw data
# ============================================

total_imports_raw <- read_csv("data/comtrade_total_wine_imports.csv")
italian_exports_raw <- read_csv("data/comtrade_italian_exports.csv")

# ============================================
# Clean Total Wine Imports
# Keep: country name, country code, year, import value in USD
# ============================================

total_imports_clean <- total_imports_raw %>%
  # Select only the columns we need
  select(
    country_name = reporterDesc,
    country_code = reporterISO,
    year = refYear,
    import_value_usd = primaryValue
  ) %>%
  # Remove any rows with missing or zero values
  filter(!is.na(import_value_usd), import_value_usd > 0) %>%
  # Keep only years 2018-2024 (2025 data incomplete)
  filter(year >= 2018, year <= 2024) %>%
  # Sort by country and year
  arrange(country_name, year)

# Check the result - should have ~1100 rows
nrow(total_imports_clean)

# Save cleaned data
write_csv(total_imports_clean, "data/total_wine_imports_clean.csv")

# ============================================
# Clean Italian Wine Exports
# Partner = destination country where Italy exports wine
# ============================================

italian_exports_clean <- italian_exports_raw %>%
  # Select columns - partnerDesc is the destination country
  select(
    country_name = partnerDesc,
    country_code = partnerISO,
    year = refYear,
    export_value_usd = primaryValue
  ) %>%
  # Remove missing or zero values
  filter(!is.na(export_value_usd), export_value_usd > 0) %>%
  # Remove "World" aggregate row (we only want individual countries)
  filter(country_name != "World") %>%
  # Keep only years 2018-2024
  filter(year >= 2018, year <= 2024) %>%
  # Sort
  arrange(country_name, year)

# Check the result - should have ~1200 rows
nrow(italian_exports_clean)

# Save
write_csv(italian_exports_clean, "data/italian_exports_clean.csv")

# ============================================
# Calculate Italian Market Share
# Market share = (Italian exports to country) / (Total wine imports by country) * 100
# ============================================

market_share <- total_imports_clean %>%
  # Join Italian exports to total imports by country code and year
  full_join(
    italian_exports_clean,
    by = c("country_code", "year"),
    suffix = c("_total", "_italian")
  ) %>%
  # Use the best country name available (prefer from total imports)
  mutate(
    country_name = coalesce(country_name_total, country_name_italian)
  ) %>%
  # If a country has no Italian exports, set to 0 (not NA)
  mutate(
    export_value_usd = replace_na(export_value_usd, 0)
  ) %>%
  # Calculate Italian market share as percentage
  mutate(
    italian_market_share_pct = (export_value_usd / import_value_usd) * 100,
    italian_market_share_pct = replace_na(italian_market_share_pct, 0)
  ) %>%
  # Select final columns in clean order
  select(
    country_name,
    country_code,
    year,
    total_imports_usd = import_value_usd,
    italian_exports_usd = export_value_usd,
    italian_market_share_pct
  ) %>%
  # Keep only rows where we have total import data
  filter(!is.na(total_imports_usd))

# Check the result - should have many rows
nrow(market_share)

# Save
write_csv(market_share, "data/market_share_clean.csv")

# ============================================
# Calculate Growth Rates (2018 to 2024)
# This shows which markets are growing vs. stagnant
# ============================================

# Get 2018 baseline
baseline_2018 <- market_share %>%
  filter(year == 2018) %>%
  select(country_code, baseline_imports = total_imports_usd, baseline_italian_share = italian_market_share_pct)

# Get 2024 most recent
recent_2024 <- market_share %>%
  filter(year == 2024) %>%
  select(country_code, recent_imports = total_imports_usd, recent_italian_share = italian_market_share_pct)

# Calculate compound annual growth rate (CAGR) over 6 years
growth_analysis <- baseline_2018 %>%
  inner_join(recent_2024, by = "country_code") %>%
  mutate(
    # Total change in imports from 2018 to 2024
    total_growth_pct = ((recent_imports - baseline_imports) / baseline_imports) * 100,
    # Compound annual growth rate over 6 years
    cagr = ((recent_imports / baseline_imports)^(1/6) - 1) * 100,
    # Change in Italian market share (percentage points)
    italian_share_change = recent_italian_share - baseline_italian_share
  )

# ============================================
# Summary Statistics - Key Tables
# ============================================

# TABLE 1: Top 20 wine importing countries by value (2024)
top_importers_2024 <- total_imports_clean %>%
  filter(year == 2024) %>%
  arrange(desc(import_value_usd)) %>%
  mutate(
    Rank = row_number(),
    import_value_millions = round(import_value_usd / 1000000, 0)
  ) %>%
  select(
    Rank,
    Country = country_name,
    Value_M_USD = import_value_millions
  ) %>%
  head(20)

print(top_importers_2024)

# TABLE 2: Top 20 Italian wine export destinations (2024)
top_italian_destinations_2024 <- italian_exports_clean %>%
  filter(year == 2024) %>%
  arrange(desc(export_value_usd)) %>%
  mutate(
    Rank = row_number(),
    export_value_millions = round(export_value_usd / 1000000, 0)
  ) %>%
  select(
    Rank,
    Country = country_name,
    Value_M_USD = export_value_millions
  ) %>%
  head(20)

print(top_italian_destinations_2024)

# TABLE 3: OPPORTUNITY MARKETS (2024)
# Large markets with LOW Italian presence = underserved opportunities
opportunity_markets_2024 <- market_share %>%
  filter(year == 2024) %>%
  # Only look at markets worth at least $100M
  filter(total_imports_usd > 100000000) %>%
  # Sort by lowest Italian market share (least competition)
  arrange(italian_market_share_pct) %>%
  mutate(
    market_size_M = round(total_imports_usd / 1000000, 0),
    italian_share_pct = round(italian_market_share_pct, 1),
    italian_exports_M = round(italian_exports_usd / 1000000, 0)
  ) %>%
  select(
    Country = country_name,
    Market_size_M_USD = market_size_M,
    Italian_share_pct = italian_share_pct,
    Italian_exports_M_USD = italian_exports_M
  ) %>%
  head(25)

print(opportunity_markets_2024)

# TABLE 4: SATURATED MARKETS (2024)
# Markets where Italy already has high market share = intense competition
saturated_markets_2024 <- market_share %>%
  filter(year == 2024) %>%
  filter(total_imports_usd > 100000000) %>%
  arrange(desc(italian_market_share_pct)) %>%
  mutate(
    market_size_M = round(total_imports_usd / 1000000, 0),
    italian_share_pct = round(italian_market_share_pct, 1)
  ) %>%
  select(
    Country = country_name,
    Market_size_M_USD = market_size_M,
    Italian_share_pct = italian_share_pct
  ) %>%
  head(20)

print(saturated_markets_2024)

# TABLE 5: FASTEST GROWING MARKETS (2018-2024)
# Markets with high growth AND low Italian share = best opportunities
fastest_growing_opportunities <- growth_analysis %>%
  # Join with 2024 data for current market size and Italian share
  inner_join(
    market_share %>% filter(year == 2024),
    by = "country_code"
  ) %>%
  # Only markets worth at least $50M
  filter(baseline_imports > 50000000) %>%
  # Sort by highest growth rate
  arrange(desc(cagr)) %>%
  mutate(
    market_size_M = round(total_imports_usd / 1000000, 0),
    italian_share_pct = round(italian_market_share_pct, 1),
    cagr_pct = round(cagr, 1)
  ) %>%
  select(
    Country = country_name,
    Market_size_M_USD = market_size_M,
    Growth_CAGR_pct = cagr_pct,
    Italian_share_pct = italian_share_pct
  ) %>%
  head(25)

print(fastest_growing_opportunities)

# TABLE 6: HIDDEN GEMS
# Fast growing markets (>5% CAGR) + Low Italian share (<15%) + Decent size (>$100M)
hidden_gems <- growth_analysis %>%
  inner_join(
    market_share %>% filter(year == 2024),
    by = "country_code"
  ) %>%
  filter(
    cagr > 5,  # Growing faster than 5% per year
    italian_market_share_pct < 15,  # Low Italian competition
    total_imports_usd > 100000000  # At least $100M market
  ) %>%
  arrange(desc(cagr)) %>%
  mutate(
    market_size_M = round(total_imports_usd / 1000000, 0),
    italian_share_pct = round(italian_market_share_pct, 1),
    cagr_pct = round(cagr, 1)
  ) %>%
  select(
    Country = country_name,
    Market_size_M_USD = market_size_M,
    Growth_CAGR_pct = cagr_pct,
    Italian_share_pct = italian_share_pct
  )

print(hidden_gems)

write_csv(top_importers_2024, "outputs/top_importers_2024.csv")
write_csv(opportunity_markets_2024, "outputs/opportunity_markets_2024.csv")
write_csv(hidden_gems, "outputs/hidden_gems.csv")
write_csv(opportunity_markets, "outputs/opportunity_markets.csv")
write_csv(opportunity_markets_2024, "outputs/opportunity_markets_2024.csv")
write_csv(fastest_growing_opportunities, "outputs/fastest_growing_opportunities.csv")
