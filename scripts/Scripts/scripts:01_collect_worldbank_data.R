# Install packages (only need to do this once)
library("tidyverse")    # data manipulation
library("wbstats")      # World Bank data
library("countrycode")  # country name standardization
library("RSQLite")      # connect R to your database
library("DBI")          # database interface
library("httr")         # for downloading files
library("jsonlite")     # for API data

setwd("~/Documents/GitHub/wine-export-market-analysis")

# ============================================
# Download GDP per capita data
# ============================================

gdp_data <- wb_data(
  indicator = "NY.GDP.PCAP.CD",  # GDP per capita (current US$)
  start_date = 2018,
  end_date = 2024,  # Updated to 2024
  return_wide = FALSE
)

# Clean the data
gdp_clean <- gdp_data %>%
  select(
    country_name = country,
    country_code = iso3c,
    year = date,
    gdp_per_capita = value
  ) %>%
  filter(!is.na(gdp_per_capita))

# Save
write_csv(gdp_clean, "data/gdp_per_capita.csv")

# ============================================
# Download Population data
# ============================================

pop_data <- wb_data(
  indicator = "SP.POP.TOTL",  # Total population
  start_date = 2018,
  end_date = 2024,  # Updated to 2024
  return_wide = FALSE
)

# Clean
pop_clean <- pop_data %>%
  select(
    country_name = country,
    country_code = iso3c,
    year = date,
    population = value
  ) %>%
  filter(!is.na(population))

# Save
write_csv(pop_clean, "data/population.csv")

# ============================================
# Combine into Countries table (using most recent year: 2024)
# ============================================

countries_data <- gdp_clean %>%
  left_join(pop_clean, by = c("country_code", "year", "country_name")) %>%
  # Add region information
  mutate(
    region = countrycode(country_code, origin = "iso3c", destination = "region")
  ) %>%
  # Focus on most recent year for the countries table
  filter(year == 2024) %>%
  arrange(desc(gdp_per_capita))

# Preview
head(countries_data, 10)

# Save
write_csv(countries_data, "data/countries_raw.csv")