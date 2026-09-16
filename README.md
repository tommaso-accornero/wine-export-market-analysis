# Italian Wine Export Market Analysis

## Overview

This project develops a data-driven framework for identifying high-potential export markets for small Italian wine producers. Drawing on experience from a family winery in Piedmont, the analysis addresses a common problem for small producers: limited resources for comprehensive market research, leading to reliance on traditional networks and intuition rather than systematic evidence.

Using publicly available trade and economic data, this project tests whether market characteristics, particularly GDP per capita and market size, predict Italian wine market share across countries, and identifies specific income thresholds and regional patterns that determine market receptivity.

## Key Finding

Italian wine market share exhibits a critical threshold at approximately $8,000 GDP per capita. Below this level, income growth has minimal effect on market share. Above it, the effect on market share more than doubles. The threshold effect is also substantially stronger in specific emerging market regions than in others with comparable income levels, suggesting that market openness and the presence or absence of domestic wine production also shape outcomes.

## Data Sources

- UN Comtrade: Bilateral wine trade flows (HS Code 2204), 2018 to 2024
- World Bank: GDP per capita and population data, 2018 to 2024

## Methodology

- Panel dataset covering 103 wine importing countries over seven years (615 country year observations after cleaning)
- Dependent variable: Italian wine exports as a percentage of each country's total wine imports
- Fixed effects panel regression to control for time invariant country characteristics
- Threshold detection using interaction terms, tested across multiple candidate cutoffs
- Robustness checks: outlier removal, log log specification, alternative thresholds, and regional subsample analysis
- Hausman test used to confirm fixed effects over random effects specification
- Robust standard errors applied throughout to account for heteroskedasticity and serial correlation

## Repository Structure

data/
    Raw and cleaned datasets

outputs/
    Regression results, tables, and saved model objects

scripts_01_collect_worldbank_data.R
    Downloads GDP and population data via World Bank API

02_clean_comtrade_data.R
    Cleans raw Comtrade trade flows, constructs market share variable

03_regression.R
    Baseline panel regression: pooled OLS, fixed effects, random effects, Hausman test

04_advanced_analysis_and_robust.R
    Threshold detection across candidate GDP cutoffs

05_robust.R
    Robustness checks: outliers, log log specification, subsamples, alternative thresholds

06_brics.R
    Regional and geopolitical subsample analysis

## Tools and Packages

- R: primary language for data cleaning, modeling, and visualization
- tidyverse: data wrangling
- plm: panel data regression
- lmtest and sandwich: robust standard errors
- stargazer: regression output tables
- wbstats: World Bank API access
- countrycode: country code standardization

## Reproducing the Analysis

Scripts are numbered in the order they should be run:

1. scripts_01_collect_worldbank_data.R retrieves GDP and population data
2. 02_clean_comtrade_data.R cleans trade data and builds the core dataset
3. 03_regression.R runs the baseline fixed effects model
4. 04_advanced_analysis_and_robust.R identifies the GDP threshold
5. 05_robust.R runs all robustness checks
6. 06_brics.R runs the regional subsample analysis

Each script assumes a working directory containing a data/ folder with the relevant input files, and writes outputs to an outputs/ folder.

## Summary of Results

- GDP per capita has a strong, statistically significant positive relationship with Italian wine market share
- The relationship is non-linear, with a clear threshold near $8,000 GDP per capita
- Market size has a negative effect on Italian market share, suggesting larger markets attract more intense competition
- The threshold effect holds under outlier removal, alternative functional forms, and subsample splits
- Regional analysis suggests the strength of the threshold effect depends on market openness and the presence of established domestic wine production

## Limitations

The regional subsample analysis is based on a small number of countries per group, which limits statistical power and should be interpreted as suggestive rather than conclusive. Results are based on aggregate trade data and do not account for wine specific factors such as quality classification, varietal preferences, or distribution channels, which are natural extensions for future work.

## Author

Tommaso Accornero
MSc Computational Social Science, Universidad Carlos III de Madrid