# Syndemic Burden Among Bangladeshi Women - BDHS 2022

A manuscript-style Quarto report analyzing co-occurring nutritional, reproductive-health, and healthcare-access/autonomy risks among women aged 15-49 in Bangladesh using BDHS 2022.

## Key Findings

- 11.4% of women carry high syndemic burden, defined as score >=4
- All three risk domains co-occur more often than expected under independence
- Higher education is strongly protective in the full ordinal model
- Rural residence is associated with higher odds of elevated burden
- Wealth, employment, and husband's education show independent protective associations

## Methods

- Data: BDHS 2022 (N = 11,657 women in analytic data)
- Syndemic score: 9 binary risk indicators across 3 domains
- Primary outcome: Low (0), Medium (1-3), High (>=4)
- Analysis: survey-weighted descriptives, observed/expected clustering checks, ordinal logistic regression, count-model sensitivity analysis
- Software: R (survey, dplyr, ggplot2, Quarto)

## Live Report

[moccaram.github.io/syndemic-bdhs2022](https://moccaram.github.io/syndemic-bdhs2022) https://moccaram.github.io/syndemic-analysis/ 

## Repository Structure

- `index.qmd` - Main Quarto report
- `references.bib` - Report bibliography
- `data/` - Aggregate CSV tables used by the public report
- `figures/` - Publication figures
- `scripts/derive_paper_outputs.R` - Reproducible aggregate-output script; requires local restricted analysis data

## Author

Mukarram Hosain | github.com/moccaram
