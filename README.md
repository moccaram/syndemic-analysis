# Syndemic Burden Among Bangladeshi Women — BDHS 2022

📊 **Live report → https://moccaram.github.io/syndemic-analysis/**

A manuscript-style, fully reproducible Quarto report analyzing co-occurring nutritional,
reproductive-health, and healthcare-access/autonomy risks among women aged 15–49 in
Bangladesh using the nationally representative Bangladesh Demographic and Health Survey (BDHS) 2022.

This project is a working example of an end-to-end **statistical analysis of a complex-survey
exposure study** — from study-population definition and covariate handling through
survey-weighted modeling, effect estimation, and transparent sensitivity analysis.

## Skills Demonstrated

Relevant to exposure/outcome and epidemiological statistical-analysis work:

- **Complex-survey estimation** — analyses incorporate sampling weights, primary sampling
  units, and strata throughout (R `survey`), so prevalence and effect estimates are
  nationally representative rather than naïve.
- **Exposure → outcome modeling** — survey-weighted **quasi-Poisson** regression for a count
  outcome (incidence rate ratios) as the primary model, with an **ordinal proportional-odds**
  model as a sensitivity check; predictors adjusted for demographic, socioeconomic,
  geographic, and empowerment covariates.
- **Effect estimates with uncertainty** — all associations reported as IRRs/ORs with 95%
  confidence intervals and formatted p-values.
- **Clustering / association testing** — observed-vs-expected co-occurrence analysis and
  interaction tests for effect modification.
- **Sensitivity & diagnostics** — available-indicator vs complete-case scoring, model
  dispersion/fit diagnostics, missingness summaries, and cutoff sensitivity.
- **Reproducible reporting** — parameterized R + Quarto pipeline, separated derivation
  script and presentation layer, published as a multi-page website.

## Key Findings

- 11.4% of women carry high syndemic burden (score ≥ 4); 64.6% carry medium burden (1–3).
- All three risk domains co-occur more often than expected under independence (O/E ratio 1.23).
- Strong, consistent social gradients: no education vs higher (IRR 2.06), poorest vs richest
  wealth (IRR 1.39), and rural vs urban residence (IRR 1.11) all predict elevated burden.
- Findings are robust across the count and ordinal sensitivity models.

## Methods

- **Data:** BDHS 2022 (n = 11,657 women with recent births; n = 7,626 for multivariable models)
- **Outcome:** cumulative syndemic score from 9 binary risk indicators across 3 domains;
  categorized Low (0) / Medium (1–3) / High (≥4)
- **Analysis:** survey-weighted descriptives, observed/expected clustering checks,
  quasi-Poisson count regression (primary), ordinal logistic regression (sensitivity)
- **Software:** R (`survey`, `dplyr`, `ggplot2`, `knitr`) + Quarto

## Repository Structure

- `index.qmd` — Overview (Abstract, Study Snapshot, Introduction, Conceptual Framework)
- `methods.qmd`, `results.qmd`, `discussion.qmd`, `appendix.qmd` — report sections
- `_common.R` — shared libraries and helper functions sourced by each page
- `_quarto.yml`, `_website.yml`, `custom.scss` — site configuration and theme
- `data/` — aggregate CSV tables used by the public report
- `figures/` — publication figures
- `scripts/derive_paper_outputs.R` — reproducible aggregate-output script (requires local restricted analysis data)
- `references.bib` — report bibliography

## Author

Mukarram Hosain · [github.com/moccaram](https://github.com/moccaram)

## License

Released under the [MIT License](LICENSE).
