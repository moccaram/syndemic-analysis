# Derive aggregate outputs for the manuscript-style Quarto report.
# The individual-level BDHS-derived analysis file is intentionally not copied
# into this public report repo. Set SYNDEMIC_ANALYSIS_DATA to override input.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(survey)
  library(tidyr)
})

options(survey.lonely.psu = "adjust")

input_path <- Sys.getenv(
  "SYNDEMIC_ANALYSIS_DATA",
  "../01_ACTIVE_PROJECT_BDHS2022/Syndemic_Model_BDHS2022/output/data_files/bdhs_2022_syndemic_analysis_data.csv"
)

if (!file.exists(input_path)) {
  stop("Analysis dataset not found. Set SYNDEMIC_ANALYSIS_DATA to the cleaned BDHS analysis CSV.")
}

dir.create("data", showWarnings = FALSE, recursive = TRUE)

risk_vars <- c(
  "underweight", "stunted_height",
  "high_parity", "adolescent_birth", "short_interval", "home_delivery",
  "distance_problem", "low_autonomy", "inadequate_anc"
)

analysis_data <- read_csv(input_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    age_group = str_replace_all(as.character(age_group), "–", "-"),
    syndemic_tertile = str_replace_all(as.character(syndemic_tertile), "–", "-"),
    syndemic_tertile = str_replace_all(syndemic_tertile, "≥", ">=")
  )

# Preserve the original en dash levels when present.
analysis_data <- analysis_data %>%
  mutate(
    age_group = factor(age_group, levels = c("15-24", "25-34", "35-49")),
    education = factor(education, levels = c("No education", "Primary", "Secondary", "Higher")),
    wealth = factor(wealth, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    division = factor(
      division,
      levels = c("Barishal", "Chattogram", "Dhaka", "Khulna", "Mymensingh", "Rajshahi", "Rangpur", "Sylhet")
    ),
    employment = factor(employment, levels = c("Not working", "Working")),
    husband_education = factor(husband_education, levels = c("No education", "Primary", "Secondary", "Higher")),
    syndemic_tertile = ordered(
      syndemic_tertile,
      levels = c("Low (0)", "Medium (1-3)", "High (>=4)")
    ),
    nutrition_any = if_else(!is.na(syndemic_nutrition), as.integer(syndemic_nutrition > 0), NA_integer_),
    reproductive_any = if_else(!is.na(syndemic_reprod), as.integer(syndemic_reprod > 0), NA_integer_),
    access_any = if_else(!is.na(syndemic_access), as.integer(syndemic_access > 0), NA_integer_),
    high_burden_ge4 = if_else(!is.na(syndemic_score), as.integer(syndemic_score >= 4), NA_integer_),
    very_high_burden_ge6 = if_else(!is.na(syndemic_score), as.integer(syndemic_score >= 6), NA_integer_),
    n_observed_indicators = rowSums(!is.na(across(all_of(risk_vars)))),
    complete_case_score = if_else(
      n_observed_indicators == length(risk_vars),
      rowSums(across(all_of(risk_vars)), na.rm = TRUE),
      NA_real_
    ),
    complete_case_tertile = ordered(
      case_when(
        is.na(complete_case_score) ~ NA_character_,
        complete_case_score == 0 ~ "Low (0)",
        complete_case_score %in% 1:3 ~ "Medium (1-3)",
        complete_case_score >= 4 ~ "High (>=4)"
      ),
      levels = c("Low (0)", "Medium (1-3)", "High (>=4)")
    )
  )

design_all <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analysis_data,
  nest = TRUE
)

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

weighted_mean <- function(design, var) {
  est <- svymean(as.formula(paste0("~", var)), design, na.rm = TRUE)
  ci <- confint(est)
  tibble(
    estimate = as.numeric(coef(est))[1],
    se = as.numeric(SE(est))[1],
    ci_lower = as.numeric(ci[1, 1]),
    ci_upper = as.numeric(ci[1, 2])
  )
}

weighted_distribution <- function(design, var, label) {
  tab <- svytable(as.formula(paste0("~", var)), design)
  as_tibble(as.data.frame(tab)) %>%
    rename(category = 1, weighted_n = Freq) %>%
    mutate(
      analysis = label,
      weighted_percent = weighted_n / sum(weighted_n) * 100
    ) %>%
    select(analysis, category, weighted_n, weighted_percent)
}

# Reporting/data quality summaries.
missingness <- tibble(variable = c(risk_vars, "syndemic_score", "syndemic_tertile", "age_group", "education", "wealth", "residence", "division", "employment", "husband_education")) %>%
  mutate(
    n_total = nrow(analysis_data),
    n_missing = vapply(variable, function(v) sum(is.na(analysis_data[[v]])), integer(1)),
    n_nonmissing = n_total - n_missing,
    missing_percent = n_missing / n_total * 100
  )
write_csv(missingness, "data/paper_missingness_summary.csv")

sample_flow <- tibble(
  item = c(
    "Cleaned analytic records",
    "Records with complete primary covariates",
    "Records with all 9 risk indicators observed",
    "Records used in full ordinal model"
  ),
  n = c(
    nrow(analysis_data),
    sum(complete.cases(analysis_data[, c("syndemic_tertile", "age_group", "education", "wealth", "residence")])),
    sum(analysis_data$n_observed_indicators == length(risk_vars), na.rm = TRUE),
    sum(complete.cases(analysis_data[, c("syndemic_tertile", "age_group", "education", "wealth", "residence", "division", "employment", "husband_education")]))
  )
)
write_csv(sample_flow, "data/paper_sample_flow.csv")

# Domain summaries.
domain_summary <- bind_rows(
  weighted_mean(design_all, "syndemic_nutrition") %>% mutate(domain = "Nutrition score", scale = "0-2"),
  weighted_mean(design_all, "syndemic_reprod") %>% mutate(domain = "Reproductive health score", scale = "0-4"),
  weighted_mean(design_all, "syndemic_access") %>% mutate(domain = "Access/autonomy score", scale = "0-3"),
  weighted_mean(design_all, "syndemic_score") %>% mutate(domain = "Total syndemic score", scale = "0-9"),
  weighted_mean(design_all, "nutrition_any") %>% mutate(domain = "Any nutrition risk", scale = "0/1"),
  weighted_mean(design_all, "reproductive_any") %>% mutate(domain = "Any reproductive risk", scale = "0/1"),
  weighted_mean(design_all, "access_any") %>% mutate(domain = "Any access/autonomy risk", scale = "0/1")
) %>%
  mutate(
    estimate_display = if_else(
      scale == "0/1",
      sprintf("%.1f%% (%.1f-%.1f)", estimate * 100, ci_lower * 100, ci_upper * 100),
      sprintf("%.2f (%.2f-%.2f)", estimate, ci_lower, ci_upper)
    )
  ) %>%
  select(domain, scale, estimate, se, ci_lower, ci_upper, estimate_display)
write_csv(domain_summary, "data/paper_domain_summary.csv")

# Correct high burden by division for the primary >=4 cutoff and keep >=6 as very high.
division_high_ge4 <- svyby(~high_burden_ge4, ~division, design_all, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(
    prevalence_pct = high_burden_ge4 * 100,
    se_pct = se * 100,
    ci_lower = prevalence_pct - 1.96 * se_pct,
    ci_upper = prevalence_pct + 1.96 * se_pct,
    prev_ci = sprintf("%.1f%% (%.1f-%.1f)", prevalence_pct, ci_lower, ci_upper)
  ) %>%
  arrange(desc(prevalence_pct))
write_csv(division_high_ge4, "data/paper_division_high_ge4.csv")

division_very_high_ge6 <- svyby(~very_high_burden_ge6, ~division, design_all, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(
    prevalence_pct = very_high_burden_ge6 * 100,
    se_pct = se * 100,
    ci_lower = prevalence_pct - 1.96 * se_pct,
    ci_upper = prevalence_pct + 1.96 * se_pct,
    prev_ci = sprintf("%.1f%% (%.1f-%.1f)", prevalence_pct, ci_lower, ci_upper)
  ) %>%
  arrange(desc(prevalence_pct))
write_csv(division_very_high_ge6, "data/paper_division_very_high_ge6.csv")

# Domain co-occurrence and observed/expected clustering.
domain_complete <- analysis_data %>%
  filter(!is.na(nutrition_any), !is.na(reproductive_any), !is.na(access_any)) %>%
  mutate(
    domain_pattern = paste0(
      if_else(nutrition_any == 1, "Nutrition", "No nutrition"), " + ",
      if_else(reproductive_any == 1, "Reproductive", "No reproductive"), " + ",
      if_else(access_any == 1, "Access", "No access")
    ),
    nutr_reprod = nutrition_any * reproductive_any,
    nutr_access = nutrition_any * access_any,
    reprod_access = reproductive_any * access_any,
    all_three_domains = nutrition_any * reproductive_any * access_any
  )

design_domain <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = domain_complete,
  nest = TRUE
)

write_csv(
  weighted_distribution(design_domain, "domain_pattern", "Observed risk-domain pattern") %>%
    arrange(desc(weighted_percent)),
  "data/paper_domain_patterns.csv"
)

oe_calc <- function(design, exposure_vars, joint_var, label) {
  p <- lapply(exposure_vars, function(v) weighted_mean(design, v)$estimate)
  observed <- weighted_mean(design, joint_var)
  expected <- prod(unlist(p))
  tibble(
    clustering_test = label,
    observed_pct = observed$estimate * 100,
    expected_pct_under_independence = expected * 100,
    observed_expected_ratio = observed$estimate / expected,
    absolute_excess_pct = (observed$estimate - expected) * 100,
    ci_lower_pct = observed$ci_lower * 100,
    ci_upper_pct = observed$ci_upper * 100,
    n_complete_domain_records = nrow(design$variables)
  )
}

observed_expected <- bind_rows(
  oe_calc(design_domain, c("nutrition_any", "reproductive_any"), "nutr_reprod", "Nutrition + reproductive"),
  oe_calc(design_domain, c("nutrition_any", "access_any"), "nutr_access", "Nutrition + access/autonomy"),
  oe_calc(design_domain, c("reproductive_any", "access_any"), "reprod_access", "Reproductive + access/autonomy"),
  oe_calc(design_domain, c("nutrition_any", "reproductive_any", "access_any"), "all_three_domains", "All three domains")
) %>%
  mutate(
    result = sprintf(
      "%.1f%% observed vs %.1f%% expected; O/E %.2f",
      observed_pct,
      expected_pct_under_independence,
      observed_expected_ratio
    )
  )
write_csv(observed_expected, "data/paper_domain_observed_expected.csv")

# Correlation matrix from available pairwise observations.
corr_matrix <- analysis_data %>%
  select(all_of(risk_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")
write_csv(as_tibble(corr_matrix, rownames = "risk_factor"), "data/paper_risk_correlation_matrix.csv")

# Sensitivity: available-indicator scoring vs complete-case scoring.
complete_case_distribution <- bind_rows(
  weighted_distribution(design_all, "syndemic_tertile", "Available-indicator score"),
  weighted_distribution(subset(design_all, n_observed_indicators == length(risk_vars)), "complete_case_tertile", "Complete 9-indicator score")
)
write_csv(complete_case_distribution, "data/paper_complete_case_sensitivity.csv")

# Models and interaction tests.
model_vars <- c("syndemic_tertile", "syndemic_score", "high_burden_ge4", "very_high_burden_ge6",
                "age_group", "education", "wealth", "residence", "division", "employment", "husband_education")
model_data <- analysis_data %>%
  filter(complete.cases(across(all_of(model_vars))))

design_model <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = model_data,
  nest = TRUE
)

ordinal_base <- svyolr(
  syndemic_tertile ~ age_group + education + wealth + residence + division + employment + husband_education,
  design = design_model
)
ordinal_wr <- svyolr(
  syndemic_tertile ~ age_group + education + wealth * residence + division + employment + husband_education,
  design = design_model
)
ordinal_er <- svyolr(
  syndemic_tertile ~ age_group + education * residence + wealth + division + employment + husband_education,
  design = design_model
)

term_test <- function(model, term, label) {
  out <- tryCatch(regTermTest(model, term), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble(test = label, statistic = NA_real_, p_value = NA_real_, p_formatted = NA_character_))
  }
  stat <- as.numeric(unlist(out$Ftest))[1]
  p <- as.numeric(unlist(out$p))[1]
  tibble(
    test = label,
    statistic = stat,
    p_value = p,
    p_formatted = fmt_p(p)
  )
}

interaction_tests <- bind_rows(
  term_test(ordinal_wr, ~wealth:residence, "Ordinal model: wealth x residence"),
  term_test(ordinal_er, ~education:residence, "Ordinal model: education x residence")
)
write_csv(interaction_tests, "data/paper_social_interaction_tests.csv")

extract_model <- function(model, exponentiate = TRUE) {
  beta <- coef(model)
  se <- sqrt(diag(vcov(model)))[names(beta)]
  tibble(
    term = names(beta),
    estimate = beta,
    std_error = se,
    effect = if (exponentiate) exp(beta) else beta,
    ci_lower = if (exponentiate) exp(beta - 1.96 * se) else beta - 1.96 * se,
    ci_upper = if (exponentiate) exp(beta + 1.96 * se) else beta + 1.96 * se,
    p_value = 2 * pnorm(-abs(beta / se)),
    p_formatted = fmt_p(p_value),
    effect_ci = sprintf("%.2f (%.2f-%.2f)", effect, ci_lower, ci_upper)
  )
}

count_model <- svyglm(
  syndemic_score ~ age_group + education + wealth + residence + division + employment + husband_education,
  design = design_model,
  family = quasipoisson()
)
write_csv(extract_model(count_model), "data/paper_count_model_results.csv")

cat("Aggregate manuscript outputs written to data/.\n")
