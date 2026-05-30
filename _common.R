# Shared setup sourced by every page of the site.
# Each .qmd renders in its own R session, so libraries + helpers are loaded here
# rather than duplicated across files.

library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(stringr)
library(tidyr)

theme_set(theme_minimal(base_size = 13))

pct1 <- function(x) sprintf("%.1f%%", x)

format_effect <- function(effect, ci_low, ci_high) {
  sprintf("%.2f (%.2f–%.2f)", effect, ci_low, ci_high)
}

clean_variable_group <- function(variable) {
  case_when(
    variable == "age_group"        ~ "Age group",
    variable == "education"        ~ "Respondent education",
    variable == "wealth"           ~ "Wealth quintile",
    variable == "residence"        ~ "Residence",
    variable == "division"         ~ "Division",
    variable == "employment"       ~ "Employment",
    variable == "husband_education"~ "Husband's education",
    TRUE                           ~ variable
  )
}
