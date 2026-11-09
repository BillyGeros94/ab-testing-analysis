source("src/load_data.R")
source("src/data_prep.R")
source("src/frequentist.R")
source("src/bayesian.R")
source("src/expanded.R")
source("src/checks.R")
source("src/plots.R")

# Load packages
library(tidyverse)

# Make the Monte Carlo results repeatable
set.seed(42)

# Load the data
ab_data <- load_data(file_path = "data/ab_data.csv")

## ----------------- INITIAL DATA INSPECTION -----------------

# Data Structure
cat("\nData Structure:\n")
str(ab_data)

# Data summary
cat("\nData Summary:\n")
print(summary(ab_data))

# Inspect data
cat("\nData Inspection:\n")
print(head(ab_data))

# Check for missing values
cat("\nMissing values found:\n")
print(colSums(is.na(ab_data)))

## ----------------- DATA PREPARATION -----------------

cat("\n--- Data Preparation ---\n")
ab_raw <- ab_data
ab_data <- prepare_data(ab_data)

# Clean Data summary
cat("\nClean Data Summary:\n")
summary(ab_data)

# Variance Across Groups
cat("\nVariance across groups:\n")
ab_data |>
        group_by(group) |>
        summarise(std_dev = sd(converted), variance = var(converted), n = n(), .groups = "drop")

# Cases per group and per page
plot_group_counts(ab_data)
plot_page_counts(ab_data)

# Conversion Rate per Group
ab_data |>
  group_by(group) |>
  summarise(conversion_rate = mean(converted), .groups = "drop")

# Conversion Rate per Page
ab_data |> 
  group_by(landing_page) |>
  summarise(conversion_rate = mean(converted), .groups = "drop")

# Conversion charts
plot_conversion_rate(ab_data)
plot_conversion_percentages(ab_data)

# User allocation and conversion rate over time
plot_daily_allocation(ab_data)
plot_daily_rate(ab_data)

## ----------------- VALIDITY CHECKS -----------------

cat("\n--- Validity checks ---\n")
srm_p <- srm_check(ab_data)
weekly_results <- weekly_effect(ab_data)
both_pages_results <- both_pages_sensitivity(ab_raw, ab_data)

## ----------------- FREQUENTIST ANALYSIS ----------------

# Samples
n_control <- length(ab_data$group[ab_data$group == "control"])
n_treatment <- length(ab_data$group[ab_data$group == "treatment"])

# Frequentist results
cat("\n--- Frequentist analysis ---\n")
frequentist_results <- frequentist(ab_data, n_control, n_treatment)

## ----------------- BAYESIAN ANALYSIS ----------------

# Conversions per Group
plot_conversions(ab_data)

# Bayesian results
cat("\n--- Bayesian analysis ---\n")
bayesian_results <- bayesian(ab_data, n_control, n_treatment,
                             mde = frequentist_results$mde)

# Posterior difference distribution
plot_posterior(bayesian_results)

## ----------------- EXPANDED ANALYSIS -----------------

# Read the file
countries <- load_data(file_path = "data/countries.csv")

# Transform to factors
countries <- countries|>
  mutate(
    country = factor(country)
    )

cat("\nData Structure:\n")
str(countries)

# Data summary
cat("\nData Summary:\n")
print(summary(countries))

# Inspect data
cat("\nData Inspection:\n")
print(head(countries))

# Check for missing values
cat("\nMissing values found:\n")
print(colSums(is.na(countries)))

# Join data 
ab_data_exp <- inner_join(ab_data, countries, by = "user_id")

# Cases per Country
plot_country_counts(ab_data_exp)

# Vector of the countries 
countries_to_analyze <- c("US", "UK", "CA")

# Empty list
expanded_analysis <- list()
expanded_plots <- list()

# Bayesian comparison and posterior plot for each country
for (country_name in countries_to_analyze) {
  expanded_analysis[[country_name]] <- expanded(ab_data_exp, country_name,
                                                mde = frequentist_results$mde)
  expanded_plots[[country_name]] <- plot_posterior(expanded_analysis[[country_name]], country_name)
}

# Posterior difference distribution (US)
expanded_plots$US

# Posterior difference distribution (UK)
expanded_plots$UK

# Posterior difference distribution (CA)
expanded_plots$CA

# Frequentist tests per country
country_results <- country_tests(ab_data_exp)

## ----------------- SAVE KEY NUMBERS -----------------

key_numbers <- tibble(
  metric = c(
    "rows_raw", "users", "n_control", "n_treatment",
    "conv_rate_control", "conv_rate_treatment", "delta",
    "z_score", "p_value", "ci_lower", "ci_upper", "mde",
    "min_sample_size_per_group", "srm_p_value",
    "bayes_p_treatment_better", "bayes_p_reaches_mde", "rope_coverage",
    "cred_lower", "cred_upper",
    "both_pages_users_removed", "both_pages_delta", "both_pages_p_value",
    "both_pages_ci_lower", "both_pages_ci_upper",
    "country_interaction_p_value"
  ),
  value = c(
    nrow(ab_raw), nrow(ab_data), n_control, n_treatment,
    frequentist_results$conv_control, frequentist_results$conv_treatment,
    frequentist_results$delta, frequentist_results$z_score,
    frequentist_results$p_value, frequentist_results$conf_interval,
    frequentist_results$mde, frequentist_results$min_sample_size, srm_p,
    bayesian_results$delta, bayesian_results$p_mde, bayesian_results$rc,
    unname(bayesian_results$cred_int),
    both_pages_results$users_removed, both_pages_results$delta,
    both_pages_results$p_value, both_pages_results$conf_interval,
    country_results$interaction_p
  )
)

dir.create("results", showWarnings = FALSE)
write_csv(key_numbers, "results/key_numbers.csv")
write_csv(country_results$per_country, "results/country_tests.csv")
write_csv(weekly_results, "results/weekly_effect.csv")