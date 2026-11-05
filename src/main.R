source("src/load_data.R")
source("src/data_prep.R")
source("src/frequentist.R")
source("src/bayesian.R")
source("src/expanded.R")
source("src/checks.R")

# Install and load packages
install_and_load(packages)

# Make the Monte Carlo results repeatable
set.seed(42)

# Load the data
ab_data <- load_data(file_path = "data/ab_data.csv")

## ----------------- INITIAL DATA INSPECTION -----------------

# Data Structure
cat("\nData Structure:\n")
print(str(ab_data))

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

# Cases per group
ggplot(ab_data, aes(x = group, fill = group)) + 
  geom_bar(colour = "black") + 
  scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
  labs(title = "Samples per Group", y = "Samples", x = "Group") +
  stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Cases per page
ggplot(ab_data, aes(x = landing_page , fill = landing_page )) + 
  geom_bar(colour = "black") + 
  scale_fill_manual(values = c("old_page" = "coral", "new_page" = "skyblue")) +
  labs(title = "Samples per Page", y = "Samples", x = "Landing Page") +
  stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Conversion Rate per Group
ab_data |>
  group_by(group) |>
  summarise(conversion_rate = mean(converted), .groups = "drop")

# Conversion Rate per Page
ab_data |> 
  group_by(landing_page) |>
  summarise(conversion_rate = mean(converted), .groups = "drop")

# Visualize Conversion Rate
ab_data |>
  group_by(group) |>
  summarise(conversion_rate = mean(converted), .groups = "drop") |>
  ggplot(aes(x = group, y = conversion_rate, fill = group)) +
  geom_col() +
  geom_text(aes(label = scales::percent(conversion_rate, accuracy = 0.1)),
            vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Conversion Rate per Group", y = "Conversion Rate", x = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
# Conversion Percentages per Group 
ab_data |>
  group_by(group, converted) |>  
  summarise(n = n(), .groups = "drop") |>
  group_by(group) |> 
  mutate(percent = n / sum(n)) |>
  ggplot(aes(x = group, y = percent, fill = factor(converted))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_text(
    aes(label = scales::percent(percent, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.2,
    size = 3.5,
    color = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "coral", "1" = "skyblue"), name = "Converted") +
  labs(
    title = "Conversion Percentages per Group",
    x = "Group",
    y = "Percentage"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# User Allocation
ab_data |>
  mutate(date = as_date(timestamp)) |>
  group_by(date, group) |>
  summarise(n = n(), .groups = "drop") |>
  ggplot(aes(x = date, y = n, fill = group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Daily User Allocation per Group", x = "Date",
    y = "User Count", fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Conversion Rate trend
ab_data |>
  mutate(date = as_date(timestamp)) |>
  group_by(date, group) |>
  summarise(rate = mean(converted), .groups = "drop") |>
  ggplot(aes(x = date, y = rate, color = group)) +
  geom_line() +
  labs(
    title = "Daily Conversion Rate by Group", x = "Date",
    y = "Conversion Rate", color = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

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
ab_data |>
  group_by(group) |>
  summarise(conversions = sum(converted), .groups = "drop") |>
  ggplot(aes(x = group, y = conversions, fill = group)) + 
  geom_col(colour = "black") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
  geom_text(aes(label = conversions), vjust = -0.5) + 
  labs(
    title = "Total Conversions per Group", x = "Group",
    y = "Conversions", fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Bayesian results
cat("\n--- Bayesian analysis ---\n")
bayesian_results <- bayesian(ab_data, n_control, n_treatment)

# Posterior difference distribution
ggplot(as.data.frame(bayesian_results$diff), aes(x = bayesian_results$diff)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
  geom_vline(xintercept = bayesian_results$cred_int, linetype = "dotted", color = "darkblue") +
  labs(
    title = "Posterior Distribution of Conversion Rate Difference (Treatment - Control)",
    x = "Difference in Conversion Rate (pt - pc)",
    y = "Density",
    caption = paste0("P(T > C) = ", round(bayesian_results$delta, 3),
                     "; 95% CI: [", round(bayesian_results$cred_int[1], 4),
                     ", ", round(bayesian_results$cred_int[2], 4), "]"))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## ----------------- EXPANDED ANALYSIS -----------------

# Read the file
countries <- load_data(file_path = "data/countries.csv")

# Transform to factors
countries <- countries|>
  mutate(
    country = factor(country)
    )

cat("\nData Structure:\n")
print(str(countries))

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
ggplot(ab_data_exp, aes(x = country , fill = country )) + 
  geom_bar(colour = "black") + 
  scale_fill_manual(values = c("UK" = "coral", "US" = "skyblue", "CA" = "grey")) +
  labs(title = "Samples per Country", y = "Samples", x = "Country") +
  stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Vector of the countries 
countries_to_analyze <- c("US", "UK", "CA")

# Empty list
expanded_analysis <- list()
expanded_plots <- list()

# Loop to generate each plot
for (country_name in countries_to_analyze) {
  
  # Get the results for the current country
  expanded_analysis[[country_name]] <- expanded(ab_data_exp, country_name)
  
  # Create the plot
  expanded_plots[[country_name]] <- ggplot(as.data.frame(expanded_analysis[[country_name]]$diff), aes(x = expanded_analysis[[country_name]]$diff)) +
    geom_density(fill = "skyblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = expanded_analysis[[country_name]]$cred_int, linetype = "dotted", color = "darkblue") +
    labs(
      title = paste("Posterior Distribution of Conversion Rate Difference (Treatment - Control) (", country_name, ")"),
      x = "Difference in Conversion Rate (pt - pc)",
      y = "Density",
      caption = paste0("P(T > C) = ", round(expanded_analysis[[country_name]]$delta, 3),
                       "; 95% CI: [", round(expanded_analysis[[country_name]]$cred_int[1], 4),
                       ", ", round(expanded_analysis[[country_name]]$cred_int[2], 4), "]")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
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