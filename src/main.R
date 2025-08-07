# Load packages
library("tidyverse")

# Set up a path to read the file
path <- file.choose()

# Read the file
ab_data <- read_csv(path)

# Transform to factors
ab_data <- ab_data|>
  mutate(
    group = factor(group),
    landing_page = factor(landing_page)
  )

## ----- Data Preparation -----

# Data Structure
str(ab_data)

# Data summary
summary(ab_data)

# Inspect data
head(ab_data)

# Check for missing values
colSums(is.na(ab_data))

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

# Identify mismatches
ab_data |>
  mutate(mismatches = case_when(
    group == "control" & landing_page == "new_page"  ~ "Control Mismatch",
    group == "treatment" & landing_page == "old_page" ~ "Treatment Mismatch",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(mismatches)) |>
  count(mismatches)

# Exclude mismatches
ab_data <- ab_data |>
  filter(!(
    (group == "control" & landing_page == "new_page") |
      (group == "treatment" & landing_page == "old_page")
  ))

# Identify duplicates
ab_data |>
  count(user_id) |>
  filter(n > 1)

# Drop duplicates
ab_data <- ab_data |>
  arrange(user_id, timestamp) |>
  distinct(user_id, .keep_all = TRUE)

# Clean Data summary
summary(ab_data)

# Variance Across Groups
ab_data |>
  group_by(group) |>
  summarise(std_dev = sd(converted), variance = var(converted), n = n(), .groups = "drop")

# Corrected cases per group
ggplot(ab_data, aes(x = group, fill = group)) + 
  geom_bar(colour = "black") + 
  scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
  labs(title = "Samples per Group", y = "Samples", x = "Group") +
  stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Corrected cases per page
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

## ----- Frequentist Analysis -----

# Baseline Conversion
p1 <- mean(ab_data$converted[ab_data$group == "control"])

# Target Conversion
p2 <- mean(ab_data$converted[ab_data$group == "control"]) + 0.0075

# Average p
p <- (p1 + p2)/ 2

# MDE
mde <- p2 - p1

# Relative lift
rlift <- (p2 - p1) / p1

# Minimum Sample Size
n <- ((1.96 * sqrt(2* p * (1-p)) + 0.84 * sqrt(p1 * (1 - p1) + p2 * (1-p2)))^2) / ((p2 - p1)^2)

# Print
cat("Minimum Sample Size:", n, "\n")

# Control Conversion
pc <- mean(ab_data$converted[ab_data$group == "control"])

# Treatment Conversion
pt <- mean(ab_data$converted[ab_data$group == "treatment"])

# Samples
n_control <- length(ab_data$group[ab_data$group == "control"])
n_treatment <- length(ab_data$group[ab_data$group == "treatment"])

# Pooled proportion 
pp <- (sum(ab_data$converted[ab_data$group == "control"]) + 
         sum(ab_data$converted[ab_data$group == "treatment"])) / (n_control + n_treatment)

# Compute test statistic:
z <- (pt - pc) /sqrt(pp * (1-pp) * ((1/n_control) + (1/n_treatment)))

# Print
cat("z-score:", z, "\n")

# p - value
p_value <- 2 * (1 - pnorm(abs(z)))

# Print
cat("p-value:", p_value, "\n")

# Standard Error
se <- sqrt((pc * (1 - pc)) / n_control + (pt * (1 - pt)) / n_treatment)

# Print
cat("Standard Error:", se, "\n")

# Confidence Intervals
lower <- (pt - pc) - 1.96 * se
upper <- (pt - pc) + 1.96 * se

# Print
cat(" 95% Confidence Intervals: [", lower, ",", upper, "]", "\n")

## ----- Bayesian Analysis -----

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
    title = "Total Coversions per Group", x = "Group",
    y = "Conversions", fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Posterior parameters for A
alpha_A <- 1 + sum(ab_data$converted[ab_data$group == "control"])
beta_A <- 1 + n_control - sum(ab_data$converted[ab_data$group == "control"])

# Posterior parameters for B
alpha_B <- 1 + sum(ab_data$converted[ab_data$group == "treatment"])
beta_B <- 1 + n_treatment - sum(ab_data$converted[ab_data$group == "treatment"])

# Bayesian Analysis Sampling
samples_A <- rbeta(100000, alpha_A, beta_A)
samples_B <- rbeta(100000, alpha_B, beta_B)  

# Difference in conversion rate
diff <- samples_B - samples_A

# Probability that B is better than A
delta <- mean(diff > 0)

# Print
cat("Probability that B is better than A:", delta,"\n")

# 95% credible interval
cred_int <- quantile(diff, c(0.025, 0.975))

# Print
cat("95% credible intervals:[", cred_int, "]", "\n")

# ROPE bounds
rope_lower <- -0.003
rope_upper <-  0.003

# ROPE coverage
rc <- sum(diff > - 0.003 & diff < 0.003) / length(diff)

# Print
cat("ROPE: [", rope_lower, ",", rope_upper, "]\n")
cat("ROPE coverage:", rc, "\n")

# Posterior difference distribution
ggplot(as.data.frame(diff), aes(x = diff)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
  geom_vline(xintercept = cred_int, linetype = "dotted", color = "darkblue") +
  labs(
    title = "Posterior Distribution of Conversion Rate Difference (Treatment - Control)",
    x = "Difference in Conversion Rate (pt - pc)",
    y = "Density",
    caption = paste0("P(T > C) = ", round(delta, 3),
                     "; 95% CI: [", round(cred_int[1], 4),
                     ", ", round(cred_int[2], 4), "]"))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## -----Expanded Analysis -----

# Set up a path to read the file
path <- file.choose()

# Read the file
countries <- read_csv(path)

# Transform to factors
countries <- countries|>
  mutate(
    country = factor(country)
    )

# Data Structure
str(countries)

# Data summary
summary(countries)

# Inspect data
head(countries)

# Check for missing values
colSums(is.na(countries))

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

# Samples per Group per Country
ab_data_exp |> count(country, group)

# Conversions per Group per Country
ab_data_exp |>
  group_by(country,group) |>
  summarise(conversions = sum(converted), .groups = "drop")

# Conversion Rate per Group per Country
ab_data_exp |>
  group_by(country, group) |>
  summarise(conversion_rate = mean(converted), .groups = "drop")

# Samples
US_n_control <- length(ab_data_exp$group[ab_data_exp$group == "control" 
                                         & ab_data_exp$country == "US"])
US_n_treatment <- length(ab_data_exp$group[ab_data_exp$group == "treatment" 
                                           & ab_data_exp$country == "US"])
UK_n_control <- length(ab_data_exp$group[ab_data_exp$group == "control" 
                                         & ab_data_exp$country == "UK"])
UK_n_treatment <- length(ab_data_exp$group[ab_data_exp$group == "treatment" 
                                           & ab_data_exp$country == "UK"])
CA_n_control <- length(ab_data_exp$group[ab_data_exp$group == "control" 
                                         & ab_data_exp$country == "CA"])
CA_n_treatment <- length(ab_data_exp$group[ab_data_exp$group == "treatment" 
                                           & ab_data_exp$country == "CA"])

# Posterior parameters for A
US_alpha_A <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                            & ab_data_exp$country == "US"])
US_beta_A <- 1 + US_n_control - sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                                       & ab_data_exp$country == "US"])
UK_alpha_A <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                            & ab_data_exp$country == "UK"])
UK_beta_A <- 1 + UK_n_control - sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                                       & ab_data_exp$country == "UK"])
CA_alpha_A <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                            & ab_data_exp$country == "CA"])
CA_beta_A <- 1 + CA_n_control - sum(ab_data_exp$converted[ab_data_exp$group == "control" 
                                                   & ab_data_exp$country == "CA"])

# Posterior parameters for B
US_alpha_B <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "treatment"
                      & ab_data_exp$country == "US"])
US_beta_B <- 1 + US_n_treatment - sum(ab_data_exp$converted[ab_data_exp$group == "treatment" 
                                   & ab_data_exp$country == "US"])
UK_alpha_B <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "treatment"
                      & ab_data_exp$country == "UK"])
UK_beta_B <- 1 + UK_n_treatment - sum(ab_data_exp$converted[ab_data_exp$group == "treatment"
                                   & ab_data_exp$country == "UK"])
CA_alpha_B <- 1 + sum(ab_data_exp$converted[ab_data_exp$group == "treatment"
                      & ab_data_exp$country == "CA"])
CA_beta_B <- 1 + CA_n_treatment - sum(ab_data_exp$converted[ab_data_exp$group == "treatment"
                                   & ab_data_exp$country == "CA"])

# Bayesian Analysis Sampling
US_samples_A <- rbeta(100000, US_alpha_A, US_beta_A)
US_samples_B <- rbeta(100000, US_alpha_B, US_beta_B)
UK_samples_A <- rbeta(100000, UK_alpha_A, UK_beta_A)
UK_samples_B <- rbeta(100000, UK_alpha_B, UK_beta_B)
CA_samples_A <- rbeta(100000, CA_alpha_A, CA_beta_A)
CA_samples_B <- rbeta(100000, CA_alpha_B, CA_beta_B)

# Difference in conversion rate
US_diff <- US_samples_B - US_samples_A
UK_diff <- UK_samples_B - UK_samples_A
CA_diff <- CA_samples_B - CA_samples_A

# Probability that B is better than A
US_delta <- mean(US_diff > 0)
UK_delta <- mean(UK_diff > 0)
CA_delta <- mean(CA_diff > 0)

# Print
cat("Probability that B is better than A (US):", US_delta,"\n")
cat("Probability that B is better than A (UK):", UK_delta,"\n")
cat("Probability that B is better than A (CA):", CA_delta,"\n")

# 95% credible interval
US_cred_int <- quantile(US_diff, c(0.025, 0.975))
UK_cred_int <- quantile(UK_diff, c(0.025, 0.975))
CA_cred_int <- quantile(CA_diff, c(0.025, 0.975))

# Print
cat("95% credible intervals (US):[", US_cred_int, "]", "\n")
cat("95% credible intervals (UK):[", UK_cred_int, "]", "\n")
cat("95% credible intervals (CA):[", CA_cred_int, "]", "\n")

# ROPE bounds
rope_lower <- -0.003
rope_upper <-  0.003

# ROPE coverage
US_rc <- sum(US_diff > - 0.003 & US_diff < 0.003) / length(US_diff)
UK_rc <- sum(UK_diff > - 0.003 & UK_diff < 0.003) / length(UK_diff)
CA_rc <- sum(CA_diff > - 0.003 & CA_diff < 0.003) / length(CA_diff)

# Print
cat("ROPE: [", rope_lower, ",", rope_upper, "]\n")
cat("ROPE coverage (US):", US_rc, "\n")
cat("ROPE coverage (UK):", UK_rc, "\n")
cat("ROPE coverage (CA):", CA_rc, "\n")

# Posterior difference distribution (US)
ggplot(as.data.frame(US_diff), aes(x = US_diff)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
  geom_vline(xintercept = US_cred_int, linetype = "dotted", color = "darkblue") +
  labs(
    title = "Posterior Distribution of Conversion Rate Difference (Treatment - Control) (US)",
    x = "Difference in Conversion Rate (pt - pc)",
    y = "Density",
    caption = paste0("P(T > C) = ", round(US_delta, 3),
                     "; 95% CI: [", round(US_cred_int[1], 4),
                     ", ", round(US_cred_int[2], 4), "]"))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Posterior difference distribution (UK)
ggplot(as.data.frame(UK_diff), aes(x = UK_diff)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
  geom_vline(xintercept = UK_cred_int, linetype = "dotted", color = "darkblue") +
  labs(
    title = "Posterior Distribution of Conversion Rate Difference (Treatment - Control) (UK)",
    x = "Difference in Conversion Rate (pt - pc)",
    y = "Density",
    caption = paste0("P(T > C) = ", round(UK_delta, 3),
                     "; 95% CI: [", round(UK_cred_int[1], 4),
                     ", ", round(UK_cred_int[2], 4), "]"))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Posterior difference distribution (CA)
ggplot(as.data.frame(CA_diff), aes(x = CA_diff)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
  geom_vline(xintercept = CA_cred_int, linetype = "dotted", color = "darkblue") +
  labs(
    title = "Posterior Distribution of Conversion Rate Difference (Treatment - Control) (CA)",
    x = "Difference in Conversion Rate (pt - pc)",
    y = "Density",
    caption = paste0("P(T > C) = ", round(CA_delta, 3),
                     "; 95% CI: [", round(CA_cred_int[1], 4),
                     ", ", round(CA_cred_int[2], 4), "]"))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))