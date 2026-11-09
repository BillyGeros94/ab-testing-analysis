# Charts shared by main.R and the report (report/report.Rmd), so both always
# show the same figures.

clean_theme <- function() {
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Users per group
plot_group_counts <- function(data) {
  ggplot(data, aes(x = group, fill = group)) +
    geom_bar(colour = "black") +
    scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
    labs(title = "Samples per Group", y = "Samples", x = "Group") +
    stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    clean_theme()
}

# Users per landing page
plot_page_counts <- function(data) {
  ggplot(data, aes(x = landing_page, fill = landing_page)) +
    geom_bar(colour = "black") +
    scale_fill_manual(values = c("old_page" = "coral", "new_page" = "skyblue")) +
    labs(title = "Samples per Page", y = "Samples", x = "Landing Page") +
    stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    clean_theme()
}

# Conversion rate per group
plot_conversion_rate <- function(data) {
  data |>
    group_by(group) |>
    summarise(conversion_rate = mean(converted), .groups = "drop") |>
    ggplot(aes(x = group, y = conversion_rate, fill = group)) +
    geom_col() +
    geom_text(aes(label = scales::percent(conversion_rate, accuracy = 0.01)),
              vjust = -0.5, color = "black", size = 3.5) +
    scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Conversion Rate per Group", y = "Conversion Rate", x = "Group") +
    clean_theme()
}

# Share of converted and not converted users in each group
plot_conversion_percentages <- function(data) {
  data |>
    group_by(group, converted) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(group) |>
    mutate(percent = n / sum(n)) |>
    ggplot(aes(x = group, y = percent, fill = factor(converted))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
    geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
              position = position_dodge(width = 0.9), vjust = -0.2, size = 3.5, color = "black") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("0" = "coral", "1" = "skyblue"), name = "Converted") +
    labs(title = "Conversion Percentages per Group", x = "Group", y = "Percentage") +
    clean_theme()
}

# Users per group per day
plot_daily_allocation <- function(data) {
  data |>
    mutate(date = as_date(timestamp)) |>
    group_by(date, group) |>
    summarise(n = n(), .groups = "drop") |>
    ggplot(aes(x = date, y = n, fill = group)) +
    geom_col(position = "dodge") +
    labs(title = "Daily User Allocation per Group", x = "Date", y = "User Count", fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Conversion rate per group per day
plot_daily_rate <- function(data) {
  data |>
    mutate(date = as_date(timestamp)) |>
    group_by(date, group) |>
    summarise(rate = mean(converted), .groups = "drop") |>
    ggplot(aes(x = date, y = rate, color = group)) +
    geom_line() +
    labs(title = "Daily Conversion Rate by Group", x = "Date", y = "Conversion Rate", color = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Total conversions per group
plot_conversions <- function(data) {
  data |>
    group_by(group) |>
    summarise(conversions = sum(converted), .groups = "drop") |>
    ggplot(aes(x = group, y = conversions, fill = group)) +
    geom_col(colour = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = c("control" = "coral", "treatment" = "skyblue")) +
    geom_text(aes(label = conversions), vjust = -0.5) +
    labs(title = "Total Conversions per Group", x = "Group", y = "Conversions", fill = "Group") +
    clean_theme()
}

# Users per country
plot_country_counts <- function(data) {
  ggplot(data, aes(x = country, fill = country)) +
    geom_bar(colour = "black") +
    scale_fill_manual(values = c("UK" = "coral", "US" = "skyblue", "CA" = "grey")) +
    labs(title = "Samples per Country", y = "Samples", x = "Country") +
    stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    clean_theme()
}

# Posterior of the difference in conversion rate, from bayesian() or expanded()
plot_posterior <- function(results, country_name = NULL) {
  title <- "Posterior Distribution of Conversion Rate Difference (Treatment - Control)"
  if (!is.null(country_name)) title <- paste0(title, " (", country_name, ")")
  ggplot(data.frame(diff = results$diff), aes(x = diff)) +
    geom_density(fill = "skyblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = results$cred_int, linetype = "dotted", color = "darkblue") +
    labs(
      title = title,
      x = "Difference in Conversion Rate (pt - pc)",
      y = "Density",
      caption = paste0("P(T > C) = ", round(results$delta, 3),
                       "; 95% CI: [", round(results$cred_int[1], 4),
                       ", ", round(results$cred_int[2], 4), "]")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
