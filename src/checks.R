# Experiment validity checks and robustness tests.

# Sample ratio mismatch: was the 50/50 split achieved?
srm_check <- function(data) {
  counts <- table(data$group)
  test <- chisq.test(counts, p = c(0.5, 0.5))
  cat("\nSample ratio mismatch p-value:", test$p.value, "\n")
  test$p.value
}

# Treatment effect per experiment week: a fading effect would suggest novelty.
weekly_effect <- function(data) {
  start <- min(as_date(data$timestamp))
  result <- data |>
    mutate(week = as.integer(as_date(timestamp) - start) %/% 7 + 1) |>
    group_by(week, group) |>
    summarise(rate = mean(converted), n = n(), .groups = "drop") |>
    pivot_wider(names_from = group, values_from = c(rate, n)) |>
    mutate(delta = rate_treatment - rate_control)
  cat("\nTreatment effect by week:\n")
  print(result)
  result
}

# Rows whose group and landing page disagree belong to users who stay in the
# cleaned data through another row, so they may have seen both pages. Re-run
# the test without any of those users.
both_pages_sensitivity <- function(raw, clean) {
  affected <- raw |>
    filter((group == "control") != (landing_page == "old_page")) |>
    distinct(user_id)
  kept <- anti_join(clean, affected, by = "user_id")
  counts <- kept |>
    group_by(group) |>
    summarise(conversions = sum(converted), n = n(), .groups = "drop")
  treatment <- counts[counts$group == "treatment", ]
  control <- counts[counts$group == "control", ]
  test <- prop.test(c(treatment$conversions, control$conversions),
                    c(treatment$n, control$n), correct = FALSE)
  result <- list(
    users_removed = nrow(clean) - nrow(kept),
    n_control = control$n,
    n_treatment = treatment$n,
    delta = unname(test$estimate[1] - test$estimate[2]),
    p_value = test$p.value,
    conf_interval = as.numeric(test$conf.int)
  )
  cat("\nExcluding users with a mismatched row:\n")
  str(result)
  result
}

# Per-country tests with a Holm correction, plus a likelihood-ratio test of
# whether the treatment effect differs by country at all.
country_tests <- function(data) {
  countries <- sort(unique(as.character(data$country)))
  per_country <- bind_rows(lapply(countries, function(name) {
    rows <- data[data$country == name, ]
    treatment <- rows$converted[rows$group == "treatment"]
    control <- rows$converted[rows$group == "control"]
    test <- prop.test(c(sum(treatment), sum(control)),
                      c(length(treatment), length(control)), correct = FALSE)
    tibble(
      country = name,
      n_control = length(control),
      n_treatment = length(treatment),
      delta = mean(treatment) - mean(control),
      p_value = test$p.value,
      ci_lower = test$conf.int[1],
      ci_upper = test$conf.int[2]
    )
  })) |>
    mutate(p_holm = p.adjust(p_value, method = "holm"))

  main_effects <- glm(converted ~ group + country, family = binomial, data = data)
  interaction <- glm(converted ~ group * country, family = binomial, data = data)
  lrt <- anova(main_effects, interaction, test = "Chisq")
  interaction_p <- lrt[2, ncol(lrt)]

  cat("\nPer-country tests:\n")
  print(per_country)
  cat("\nTreatment x country interaction p-value:", interaction_p, "\n")
  list(per_country = per_country, interaction_p = interaction_p)
}
