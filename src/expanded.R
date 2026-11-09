# Bayesian comparison for one country: the same model as bayesian(), run on
# that country's users only.
expanded <- function(data, country_name, ...) {

    country_data <- data |> filter(country == country_name)

    # Samples, conversions and conversion rate per group
    cat("\nGroups in", country_name, ":\n")
    print(country_data |>
            group_by(group) |>
            summarise(samples = n(), conversions = sum(converted),
                      conversion_rate = mean(converted), .groups = "drop"))

    n_control <- sum(country_data$group == "control")
    n_treatment <- sum(country_data$group == "treatment")

    cat("\nBayesian comparison for", country_name, ":\n")
    bayesian(country_data, n_control, n_treatment, ...)
}
