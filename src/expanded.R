expanded <- function(data, country_name){

    # Samples per Group per Country
    cat("\nGroup samples for", country_name, ":\n")
    print(data |> filter(country == country_name) |> count(group))

    # Conversions per Group per Country
    cat("\nGroup conversions for", country_name, ":\n")
    print(data |>
            filter(country == country_name) |>
            group_by(group) |>
            summarise(conversions = sum(converted), .groups = "drop"))

    # Conversion Rate per Group per Country
    cat("\nConversion rate for", country_name, ":\n")
    print(data |>
            filter(country == country_name) |>
            group_by(group) |>
            summarise(conversion_rate = mean(converted), .groups = "drop"))

    # Samples
    n_control <- length(data$group[data$group == "control" 
                                         & data$country == country_name])
    n_treatment <- length(data$group[data$group == "treatment" 
                                             & data$country == country_name])

    # Posterior parameters for A
    alpha_A <- 1 + sum(data$converted[data$group == "control" 
                                            & data$country == country_name])
    beta_A <- 1 + n_control - sum(data$converted[data$group == "control" 
                                                          & data$country == country_name])

    # Posterior parameters for B
    alpha_B <- 1 + sum(data$converted[data$group == "treatment"
                                            & data$country == country_name])
    beta_B <- 1 + n_treatment - sum(data$converted[data$group == "treatment" 
                                                            & data$country == country_name])

    # Bayesian Analysis Sampling
    samples_A <- rbeta(100000, alpha_A, beta_A)
    samples_B <- rbeta(100000, alpha_B, beta_B)

    # Difference in conversion rate
    diff <- samples_B - samples_A

    # Probability that B is better than A
    delta <- mean(diff > 0)

    # Print
    cat("\nProbability that B is better than A for", country_name, ":", delta, "\n")

    # 95% credible interval
    cred_int <- quantile(diff, c(0.025, 0.975))

    # Print
    cat("\n95% credible intervals for", country_name, ":[", cred_int, "]", "\n")


    # ROPE bounds
    rope_lower <- -0.003
    rope_upper <-  0.003

    # ROPE coverage
    rc <- sum(diff > - 0.003 & diff < 0.003) / length(diff)

    # Print
    cat("\nROPE: [", rope_lower, ",", rope_upper, "]\n")
    cat("\nROPE coverage for", country_name, ":", rc, "\n")

    # Return results in a list
    results <- list(
      diff = diff,
      delta = delta,
      cred_int = cred_int,
      rc = rc
    )

    return(results)
}