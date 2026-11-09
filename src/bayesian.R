# Beta-Binomial comparison of the two groups. mde is the planned lift and rope
# the half-width of the region of practical equivalence around zero.
bayesian <- function(data, n_control, n_treatment, mde = 0.0075, rope = 0.003) {

    # Posterior parameters for A
    alpha_A <- 1 + sum(data$converted[data$group == "control"])
    beta_A <- 1 + n_control - sum(data$converted[data$group == "control"])

    # Posterior parameters for B
    alpha_B <- 1 + sum(data$converted[data$group == "treatment"])
    beta_B <- 1 + n_treatment - sum(data$converted[data$group == "treatment"])

    # Bayesian Analysis Sampling
    samples_A <- rbeta(100000, alpha_A, beta_A)
    samples_B <- rbeta(100000, alpha_B, beta_B)

    # Difference in conversion rate
    diff <- samples_B - samples_A

    # Probability that B is better than A
    delta <- mean(diff > 0)

    # Print
    cat("\nProbability that B is better than A:", delta, "\n")

    # 95% credible interval
    cred_int <- quantile(diff, c(0.025, 0.975))

    # Print
    cat("\n95% credible intervals:[", cred_int, "]", "\n")

    # ROPE coverage
    rc <- mean(diff > -rope & diff < rope)

    # Print
    cat("\nROPE: [", -rope, ",", rope, "]\n")
    cat("\nROPE coverage:", rc, "\n")

    # Probability that the lift reaches the planned MDE
    p_mde <- mean(diff >= mde)

    # Print
    cat("\nProbability that the lift reaches the MDE:", p_mde, "\n")

    # Return results in a list
    results <- list(
      p_mde = p_mde,
      diff = diff,
      delta = delta,
      cred_int = cred_int,
      rc = rc
    )

    return(results)

}
