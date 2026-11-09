frequentist <- function(data, n_control, n_treatment, mde = 0.0075, alpha = 0.05, power = 0.8) {

    # Baseline Conversion
    p1 <- mean(data$converted[data$group == "control"])

    # Target Conversion
    p2 <- p1 + mde

    # Average p
    p <- (p1 + p2) / 2

    # z-values for a two-sided test at alpha and for the chosen power
    z_alpha <- qnorm(1 - alpha / 2)
    z_power <- qnorm(power)

    # Minimum Sample Size
    n <- ((z_alpha * sqrt(2 * p * (1 - p)) + z_power * sqrt(p1 * (1 - p1) +
                                                     p2 * (1 - p2)))^2) / ((p2 - p1)^2)

    # Print
    cat("\nMinimum Sample Size:", n, "\n")

    # Control Conversion
    pc <- mean(data$converted[data$group == "control"])

    # Treatment Conversion
    pt <- mean(data$converted[data$group == "treatment"])

    # Pooled proportion
    pp <- (sum(data$converted[data$group == "control"]) +
             sum(data$converted[data$group == "treatment"])) / (n_control + n_treatment)

    # Compute test statistic:
    z <- (pt - pc) / sqrt(pp * (1 - pp) * ((1 / n_control) + (1 / n_treatment)))

    # Print
    cat("\nz-score:", z, "\n")

    # p - value
    p_value <- 2 * (1 - pnorm(abs(z)))

    # Print
    cat("\np-value:", p_value, "\n")

    # Standard Error
    se <- sqrt((pc * (1 - pc)) / n_control + (pt * (1 - pt)) / n_treatment)

    # Print
    cat("\nStandard Error:", se, "\n")

    # Confidence Intervals
    lower <- (pt - pc) - z_alpha * se
    upper <- (pt - pc) + z_alpha * se

    # Print
    cat("\n95% Confidence Intervals: [", lower, ",", upper, "]", "\n")

    # Return results in a list
    results <- list(
      conv_control = pc,
      conv_treatment = pt,
      delta = pt - pc,
      mde = mde,
      min_sample_size = n,
      z_score = z,
      p_value = p_value,
      std_error = se,
      conf_interval = c(lower, upper)
    )

    return(results)

}
