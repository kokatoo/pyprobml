library(tidyverse)
library(patchwork)

set.seed(0)

mle_fn <- function(x) {
  mean(x)
}

theta <- 0.7
choices <- c(10, 100)
n_bootstrap_draws <- 10000

plots <- vector("list", length(choices))

for (i in seq_along(choices)) {

  n_data_points <- choices[i]

  # -----------------------------
  # Generate data
  # -----------------------------
  data <- rbinom(
    n_data_points,
    size = 1,
    prob = theta
  )

  mle <- mle_fn(data)

  # -----------------------------
  # Parametric bootstrap
  # -----------------------------
  bootstrap_samples_param <- replicate(
    n_bootstrap_draws,
    rbinom(n_data_points, size = 1, prob = mle)
  )

  bootstrap_mle_param <- colMeans(bootstrap_samples_param)

  p_param <- tibble(mle = bootstrap_mle_param) |>
    ggplot(aes(mle)) +
    geom_histogram(
      bins = 10,
      fill = "gray",
      color = "black"
    ) +
    geom_vline(
      xintercept = mle,
      color = "red",
      linetype = "dashed"
    ) +
    xlim(0, 1) +
    labs(
      title = paste("Bootstrap:", n_data_points),
      x = expression(hat(theta)^s)
    ) +
    theme_minimal()

  # -----------------------------
  # Non-parametric bootstrap
  # -----------------------------
  bootstrap_samples_nonparam <- replicate(
    n_bootstrap_draws,
    sample(
      data,
      size = n_data_points,
      replace = TRUE
    )
  )

  bootstrap_mle_nonparam <- colMeans(bootstrap_samples_nonparam)

  p_nonparam <- tibble(mle = bootstrap_mle_nonparam) |>
    ggplot(aes(mle)) +
    geom_histogram(
      bins = 10,
      fill = "gray",
      color = "black"
    ) +
    geom_vline(
      xintercept = mle,
      color = "red",
      linetype = "dashed"
    ) +
    xlim(0, 1) +
    labs(
      title = paste("Non-parametric:", n_data_points),
      x = expression(hat(theta)^s)
    ) +
    theme_minimal()

  # -----------------------------
  # Bayesian posterior sampling
  # -----------------------------
  counts_1 <- sum(data)
  counts_0 <- n_data_points - counts_1

  alpha <- 1
  beta <- 1

  posterior_alpha <- alpha + counts_1
  posterior_beta  <- beta + counts_0

  posterior_samples <- rbeta(
    n_bootstrap_draws,
    posterior_alpha,
    posterior_beta
  )

  p_bayes <- tibble(theta = posterior_samples) |>
    ggplot(aes(theta)) +
    geom_histogram(
      bins = 10,
      fill = "gray",
      color = "black"
    ) +
    geom_vline(
      xintercept = mle,
      color = "red",
      linetype = "dashed"
    ) +
    xlim(0, 1) +
    labs(
      title = paste("Bayes:", n_data_points),
      x = expression(theta^s)
    ) +
    theme_minimal()

  plots[[i]] <- p_param + p_nonparam + p_bayes
}

## wrap_plots(plots)
plots[[1]]
plots[[2]]
