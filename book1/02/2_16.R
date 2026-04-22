library(patchwork)
library(tidyverse)
library(extraDistr)

plot_outlier_effect_r <- function(
  outlier_pos = 0,
  outliers = NULL,
  bins = 13,
  samples_norm_dist = 30,
  samples_graph_xaxis = 500,
  range_xaxis = c(-5, 10),
  range_yaxis = c(0, 0.6)
) {

  # -----------------------
  # Sample data
  # -----------------------
  norm_sample <- rnorm(samples_norm_dist, mean = 0, sd = 1)

  if (!is.null(outliers) && length(outliers) > 0) {
    samples <- c(norm_sample, outliers + outlier_pos)
  } else {
    samples <- norm_sample
  }

  df_hist <- tibble(x = samples)

  # -----------------------
  # x grid
  # -----------------------
  x_axis <- seq(
    range_xaxis[1],
    range_xaxis[2],
    length.out = samples_graph_xaxis
  )

  # -----------------------
  # Gaussian fit (MLE)
  # -----------------------
  norm_fit <- MASS::fitdistr(samples, "normal")
  norm_pdf <- dnorm(
    x_axis,
    mean = norm_fit$estimate["mean"],
    sd   = norm_fit$estimate["sd"]
  )

  # -----------------------
  # Laplace MLE (SciPy equivalent)
  # -----------------------
  nll_laplace <- function(par, x) {
    mu <- par[1]
    sigma <- par[2]
    if (sigma <= 0) return(1e12)

    -sum(
      extraDistr::dlaplace(
        x,
        mu = mu,
        sigma = sigma,
        log = TRUE
      )
    )
  }

  lap_fit <- optim(
    par = c(mean(samples), sd(samples)),
    fn = nll_laplace,
    x = samples,
    method = "Nelder-Mead"
  )

  lap_pdf <- extraDistr::dlaplace(
    x_axis,
    mu = lap_fit$par[1],
    sigma = lap_fit$par[2]
  )

  # -----------------------
  # Student-t MLE (SciPy equivalent)
  # -----------------------
  nll_t <- function(par, x) {
    df <- par[1]
    mu <- par[2]
    sigma <- par[3]

    if (df <= 1 || sigma <= 0) return(1e12)

    -sum(
      dt(
      (x - mu) / sigma,
      df = df,
      log = TRUE
      ) - log(sigma)
    )
  }

  t_fit <- optim(
    par = c(5, mean(samples), sd(samples)),
    fn = nll_t,
    x = samples,
    method = "Nelder-Mead"
  )

  df_t <- t_fit$par[1]
  mu_t <- t_fit$par[2]
  sd_t <- t_fit$par[3]

  t_pdf <- dt((x_axis - mu_t) / sd_t, df = df_t) / sd_t

  # -----------------------
  # Data frame for curves
  # -----------------------
  df_pdf <- data.frame(
    x = x_axis,
    gaussian = norm_pdf,
    student_t = t_pdf,
    laplace = lap_pdf
  )

  # -----------------------
  # Plot
  # -----------------------
  ggplot(df_hist, aes(x = x)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = bins,
      fill = "steelblue",
      color = "steelblue",
      alpha = 0.6
    ) +
    geom_line(data = df_pdf, aes(y = gaussian), color = "black", linewidth = 1) +
    geom_line(data = df_pdf, aes(y = student_t), color = "red", linetype = "dotdash", linewidth = 1) +
    geom_line(data = df_pdf, aes(y = laplace), color = "blue", linetype = "dotted", linewidth = 1) +
    coord_cartesian(xlim = range_xaxis, ylim = range_yaxis) +
    scale_x_continuous(breaks = seq(range_xaxis[1], range_xaxis[2], by = 5)) +
    scale_y_continuous(breaks = seq(range_yaxis[1], range_yaxis[2], length.out = 5)) +
    labs(x = "x", y = "p(x)") +
    theme_minimal()
}

# -----------------------
# Run two versions
# -----------------------
p1 <- plot_outlier_effect_r(outliers = NULL)

p2 <- plot_outlier_effect_r(outliers = c(8, 9, 10), outlier_pos = 0)

# -----------------------
# Combine
# -----------------------
p1 + p2
