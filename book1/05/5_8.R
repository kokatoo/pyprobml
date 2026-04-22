library(tidyverse)
library(patchwork)

make_plot <- function(n) {

  mus <- seq(-1.8, 1.8, by = 0.1)

  n0 <- 1
  n0B <- 5
  mu0 <- 0

  r1 <- rep(1 / n, length(mus))
  r2 <- rep(pi / (2 * n), length(mus))
  r3 <- (mus - mu0)^2
  r4 <- (n + n0)^(-2) * (n + n0^2 * (mu0 - mus)^2)
  r5 <- (n + n0B)^(-2) * (n + n0B^2 * (mu0 - mus)^2)

  df <- tibble(
    mu = mus,
    mle = r1,
    median = r2,
    fixed = r3,
    postmean1 = r4,
    postmean5 = r5
  ) |>
    pivot_longer(
      -mu,
      names_to = "method",
      values_to = "risk"
    )

  p <- ggplot(
    df,
    aes(mu, risk, color = method)
  ) +
    geom_line() +
    scale_color_manual(
      values = c(
        mle = "blue",
        median = "red",
        fixed = "black",
        postmean1 = "limegreen",
        postmean5 = "cyan"
      )) +
    scale_linetype_manual(
      values = c(
        mle = "solid",
        median = "dotted",
        fixed = "dotdash",
        postmean1 = "dashed",
        postmean5 = "solid"
      )) +
    scale_x_continuous(
      breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)
    ) +
    ggtitle(
      paste0("risk function for n=", n)
    ) +
    theme_minimal()

  if (n == 5) {
    p <- p +
      coord_cartesian(ylim = c(0, 0.5))
  } else {
    p <- p +
      coord_cartesian(ylim = c(0, 0.18))
  }

  p
}

wrap_plots(
  make_plot(5),
  make_plot(20)
)
