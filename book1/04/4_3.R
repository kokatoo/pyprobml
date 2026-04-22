library(tidyverse)

ema <- function(y, beta) {
  n <- length(y)

  z <- 0
  zs <- numeric(n)

  for (i in seq_len(n)) {
    z <- beta * z + (1 - beta) * y[i]

    zs[i] <- z
  }

  zs
}

ema_debiased <- function(y, beta) {
  n <- length(y)

  z <- 0
  zs <- numeric(n)

  for (i in seq_len(n)) {
    z <- beta * z + (1 - beta) * y[i]

    zs[i] <- z / (1 - beta^i)
  }

  zs
}

set.seed(0)

n <- 50
x <- seq(0, by = pi, length.out = n)
y <- cos(x) * exp(x / 100) - 10 * exp(-0.01 * x)

betas <- c(0.9, 0.99)

plots <- list()

for (i in seq_along(betas)) {

  beta <- betas[i]

  df <- tibble(
    x = x,
    y = y,
    ema = ema(y, beta),
    ema_debiased = ema_debiased(y, beta)
  ) |>
    pivot_longer(-x, names_to = "series", values_to = "value")

  plots[[i]] <- ggplot(df, aes(x, value, colour = series)) +
    geom_line(linewidth = 1) +
    geom_point(data = df |> filter(series == "y"), size = 2) +
    theme_minimal() +
    labs(title = paste0("beta = ", beta), colour = NULL)
}

plots[[1]] + plots[[2]]
