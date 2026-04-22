library(tidyverse)
library(patchwork)

# -----------------------
# Data + simulation
# -----------------------
x_samples <- seq(-1, 1, length.out = 200)

lower_limit <- -1
upper_limit <- 1

px_uniform <- rep(
  1 / (upper_limit - lower_limit),
  length(x_samples)
)

square_fn <- function(x) x^2
y <- square_fn(x_samples)

# analytic density (approx as in your code)
y_pdf <- 1 / (2 * sqrt(y + 1e-2))

# Monte Carlo
set.seed(1)
n <- 1000
uniform_samples <- runif(
  n,
  min = lower_limit,
  max = upper_limit
)
fn_samples <- square_fn(uniform_samples)

# -----------------------
# Plot 1: Uniform
# -----------------------
p1 <- tibble(x = x_samples, p = px_uniform) |>
  ggplot(aes(x = x, y = p)) +
  geom_line() +
  labs(
    title = "Uniform distribution",
    x = "x",
    y = "p(x)"
  ) +
  theme_minimal()

# -----------------------
# Plot 2: Analytic transform
# -----------------------
p2 <- tibble(y = y, py = y_pdf) |>
  ggplot(aes(x = y, y = py)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Analytical p(y), y(x) = x^2",
    x = "y",
    y = "p(y)"
  ) +
  theme_minimal()

df_hist <- tibble(y = y)
df_pdf  <- tibble(y = y, pdf = y_pdf)

# -----------------------
# Plot 3: Monte Carlo
# -----------------------
p3 <- df_hist |>
  ggplot(aes(x = y)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    color = "black",
    fill = "grey80"
  ) +
  geom_line(
    data = df_pdf,
    aes(x = y, y = pdf),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Monte Carlo approximation",
    x = "y",
    y = "Frequency"
  ) +
  theme_minimal()

# -----------------------
# Combine
# -----------------------
p1 + p2 + p3
