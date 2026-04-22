library(patchwork)
library(tidyverse)

# -----------------------------
# 1. Data
# -----------------------------
set.seed(43)

w0 <- 0.125
b0 <- 5.0
x_range <- c(-20, 60)

s_fun <- function(x) {
  g <- (x - x_range[1]) / (x_range[2] - x_range[1])
  3 * (0.25 + g^2)
}

load_dataset <- function(n = 150, n_tst = 150) {

  x <- runif(n, x_range[1], x_range[2])
  eps <- rnorm(n) * s_fun(x)
  y <- (w0 * x * (1 + sin(x)) + b0) + eps

  x_tst <- seq(
    x_range[1],
    x_range[2],
    length.out = n_tst
  )

  list(
    df = tibble(x = x, y = y),
    grid = tibble(x = x_tst)
  )
}

dat <- load_dataset()
df <- dat$df
grid <- dat$grid

# -----------------------------
# 2. Fixed variance model
# -----------------------------
nll_fixed <- function(par, x, y) {
  b <- par[1]
  w <- par[2]

  mu <- b + w * x

  -sum(dnorm(y, mean = mu, sd = 1, log = TRUE))
}

res_fixed <- optim(
  par = c(0, 0),
  fn = nll_fixed,
  x = df$x,
  y = df$y,
  method = "BFGS"
)

b_hat_f <- res_fixed$par[1]
w_hat_f <- res_fixed$par[2]

grid_fixed <- grid |>
  mutate(
    mu = b_hat_f + w_hat_f * x,
    sd = 1,
    upper = mu + 2 * sd,
    lower = mu - 2 * sd
  )

# -----------------------------
# 3. Heteroskedastic model
# -----------------------------
softplus <- function(z) log1p(exp(z))

nll_hetero <- function(par, x, y) {
  b  <- par[1]
  w  <- par[2]
  bp <- par[3]
  wp <- par[4]

  mu <- b + w * x
  sigma <- 1e-3 + softplus(0.05 * bp + wp * x)

  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

res_hetero <- optim(
  par = rep(0, 4),
  fn = nll_hetero,
  x = df$x,
  y = df$y,
  method = "BFGS"
)

b_hat  <- res_hetero$par[1]
w_hat  <- res_hetero$par[2]
bp_hat <- res_hetero$par[3]
wp_hat <- res_hetero$par[4]

grid_hetero <- grid |>
  mutate(
    mu = b_hat + w_hat * x,
    sd = 1e-3 + softplus(0.05 * bp_hat + wp_hat * x),
    upper = mu + 2 * sd,
    lower = mu - 2 * sd
  )

# -----------------------------
# 4. Plot
# -----------------------------
# -----------------------------
# Fixed variance plot
# -----------------------------
p_fixed <- ggplot() +
  geom_point(data = df, aes(x, y), color = "blue", alpha = 0.6) +
  geom_line(data = grid_fixed, aes(x, mu), color = "red", linewidth = 1.2) +
  geom_line(data = grid_fixed, aes(x, upper), color = "green", linewidth = 0.8) +
  geom_line(data = grid_fixed, aes(x, lower), color = "green", linewidth = 0.8) +
  labs(
    title = "Fixed Variance (σ = 1)",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

# -----------------------------
# Heteroskedastic plot
# -----------------------------
p_hetero <- ggplot() +
  geom_point(data = df, aes(x, y), color = "blue", alpha = 0.6) +
  geom_line(data = grid_hetero, aes(x, mu), color = "red", linewidth = 1.2) +
  geom_line(data = grid_hetero, aes(x, upper), color = "green", linewidth = 0.8) +
  geom_line(data = grid_hetero, aes(x, lower), color = "green", linewidth = 0.8) +
  labs(
    title = "Heteroskedastic σ(x)",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

# -----------------------------
# Combine side-by-side
# -----------------------------
p_fixed | p_hetero
