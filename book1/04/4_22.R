library(tidyverse)
library(patchwork)

# -----------------------------
# data
# -----------------------------
dataset <- c(
  rep(0, 10),
  rep(1, 1)
)
n_samples <- length(dataset)

n_heads <- sum(dataset)
n_tails <- n_samples - n_heads

# -----------------------------
# prior (Beta(1,1))
# -----------------------------
a <- 1
b <- 1

# -----------------------------
# exact posterior Beta
# -----------------------------
apost <- a + n_heads
bpost <- b + n_tails

theta_range <- seq(0.01, 0.99, length.out = 100)

marginal <- choose(n_samples, n_heads) *
  beta(apost, bpost) /
  beta(a, b)

df_plot <- tibble(
  theta = theta_range,
  prior = dbeta(
    theta_range, a, b
  ),
  posterior = dbeta(
    theta_range, apost, bpost
  ),
  likelihood =
    dbinom(
      n_heads,
      size = n_samples,
      prob = theta_range
    ) * marginal
)

# -----------------------------
# plot prior / likelihood / posterior
# -----------------------------
p1 <- ggplot(df_plot) +
  geom_line(
    aes(theta, likelihood),
    color = "red",
    linetype = "dashed"
  ) +
  geom_line(
    aes(theta, posterior),
    color = "green"
  ) +
  geom_line(
    aes(theta, prior),
    color = "black"
  ) +
  theme_minimal() +
  labs(x = "theta", y = "density")

# -----------------------------
# grid approximation
# -----------------------------
n_points_grid <- 20
theta_grid <- seq(0.01, 0.99, length.out = n_points_grid)

likelihood_grid <- dbinom(
  n_heads,
  size = n_samples,
  prob = theta_grid
)
prior_grid <- dbeta(theta_grid, a, b)

grid_unnorm <- likelihood_grid * prior_grid
grid_appx <- grid_unnorm / sum(grid_unnorm)

# grid_appx is prob * delta.
# To get density, we need to divide by delta
delta <- diff(theta_grid)[1]

df_grid <- tibble(
  theta = theta_grid,
  grid = grid_appx,
  grid_density = grid / delta
)

# -----------------------------
# compare grid vs true posterior
# -----------------------------
p2 <- ggplot() +
  geom_line(
    data = df_plot,
    aes(theta, posterior),
    color = "blue"
  ) +
  geom_col(
    data = df_grid,
    aes(theta, grid_density),
    fill = "black",
    alpha = 0.5
  ) +
  theme_minimal() +
  labs(x = "theta", y = "density")
p2

# ----------------------------
# Laplace approximation (MAP + Hessian)
# -----------------------------
neg_log_post <- function(theta) {
  -(
    dbinom(
      n_heads,
      size = n_samples,
      prob = theta,
      log = TRUE
    ) +
      dbeta(theta, a, b, log = TRUE)
  )
}

opt <- optimize(
  neg_log_post,
  interval = c(1e-6, 1 - 1e-6)
)
theta_map <- opt$minimum

hessian <- numDeriv::hessian(
  neg_log_post,
  theta_map
)
scale <- 1 / sqrt(hessian)

df_laplace <- tibble(
  theta = theta_range,
  laplace = dnorm(
    theta_range,
    mean = theta_map,
    sd = scale
  )
)

# -----------------------------
# compare posterior vs Laplace
# -----------------------------
p3 <- ggplot() +
  geom_line(
    data = df_plot,
    aes(theta, posterior),
    color = "blue"
  ) +
  geom_line(
    data = df_laplace,
    aes(theta, laplace),
    color = "orange",
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(x = "theta", y = "density")

# -----------------------------
# Laplace approximation on logit scale
# -----------------------------

# Transform theta to logit (unconstrained)
logit <- function(p) log(p / (1 - p))
inv_logit <- function(alpha) 1 / (1 + exp(-alpha))

# Negative log-posterior on logit scale
# Includes Jacobian: d theta / d alpha = theta * (1 - theta)
neg_log_post_logit <- function(alpha) {
  theta <- inv_logit(alpha)
  log_jacobian <- log(theta * (1 - theta))  # |d theta / d alpha|

  -(
    dbinom(
      n_heads,
      size = n_samples,
      prob = theta,
      log = TRUE
    ) +
      dbeta(theta, a, b, log = TRUE) +
      log_jacobian
  )
}

# Find mode on logit scale
opt_logit <- optimize(
  neg_log_post_logit,
  interval = c(-10, 10)
)
alpha_map <- opt_logit$minimum

# Hessian on logit scale (second derivative)
hessian_logit <- numDeriv::hessian(
  neg_log_post_logit,
  alpha_map
)
scale_logit <- 1 / sqrt(hessian_logit)

# Laplace approximation on theta scale (transform back)
# Method: sample from Gaussian on alpha, then map to theta
set.seed(42)
alpha_samples <- rnorm(
  5000,
  mean = alpha_map,
  sd = scale_logit
)
theta_samples <- inv_logit(alpha_samples)

jacobian <- theta_range * (1 - theta_range)

# Density of transformed samples (kernel density estimate)
df_laplace_logit <- tibble(
  theta = theta_range,
  laplace_logit = density(
    theta_samples,
    from = 0,
    to = 1,
    n = length(theta_range)
  )$y,
  laplace_logit_direct = dnorm(
    alpha_map,
    mean = alpha_map,
    sd = scale_logit
  ) * (1 / jacobian)
)

# Compare original Laplace (theta-space) vs logit-Laplace
p4 <- ggplot() +
  geom_line(
    data = df_plot,
    aes(theta, posterior),
    color = "blue",
    linewidth = 1
  ) +
  geom_line(
    data = df_laplace,
    aes(theta, laplace),
    color = "orange",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_line(
    data = df_laplace_logit,
    aes(theta, laplace_logit),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_line(
    data = df_laplace_logit,
    aes(theta, laplace_logit_direct),
    color = "blue",
    linetype = "dotted",
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    x = "theta", y = "density",
    title = "Laplace approximations",
    subtitle = "Blue: exact posterior | Orange: Laplace on theta | Red: Laplace on logit"
  )
p4

(p1 + p2) /
  (p3 + p4)
