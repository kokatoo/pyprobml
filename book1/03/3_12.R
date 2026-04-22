library(mclust)
library(ggforce)
library(tidyverse)

set.seed(42)

X1a <- MASS::mvrnorm(
  500,
  mu = c(4, -4),
  Sigma = diag(2)
)

X1b <- MASS::mvrnorm(
  500,
  mu = c(0, 0),
  Sigma = diag(2)
)

X1 <- rbind(X1a, X1b)

# linear transformation (rotation/shear/scale)
A <- matrix(
  c(0.374, 0.732,
    0.95,  0.598),
  2,
  byrow = TRUE
)

X1 <- X1 %*% A

centers <- matrix(
  c(-4, 1,
    -4, 3,
    -4, -2),
  byrow = TRUE,
  ncol = 2
)

X2 <- do.call(
  rbind,
  lapply(1:3, function(i) {
    MASS::mvrnorm(
      1000,
      mu = centers[i, ],
      Sigma = diag(2) * 0.25
    )
  }
  )
)

X <- rbind(X1, X2) |>
  as_tibble() |>
  setNames(c("x1", "x2"))

ggplot(X, aes(x1, x2)) +
  geom_point(size = 0.8) +
  coord_equal() +
  theme_minimal()

# -----------------------------
# GMM FIT (full)
# -----------------------------
K <- 5
X_2 <- X
gm <- Mclust(X_2, G = K, modelNames = "VVV")

X_2$cluster <- factor(gm$classification)

# -----------------------------
# GRID PDF (like score_samples)
# -----------------------------

size <- 50
grid <- expand.grid(
  x1 = seq(min(X$x1) - 1, max(X$x1) + 1, length.out = size),
  x2 = seq(min(X$x2) - 1, max(X$x2) + 1, length.out = size)
)

dens <- predict(gm, grid, what = "dens")

grid <- grid |>
  bind_cols(as_tibble(dens$z))

grid_long <- grid |>
  pivot_longer(
    cols = -c(x1, x2),
    names_to = "cluster",
    values_to = "density"
  )

# -----------------------------
# ELLIPSES + CLUSTERS
# -----------------------------
ggplot(X, aes(x1, x2, color = cluster)) +
  geom_point(size = 0.8) +
  stat_ellipse(level = 0.68) +
  stat_ellipse(level = 0.95) +
  coord_equal() +
  theme_minimal()

# -----------------------------
# MULTI MODELS
# -----------------------------
gm_full <- Mclust(X, G = K, modelNames = "VVV")
gm_tied <- Mclust(X, G = K, modelNames = "EEE")
gm_spherical <- Mclust(X, G = K, modelNames = "EII")
gm_diag <- Mclust(X, G = K, modelNames = "VVI")

plot_model <- function(model, name) {

  X2 <- X
  X2$cluster <- factor(model$classification)

  p1 <- ggplot(X2, aes(x1, x2, color = cluster)) +
    geom_point(size = 0.8) +
    stat_ellipse(level = 0.68) +
    stat_ellipse(level = 0.95) +
    coord_equal() +
    ggtitle(name) +
    theme_minimal()

  print(p1)
}

get_ellipse <- function(mu, Sigma, level = 0.68, n = 100) {

  eig <- eigen(Sigma)

  a <- sqrt(eig$values[1] * qchisq(level, df = 2))
  b <- sqrt(eig$values[2] * qchisq(level, df = 2))

  theta <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])

  t <- seq(0, 2 * pi, length.out = n)

  tibble(
    x1 = mu[1] + a * cos(t) * cos(theta) - b * sin(t) * sin(theta),
    x2 = mu[2] + a * cos(t) * sin(theta) + b * sin(t) * cos(theta)
  )
}

plot_model <- function(model, name) {

  X2 <- X
  X2$cluster <- factor(model$classification)

  mu <- model$parameters$mean
  Sigma <- model$parameters$variance

  K <- model$G

  ell_68 <- lapply(
    1:K,
    function(k) {
      get_ellipse(mu[, k], Sigma$sigma[, , k], level = 0.68) |>
        mutate(cluster = factor(k))
    }
  ) |>
    bind_rows()

  ell_95 <- lapply(
    1:K,
    function(k) {
      get_ellipse(mu[, k], Sigma$sigma[, , k], level = 0.95) |>
        mutate(cluster = factor(k))
    }
  ) |>
    bind_rows()

  p1 <- ggplot(X2, aes(x1, x2, color = cluster)) +
    geom_point(size = 0.8) +
    geom_path(data = ell_68, aes(x1, x2, group = cluster), linewidth = 0.8) +
    geom_path(data = ell_95, aes(x1, x2, group = cluster), linewidth = 0.8) +
    coord_equal() +
    ggtitle(name) +
    theme_minimal()

  print(p1)
}

plot_model(gm_full, "full")
plot_model(gm_tied, "tied")
plot_model(gm_spherical, "spherical")
plot_model(gm_diag, "diag")

# -----------------------------
# BIC over K
# -----------------------------

Ks <- 2:8
mods <- lapply(Ks, function(k) Mclust(X, G = k))

bic <- sapply(mods, BIC)

tibble(K = Ks, BIC = bic) |>
  ggplot(aes(K, BIC)) +
  geom_line() +
  geom_point() +
  theme_minimal()
