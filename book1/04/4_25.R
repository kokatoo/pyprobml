library(glmnet)
library(tidyverse)
library(patchwork)

set.seed(42)

# Draw n samples from a Gaussian N(mu, sigma) by transforming
# standard normal noise (Z ~ N(0, I)) using Cholesky: X = mu + LZ
gauss_sample <- function(mu, sigma, n) {

  # Z ~ N(0, I)
  Z <- matrix(
    rnorm(n * length(mu)),
    nrow = length(mu),
    ncol = n
  )

  # -----------------------------
  # Cholesky factor
  # -----------------------------
  # L such that L L^T = sigma
  L <- t(chol(sigma))

  # -----------------------------
  # transform + shift
  # -----------------------------
  # X = mu + L Z  ~ N(mu, sigma)
  X <- L %*% Z

  # add mean to each column  X <- sweep(X, 1, mu, "+")

  return(X)
}

# -----------------------------
# Gaussian sampler using mvtnorm
# -----------------------------
# Draws n samples from N(mu, sigma)
gauss_sample2 <- function(mu, sigma, n) {

  # rmvnorm returns samples in rows: n x d
  X <- mvtnorm::rmvnorm(
    n = n,
    mean = mu,
    sigma = sigma
  )

  # transpose to match original shape: d x n
  t(X)
}

# Ridge regression solves:
# w = (X^T X + sqrt(lambda) I)^(-1) X^T y
# i.e. minimizes: ||Xw - y||^2 + sqrt(lambda) ||w||^2
ridge <- function(X, y, lambda) {
  solve(
    t(X) %*% X + sqrt(lambda) * diag(ncol(X)),
    t(X) %*% y
  )
}

# -----------------------------
# basis expansion (RBF)
# -----------------------------
basis_expansion <-
  function(X, s = NULL, centers = NULL) {

  n_basis <- 25

  if (is.null(s)) {
    s <- sd(X) / sqrt(n_basis)
  }

  if (is.null(centers)) {
    centers <- X[-1]
  }

  X <- matrix(X, ncol = 1)

  Xbasis <- matrix(
    1,
    nrow = nrow(X),
    ncol = n_basis
  )

  for (i in 2:n_basis) {
    Xbasis[, i] <- exp(-(1 / (2 * s^2)) *
                         (X - centers[i - 1])^2)
  }

  list(
    Xbasis = Xbasis,
    s = s,
    centers = centers
  )
}

fun <- function(X) {
  cos(2 * pi * X)
}

synthesize_data <- function(n, d) {

  sigma <- matrix(0.1)
  mu <- 0

  X <- runif(n * d)
  X <- matrix(X, nrow = n)

  y_mean <- fun(X)
  noise <- as.vector(
    gauss_sample(mu, sigma, n)
  )

  y <- as.vector(y_mean) + noise

  list(X = X, y = y)
}

# -----------------------------
# settings
# -----------------------------
n <- 25
d <- 1
lambdas <- c(exp(5), exp(-5))
n_data_sets <- 100
show_n_sets <- 20

domain <- seq(0, 1, length.out = 200)

plots <- list()

# -----------------------------
# main loop
# -----------------------------
for (lam_i in seq_along(lambdas)) {

  lambda <- lambdas[lam_i]

  yhat <- matrix(
    0,
    nrow = length(domain),
    ncol = n_data_sets
  )

  for (j in 1:n_data_sets) {

    dat <- synthesize_data(n, d)
    X <- dat$X
    y <- dat$y

    bp <- basis_expansion(X)
    Xb <- bp$Xbasis

    W <- ridge(Xb, y, lambda)

    pred_bp <- basis_expansion(
      domain,
      bp$s,
      bp$centers
    )

    Xtest <- pred_bp$Xbasis

    yhat[, j] <- as.vector(Xtest %*% W)

    ## sigma <- n_basis / (2 * sd(X)^2)
    ## rbf <- kernlab::rbfdot(sigma = sigma)  # you can tune sigma later

    ## # -----------------------------
    ## # training kernel matrix
    ## # -----------------------------
    ## Xb <- kernlab::kernelMatrix(rbf, as.matrix(X), as.matrix(X))

    ## W <- ridge(Xb, y, lambda)

    ## # -----------------------------
    ## # test kernel matrix
    ## # -----------------------------
    ## Xtest <- kernlab::kernelMatrix(
    ##   rbf,
    ##   as.matrix(X),
    ##   as.matrix(domain)
    ## )

    ## yhat[, j] <- as.vector(t(Xtest) %*% W)
  }

  # -----------------------------
  # left panel (sample functions)
  # -----------------------------
  df_lines <- as_tibble(yhat[, 1:show_n_sets]) |>
    mutate(x = domain) |>
    pivot_longer(
      -x,
      names_to = "id",
      values_to = "y"
    )

  p_left <- ggplot(
    df_lines,
    aes(x, y, group = id)
  ) +
    geom_line(color = "orange", alpha = 0.4) +
    coord_cartesian(
      xlim = c(-0.1, 1.1),
      ylim = c(-1.5, 1.5)
    ) +
    labs(
      title = paste0("log(lambda) = ", log(lambda))
    ) +
    theme_minimal()

  # -----------------------------
  # right panel (mean + truth)
  # -----------------------------
  df_mean <- tibble(
    x = domain,
    truth = fun(domain),
    mean = rowMeans(yhat)
  )

  p_right <- ggplot(
    df_mean,
    aes(x)
  ) +
    geom_line(
      aes(y = truth),
      linewidth = 1.2
    ) +
    geom_line(
      aes(y = mean),
      linetype = "dashed",
      linewidth = 1.2
    ) +
    labs(
      title = paste0("log(lambda) = ", log(lambda))
    ) +
    theme_minimal()

  plots[[lam_i]] <- p_left + p_right
}

wrap_plots(plots, ncol = 1)
