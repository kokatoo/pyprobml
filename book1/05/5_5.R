library(tidyverse)
library(patchwork)

poly_design <- function(x, deg) {
  sapply(
    0:deg,
    function(p) x^p
  ) |>
    as.matrix()
}

generate_data <- function(N, seed) {
  set.seed(seed)

  x_train <- runif(N, 0, 10)
  y_train <- (x_train - 4)^2 + 5 * rnorm(N)

  x_true <- seq(-2, 12.1, length.out = 100)
  y_true <- (x_true - 4)^2

  list(
    x_train = x_train,
    y_train = y_train,
    x_true = x_true,
    y_true = y_true
  )
}

empirical_bayes <- function(X, y) {

  fn <- function(params) {
    -log_p_D_M(
      X, y,
      exp(params[1]), exp(params[2])
    )
  }

  opt <- optim(c(0, 0), fn)

  list(
    alpha = exp(opt$par[1]),
    beta = exp(opt$par[2])
  )
}

# p(D|M) = ∫ p(D|θ,M) p(θ|M) dθ
log_p_D_M <- function(X, y, alpha, beta) {
  N <- nrow(X)
  M <- ncol(X)

  m0 <- rep(0, M)                 # prior mean
  S0_inv <- alpha * diag(M)       # prior precision

  SN_inv <- S0_inv + beta * t(X) %*% X   # posterior precision
  SN <- solve(SN_inv)                    # posterior covariance
  mN <- SN %*% (S0_inv %*% m0 + beta * t(X) %*% y)  # posterior mean

  resid <- y - X %*% mN            # residuals at posterior mean
  logdetSN <- determinant(
    SN,
    logarithm = TRUE
  )$modulus

  as.numeric((
    M * log(alpha) +               # prior contribution
      N * log(beta) -              # likelihood precision
        beta * t(resid) %*% resid -  # data fit
        M * log(2 * pi) -            # constant
        alpha * t(mN) %*% mN +       # prior at posterior mean
        logdetSN                     # posterior volume
  ) / 2)
}

generate_plots <- function(N) {
  dat <- generate_data(N, 1)

  x_train <- dat$x_train
  y_train <- dat$y_train
  x_true <- dat$x_true
  y_true <- dat$y_true

  plots <- list()
  log_p_D_M_vals <- c()

  for (deg in deg_values) {
    X_train <- poly_design(x_train, deg)
    X_train <- poly(x_train, deg, raw = TRUE)

    eb <- empirical_bayes(X_train, y_train)

    log_p_D_M_val <- log_p_D_M(
      X_train,
      y_train,
      eb$alpha,
      eb$beta
    )

    log_p_D_M_vals <- c(
      log_p_D_M_vals,
      log_p_D_M_val
    )

    X_true <- poly_design(x_true, deg)
    X_true <- poly(x_true, deg, raw = TRUE)

    # posterior: p(θ|D) ∝ p(D|θ) p(θ)
    m0 <- rep(0, ncol(X_train))
    S0_inv <- eb$alpha * diag(ncol(X_train))
    SN_inv <- S0_inv + eb$beta * t(X_train) %*% X_train
    SN <- solve(SN_inv)
    mN <- SN %*%
      (S0_inv %*% m0 + eb$beta * t(X_train) %*% y_train)

    y_mean <- as.vector(X_true %*% mN)
    y_std <- sqrt(
      1/eb$beta + rowSums((X_true %*% SN) * X_true)
    )

    df <- tibble(
      x = x_true,
      mean = y_mean,
      upper = y_mean + 2*y_std,
      lower = y_mean - 2*y_std
    )

    p <- ggplot(
      df,
     aes(x = x)
    ) +
      geom_line(aes(y = upper), linetype = "dotted") +
      geom_line(aes(y = lower), linetype = "dotted") +
      geom_line(aes(y = mean), color = "red") +
      geom_point(
        data = tibble(
          x = x_train,
          y = y_train
        ),
        aes(x, y),
        inherit.aes = FALSE
      ) +
      geom_line(
        data = tibble(
          x = x_true,
          y = y_true
        ),
        aes(x, y),
        color = "green"
      ) +
      ggtitle(
        paste0("deg=", deg, " | log p(D|M)=", round(log_p_D_M_val, 2))
      ) +
      theme_minimal()

    plots[[deg]] <- p
  }

  # p(M|D) ∝ p(D|M) * p(M), with uniform p(M)
  # p(M) = 1/3
  p_M_given_D <- exp(log_p_D_M_vals)
  p_M_given_D <- p_M_given_D /
    sum(p_M_given_D)

  pbar <- tibble(
    deg = deg_values,
    prob = p_M_given_D
  ) |>
    ggplot(aes(x = factor(deg), y = prob)) +
    geom_col() +
    theme_minimal()

  plots <- c(plots, list(pbar))
  plots
}

alpha <- 0.01
beta <- 0.1
N_values <- c(5, 30)
deg_values <- c(1, 2, 3)

wrap_plots(generate_plots(N_values[1]))
wrap_plots(generate_plots(N_values[2]))
