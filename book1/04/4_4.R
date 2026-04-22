library(patchwork)
library(tidyverse)

## A function that generates a random covariance matrix (and its inverse) with the given condition number
# and with the given first axis a (unnormalised). Returns (Sigma, Lambda).
covcond <- function(cond_number, A) {
  A <- as.numeric(A)

  A[1] <- A[1] + sign(A[2]) * sqrt(sum(A^2))

  Z <- diag(length(A)) - 2 * (A %*% t(A)) / sum(A^2)

  e <- rev(
    sort(
      1 / seq(
        cond_number,
        1, length.out = length(A)
      )
    )
  )

  Sigma <- Z %*% diag(e) %*% t(Z)
  Lambda <- Z %*% diag(1 / e) %*% t(Z)

  list(
    Sigma = Sigma,
    Lambda = Lambda
  )
}

set.seed(42)

D <- 50
cond_number <- 10

A <- matrix(rnorm(D), ncol = 1)

res <- covcond(cond_number, A)
Sigma <- res$Sigma

evals_true <- eigen(Sigma)$values |>
                         sort(decreasing = TRUE)

fractions <- c(2, 1, 0.5)

plots <- list()

for (i in seq_along(fractions)) {

  n <- as.integer(fractions[i] * D)

  X <- MASS::mvrnorm(
    n,
    mu = rep(0, D),
    Sigma = Sigma
  )

  # R cov() has correction bias
  S_mle <- cov(X) * (n - 1) / n
  evals_mle <- eigen(S_mle)$values |>
                          sort(decreasing = TRUE)

  lambda <- 0.9

  S_shrink <- lambda * diag(diag(S_mle)) +
    (1 - lambda) * S_mle

  evals_shrink <- eigen(S_shrink)$values |>
                                sort(decreasing = TRUE)

  df <- tibble(
    idx = rep(1:D, 3),
    value = c(evals_true, evals_mle, evals_shrink),
    type = rep(c("true", "MLE", "MAP"), each = D)
  )

  plots[[i]] <- ggplot(
    df,
    aes(idx, value, colour = type)
  ) +
    geom_line(linewidth = 1) +
    coord_cartesian(ylim = c(0, 1.5)) +
    theme_minimal() +
    labs(
      title = paste0("N=", n, ", D=", D),
      x = NULL,
      y = "eigenvalue"
    )
}

plots[[1]] + plots[[2]] + plots[[3]]
