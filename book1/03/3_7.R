library(mvtnorm)
library(tidyverse)
library(patchwork)

gauss_impute_r <- function(mu, Sigma, X) {

  X_imp <- X
  n <- nrow(X)
  d <- ncol(X)

  for (i in 1:n) {

    x <- X[i, ]

    hidden <- which(is.na(x))
    visible <- which(!is.na(x))

    if (length(hidden) == 0) next

    x_v <- x[visible]

    mu_v <- mu[visible]
    mu_h <- mu[hidden]

    Sigma_vv <- Sigma[visible, visible, drop = FALSE]
    Sigma_hv <- Sigma[hidden, visible, drop = FALSE]

    # conditional mean
    mu_cond <- mu_h +
      Sigma_hv %*% solve(Sigma_vv) %*% (x_v - mu_v)

    X_imp[i, hidden] <- as.vector(mu_cond)
  }

  X_imp
}

# -----------------------
# Hinton plot
# -----------------------
plot_hinton_data_r <-
  function(data,
           title,
           show_y = TRUE,
           max_weight = 7) {

    df <- as_tibble(data) |>
      mutate(row = row_number()) |>
      pivot_longer(
        -row,
        names_to = "col",
        values_to = "value"
      ) |>
      mutate(
        col = as.numeric(gsub("V", "", col)),
        size = sqrt(abs(value) / max_weight),
        fill = ifelse(value > 0, "white", "black")
      )

    ggplot(df, aes(x = col, y = row)) +
      geom_tile(
        aes(
          width = size,
          height = size,
          fill = fill
        ),
        color = "grey50"
      ) +
      scale_fill_identity() +
      scale_y_reverse() +
      coord_fixed() +
      labs(
        title = title,
        x = "Sample Number",
        y = if (show_y) "Dimension of data" else NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.y = if (show_y) element_text() else element_blank(),
        axis.ticks.y = if (show_y) element_line() else element_blank()
      )
  }

# -----------------------
# Data generation + imputation
# -----------------------
gen_imputer_data_r <-
  function(data_dim = 8,
           sample_size = 10,
           threshold_missing = 0.5) {

    mean <- rnorm(data_dim)

    # random SPD covariance
    A <- matrix(
      rnorm(data_dim^2),
      data_dim,
      data_dim
    )
    cov <- t(A) %*% A

    # sample true data
    x_true <- mvtnorm::rmvnorm(
      sample_size,
      mean = mean,
      sigma = cov
    )

    # missing mask
    missing_idx <- matrix(
      runif(sample_size * data_dim) < threshold_missing,
      nrow = sample_size,
      ncol = data_dim
    )

    x_observed <- x_true
    x_observed[missing_idx] <- NA

    x_imputed <- gauss_impute_r(mean, cov, x_observed)

    # replace NA with 0 for plotting
    x_observed0 <- x_observed
    x_observed0[is.na(x_observed0)] <- 0

    p1 <- plot_hinton_data_r(x_observed0, "Observed data matrix", TRUE)
    p2 <- plot_hinton_data_r(x_true, "True data matrix", FALSE)
    p3 <- plot_hinton_data_r(x_imputed, "Imputed data matrix", FALSE)

    p1 + p2 + p3
  }

# -----------------------
# Run
# -----------------------
gen_imputer_data_r()
