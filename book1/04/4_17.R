library(tidyverse)
library(patchwork)

plot_data_r <- function(x, mu_true, title) {
  df <- as_tibble(x) |>
    setNames(c("y1", "y2"))

  mu_df <- tibble(
    y1 = mu_true[1],
    y2 = mu_true[2]
  )

  ggplot(
    df,
    aes(x = y1, y = y2)
  ) +
    geom_point(color = "blue", size = 2) +
    geom_point(
      data = mu_df,
      aes(x = y1, y = y2),
      shape = 4,
      size = 4,
      color = "black",
      stroke = 1.2
    ) +
    coord_cartesian(
      xlim = c(-1, 1),
      ylim = c(-1, 1)
    ) +
    labs(title = title, x = "y1", y = "y2") +
    theme_minimal()
}

make_contour_plot_r <- function(df, title) {

  ggplot(
    df,
    aes(x = x1, y = x2, z = p)
  ) +
    geom_contour(
      aes(color = after_stat(level))
    ) +
    coord_cartesian(
      xlim = c(-1, 1),
      ylim = c(-1, 1)
    ) +
    labs(
      title = title,
      x = "z1",
      y = "z2"
    ) +
    theme_minimal()
}

set.seed(5)

n_points <- 10

# Part A
z <- c(0.5, 0.5)

sigma_data <- 0.1 *
  matrix(
    c(2, 1,
      1, 1),
    nrow = 2,
    byrow = TRUE
  )

data_points <- MASS::mvrnorm(
  n = n_points,
  mu = z,
  Sigma = sigma_data
)

p_data <- plot_data_r(data_points, z, "Data")

# Part B
z_prior_mu <- c(0, 0)
z_prior_Sigma <- 0.1 * diag(2)

grid_seq <- seq(-1, 1, length.out = 100)

grid <- expand_grid(x1 = grid_seq, x2 = grid_seq)

df_prior <- grid |>
  mutate(
    p = mvtnorm::dmvnorm(
      cbind(x1, x2),
      mean = z_prior_mu,
      sigma = z_prior_Sigma
    )
  )

p_prior <- make_contour_plot_r(df_prior, "Prior")

# Part C
S0_inv <- solve(z_prior_Sigma)
S_inv  <- solve(sigma_data)

post_Sigma <- solve(S0_inv + n_points * S_inv)

y_bar <- colMeans(data_points)

post_mu <- post_Sigma %*% (
  n_points * S_inv %*% y_bar +
    S0_inv %*% z_prior_mu
)

df_post <- grid |>
  mutate(
    p = mvtnorm::dmvnorm(
      cbind(x1, x2),
      mean = as.vector(post_mu),
      sigma = post_Sigma
    )
  )

p_post <- make_contour_plot_r(
  df_post,
  paste0("Posterior after ", n_points, " points")
)

p_data | p_prior | p_post
