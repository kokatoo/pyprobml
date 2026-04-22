library(tidyverse)

rdirichlet <- function(n, alpha) {
  # k = number of components (categories)
  # α = (α1, ..., αk)
  k <- length(alpha)

  # Step 1: generate Gamma random variables
  # Y_ij ~ Gamma(α_j, 1),  for i = 1,...,n and j = 1,...,k
  # total draws = n * k
  mat <- matrix(
    rgamma(n * k, shape = alpha, rate = 1),
    nrow = n,
    byrow = TRUE
  )
  # now each row i looks like:
  # (Y_i1, Y_i2, ..., Y_ik)

  # Step 2: normalize each row
  # T_i = sum_{j=1}^k Y_ij
  # X_ij = Y_ij / T_i
  # ⇒ (X_i1, ..., X_ik) lies on simplex and
  #    (X_i1, ..., X_ik) ~ Dirichlet(α)
  mat / rowSums(mat)
}

make_dir_sample_fig <- function(alpha) {

  alphas <- rep(alpha, num_categories)

  samples <- rdirichlet(num_samples, alphas)

  X <- 1:num_categories

  df <- as_tibble(samples) |>
    mutate(sample = row_number()) |>
    pivot_longer(
      -sample,
      names_to = "category",
      values_to = "value"
    ) |>
    mutate(
      category = as.integer(gsub("V", "", category))
    )

  ggplot(df, aes(category, value)) +
    geom_col() +
    facet_wrap(~sample, ncol = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(breaks = X) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    theme_minimal() +
    labs(
      title = paste0("Samples from Dir(alpha = ", alpha, ")"),
      x = NULL,
      y = NULL
    )
}

set.seed(0)

num_samples <- 10
num_categories <- 5

p1 <- make_dir_sample_fig(0.1)
p2 <- make_dir_sample_fig(1.0)

p1# + p2
