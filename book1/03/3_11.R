library(plotly)
library(tidyverse)
library(patchwork)

# -----------------------
# ellipse
# -----------------------
sigmaEllipse2D <-
  function(mu, Sigma, level = 1, npoints = 128) {

  phi <- seq(0, 2 * pi, length.out = npoints)

  circle <- rbind(cos(phi), sin(phi))

  A <- level * sqrtm(Sigma)

  pts <- A %*% circle

  tibble(
    x = pts[1, ] + mu[1],
    y = pts[2, ] + mu[2],
    level = level
  )
}

# matrix square root
sqrtm <- function(Sigma) {
  sv <- svd(Sigma)
  sv$u %*% diag(sqrt(sv$d)) %*% t(sv$u)
}

plot_sigma_vector <- function(Mu, Sigma) {

  levels <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)

  df_all <- tibble()

  for (i in seq_along(Mu)) {

    mu <- Mu[[i]]
    S  <- Sigma[[i]]

    for (l in levels) {

      pts <- sigmaEllipse2D(mu, S, l)

      df_tmp <- pts |>
        mutate(
          comp = paste0("comp_", i),
          level = l
        )

      df_all <- bind_rows(df_all, df_tmp)
    }
  }

  df_all |>
    ggplot(aes(x = x, y = y)) +
    geom_path(
      aes(group = interaction(comp, level),
          color = comp),
      linewidth = 1
    ) +
    geom_point(
      data = df_all |> filter(is.na(level)),
      aes(color = comp),
      size = 3,
      shape = 4
    ) +
    coord_equal() +
    theme_minimal()
}

plot_surface <- function(d, x, y, title) {

  z_mat <- matrix(
    d$z,
    nrow = length(x),
    byrow = TRUE
  )

  plot_ly(
    x = x,
    y = y,
    z = z_mat
  ) |>
    add_surface() |>
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "y1"),
        yaxis = list(title = "y2"),
        zaxis = list(title = "p(y1,y2)")
      )
    )
}

plot_3D <- function(p) {
  htmlwidgets::saveWidget(
    p,
    "plot.html",
    selfcontained = FALSE
  )

  browseURL("plot.html")
}

# -----------------------
# Gaussian mixture surface
# -----------------------
make_gaussian_mixture <-
  function(Mu, Sigma, weights, grid) {
    Z <- rep(0, nrow(grid))

    for (i in seq_along(Mu)) {
      dens <- mvtnorm::dmvnorm(
        cbind(grid$x, grid$y),
        mean = Mu[[i]],
        sigma = Sigma[[i]]
      )

      Z <- Z + weights[i] * dens
    }

    df <- grid |>
      mutate(z = Z)
  }

# -----------------------
# inputs
# -----------------------
mu_1 <- c(0.22, 0.45)
mu_2 <- c(0.5, 0.5)
mu_3 <- c(0.77, 0.55)

Mu <- list(mu_1, mu_2, mu_3)

Sigma1 <- matrix(
  c(0.011, -0.01,
    -0.01, 0.018),
  2
)

Sigma2 <- matrix(
  c(0.018, 0.01,
    0.01, 0.011),
  2
)

Sigma3 <- Sigma1

Sigma <- list(Sigma1, Sigma2, Sigma3)

weights <- c(0.5, 0.3, 0.2)

plot_sigma_vector(Mu, Sigma)

x = seq(0, 1, 0.01)
y = seq(0, 1, 0.01)
grid <- expand_grid(x = x, y = y)

df <- make_gaussian_mixture(Mu, Sigma, weights, grid)

ggplot(df, aes(x = x, y = y, fill = z)) +
  ## geom_raster() +
  geom_contour(aes(z = z, color = after_stat(level))) +
  coord_equal() +
  theme_minimal()

Z_mat <- matrix(
  df$z,
  nrow = length(x),
  ncol = length(y),
  byrow = FALSE
)

p <- plot_ly(
  x = x,
  y = y,
  z = Z_mat
) |>
  add_surface() |>
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    )
  )
plot_3D(p)
