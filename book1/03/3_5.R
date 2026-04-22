library(plotly)
library(mvtnorm)
library(tidyverse)
library(patchwork)

plot_surface <- function(d, title) {

  z_mat <- matrix(
    d$z,
    nrow = length(grid_vals),
    byrow = TRUE
  )

  plot_ly(
    x = grid_vals,
    y = grid_vals,
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

means <- c(0, 0)

cov_list <- list(
  Full = matrix(
    c(2, 1.8,
      1.8, 2),
    2, 2,
    byrow = TRUE
  ),
  Diagonal = matrix(
    c(1, 0,
      0, 3),
    2, 2,
    byrow = TRUE
  ),
  Spherical = matrix(
    c(1, 0,
      0, 1),
    2, 2,
    byrow = TRUE
  )
)

grid_vals <- seq(-5, 5, length.out = 100)

grid <- expand.grid(x = grid_vals, y = grid_vals)

df <- lapply(
  names(cov_list),
  function(g) {
    grid |>
      mutate(
        z = mvtnorm::dmvnorm(
          cbind(x, y),
          mean = means,
          sigma = cov_list[[g]]
        ),
        type = g
      )
  }
) |>
  bind_rows()

p_list <- df |>
  split(df$type) |>
  lapply(function(d) {
    ggplot(d, aes(x = x, y = y, z = z)) +
      geom_contour_filled() +
      coord_fixed() +
      labs(
        title = unique(d$type),
        x = "y1",
        y = "y2"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

p_full <- plot_surface(
  filter(df, type == "Full"),
  "Full"
)
p_diag <- plot_surface(
  filter(df, type == "Diagonal"),
  "Diagonal"
)
p_sph  <- plot_surface(
  filter(df, type == "Spherical"),
  "Spherical"
)

(p_list[[1]] + p_list[[2]] + p_list[[3]]) +
  plot_layout(guides = "collect")

plot_3D(p_full)
plot_3D(p_diag)
plot_3D(p_sph)
