library(plotly)
library(tidyverse)

plot_3D <- function(p) {
  htmlwidgets::saveWidget(
    p,
    "plot.html",
    selfcontained = FALSE
  )

  browseURL("plot.html")
}

# simplex vertices
pts <- matrix(
  c(1, 0, 0,
    0, 1, 0,
    0, 0, 1),
  ncol = 3,
  byrow = TRUE
)

df_tri <- tibble(
  x = pts[,1],
  y = pts[,2],
  z = pts[,3]
)

# edges of simplex
edges <- tibble(
  x = c(1,0,0,1,0,0),
  y = c(0,1,0,0,1,0),
  z = c(0,0,1,0,0,1),
  xend = c(0,0,1,0,0,1),
  yend = c(1,0,0,0,0,0),
  zend = c(0,1,0,0,1,0)
)

# arrows
arrows <- tibble(
  x = c(0,0,0),
  y = c(0,0,0),
  z = c(0,0,0),
  xend = c(1.4,0,0),
  yend = c(0,1.4,0),
  zend = c(0,0,1.4),
  label = c("theta1", "theta2", "theta3")
)

fig <- plot_ly()

# triangle surface
fig <- fig |>
  add_trace(
    type = "mesh3d",
    x = df_tri$x,
    y = df_tri$y,
    z = df_tri$z,
    i = c(0),
    j = c(1),
    k = c(2),
    opacity = 0.3
  )

# arrows
for (i in 1:nrow(arrows)) {
  fig <- fig |>
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = c(arrows$x[i], arrows$xend[i]),
      y = c(arrows$y[i], arrows$yend[i]),
      z = c(arrows$z[i], arrows$zend[i]),
      line = list(width = 5)
    )
}

# labels
fig <- fig |>
  add_trace(
    type = "scatter3d",
    mode = "text",
    x = c(1, 0, 0),
    y = c(0, 1, 0),
    z = c(0, 0, 1),
    text = c("θ1", "θ2", "θ3"),
    textposition = "top center"
  )

fig |> layout(
  scene = list(
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    zaxis = list(visible = FALSE)
  )
)

## simpler way to plot simplex
V <- matrix(
  c(1, 0, 0,
    0, 1, 0,
    0, 0, 1),
  ncol = 3,
  byrow = TRUE
)

fig <- plot_ly()

fig <- fig |>
  add_trace(
    type = "mesh3d",
    x = V[,1],
    y = V[,2],
    z = V[,3],
    i = c(0),
    j = c(1),
    k = c(2),
    opacity = 0.3
  )

plot_3D(fig)

#### 4.14 (b)

grid_size <- 100
edge_distance <- 0.005

# values used for v1 and v2 grid
grid_values <- seq(0, 1, length.out = grid_size)

# simplex vertices slightly pulled away from edges
vertex1 <- c(1 - 2 * edge_distance, edge_distance, edge_distance)
vertex2 <- c(edge_distance, 1 - 2 * edge_distance, edge_distance)
vertex3 <- c(edge_distance, edge_distance, 1 - 2 * edge_distance)

ddirichlet <- function(x, alpha) {
  eps <- 1e-12
  x <- pmax(x, eps)

  # log Dirichlet density:
  # log Γ(sum α) − sum log Γ(α_i) + sum (α_i − 1) log x_i
  log_density <-
    lgamma(sum(alpha)) -
    sum(lgamma(alpha)) +
    sum((alpha - 1) * log(x))

  exp(log_density)
}

# map (v1, v2) → point on simplex, then evaluate density
dpdf <- function(v1, v2, alpha) {

  # outside simplex
  if ((v1 + v2) > 1) {
    return(NA_real_)
  }

  # barycentric combination of vertices
  theta <- v1 * vertex1 +
    v2 * vertex2 +
    (1 - v1 - v2) * vertex3

  ddirichlet(theta, alpha)
}

plot_dist <- function(alpha) {

  density_matrix <- matrix(
    NA_real_,
    nrow = grid_size,
    ncol = grid_size
  )

  for (row in 1:grid_size) {
    for (col in 1:grid_size) {

      v1 <- grid_values[row]
      v2 <- grid_values[col]

      density_matrix[row, col] <- dpdf(v1, v2, alpha)
    }
  }

  plot_ly(
    x = ~grid_values,
    y = ~grid_values,
    z = ~density_matrix,
    type = "surface"
  ) |>
    layout(
      title = paste(alpha, collapse = ", "),
      scene = list(
        xaxis = list(title = "v1"),
        yaxis = list(title = "v2"),
        zaxis = list(title = "density")
      )
    )
}

alphas <- list(
  c(20, 20, 20),
  c(3, 3, 20),
  c(0.1, 0.1, 0.1)
)

plot_3D(fig)
plot_3D(plot_dist(alphas[[1]]))
plot_3D(plot_dist(alphas[[2]]))
plot_3D(plot_dist(alphas[[3]]))

plot_3D(plot_dist(c(1, 1, 1)))
plot_3D(plot_dist(c(2, 2, 2)))
plot_3D(plot_dist(c(20, 20, 20)))
plot_3D(plot_dist(c(3, 3, 20)))
