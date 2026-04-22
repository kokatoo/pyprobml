library(tidyverse)

N <- 10
thetas <- c(0.25, 0.5, 0.75, 0.9)
x <- 0:N

make_graph <- function(x, n, theta) {
  df <- tibble(
    x = x,
    p = dbinom(x, size = n, prob = theta)
  )

  ggplot(df, aes(x, p)) +
    geom_col() +
    scale_x_continuous(breaks = x) +
    coord_cartesian(
      xlim = c(min(x) - 0.5, max(x) + 0.5),
      ylim = c(0, 0.4)
    ) +
    labs(
      x = "x",
      y = "p(x)",
      title = paste0("theta = ", theta)
    ) +
    theme_minimal()
}

# generate plots
plots <- lapply(thetas, function(theta) {
  make_graph(x, N, theta)
})

plots
