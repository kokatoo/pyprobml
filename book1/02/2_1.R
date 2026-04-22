library(tidyverse)

plot_discrete <- function(probs) {
  df <- tibble(
    x = seq_along(probs),
    p = probs
  )

  ggplot(df, aes(x = x, y = p)) +
    geom_col(width = 0.6) +
    scale_x_continuous(breaks = df$x) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "x", y = "Pr(X = x)") +
    theme_minimal()
}

N <- 4

uniform_probs <- rep(1 / N, N)
plot_discrete(uniform_probs)

delta_probs <- c(1, 0, 0, 0)
plot_discrete(delta_probs)
