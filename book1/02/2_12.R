library(tidyverse)

# -----------------------------
# Softmax distribution (ggplot version)
# -----------------------------

plot_softmax_distribution <- function(T, a, softmax_fn) {

  df <- map_dfr(T, function(Ti) {
    tibble(
      logits = seq_along(a),
      prob = softmax_fn(a / Ti),
      T = factor(Ti)
    )
  })

  ggplot(df, aes(x = factor(logits), y = prob)) +
    geom_col() +
    facet_wrap(~ T, nrow = 1) +
    labs(
      x = "logits (a)",
      y = "S(a | T)"
    ) +
    ylim(0, 1) +
    theme_minimal()
}

softmax <- function(a) {
  e <- exp(a)
  e / sum(e)
}

# Example
T_array <- c(100, 2, 1)
a <- c(3, 0, 1)

plot_softmax_distribution(T_array, a, softmax)

lse <- function(a) {
  m <- max(a)
  m + log(sum(exp(a - m)))
}

softmax_stable <- function(a) {
  exp(a - lse(a))
}


plot_softmax_distribution(T_array, a, softmax_stable)
