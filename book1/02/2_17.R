library(tidyverse)

make_graph_r <- function(data) {

  x <- data$x
  a_list <- data$a_list
  b_list <- data$b_list
  props <- data$props

  param_df <- data.frame(
    a = a_list,
    b = b_list,
    prop = props,
    label = sprintf("a=%.1f, b=%.1f", a_list, b_list)
  )

  # build long dataframe
  df <- purrr::map2_dfr(
    a_list,
    b_list,
    function(a, b) {
      data.frame(
        x = x,
        y = dbeta(x, shape1 = a, shape2 = b),
        a = a,
        b = b
      )
    }
  )  |>
    left_join(param_df, by = c("a", "b"))

  df$prop <- rep(props, each = length(x))

  ggplot(df, aes(x = x, y = y, group = interaction(a, b))) +
    geom_line(aes(color = label), linewidth = 1) +
    labs(
      x = "x",
      y = "p(x)",
      title = "Beta distributions",
      color = "params"
    ) +
    theme_minimal()
}

# -----------------------
# Example data (your case)
# -----------------------
x <- seq(0, 1, length.out = 100)

data <- list(
  x = x,
  a_list = c(0.1, 0.1, 1.0, 2.0, 2.0),
  b_list = c(0.1, 1.0, 1.0, 2.0, 8.0),
  props = c("blue", "red", "black", "green", "cyan")
)

make_graph_r(data)

make_graph_gamma_r <- function(data) {

  x <- data$x
  a_list <- data$a_list
  b_list <- data$b_list
  colors <- data$props

  # -----------------------
  # parameter table (one row per curve)
  # -----------------------
  param_df <- tibble(
    a = a_list,
    b = b_list,
    color = colors,
    label = sprintf("a=%.1f, b=%.1f", a_list, b_list)
  )

  # -----------------------
  # expand curves
  # -----------------------
  df <- param_df |>
    mutate(data = map2(a, b, ~ tibble(
      x = x,
      y = dgamma(x, shape = .x, rate = .y)
    ))) |>
    unnest(data)

  # -----------------------
  # plot
  # -----------------------
  ggplot(df, aes(x = x, y = y, group = interaction(a, b))) +
    geom_line(aes(color = label), linewidth = 1) +
    scale_color_manual(
      values = setNames(param_df$color, param_df$label)
    ) +
    labs(
      x = "x",
      y = "p(x)"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

data <- list(
  x = x,
  a_list = c(1.0, 1.5, 2.0, 1.0, 1.5, 2.0),
  b_list = c(1.0, 1.0, 1.0, 2.0, 2.0, 2.0),
  props = c("blue", "red", "black", "green", "cyan", "blue")
)

make_graph_gamma_r(data)
