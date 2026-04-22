library(patchwork)
library(tidyverse)

make_graph_r <- function(data) {

  x <- data$x
  a_list <- data$a_list
  b_list <- data$b_list
  props <- data$props

  param_df <- tibble(
    a = a_list,
    b = b_list,
    prop = props,
    label = sprintf(
      "a=%.1f, b=%.1f",
      a_list, b_list
    )
  )

  # build long dataframe
  df <- purrr::map2_dfr(
    a_list,
    b_list,
    function(a, b) {
      tibble(
        x = x,
        y = dbeta(x, shape1 = a, shape2 = b),
        a = a,
        b = b
      )
    }
  )  |>
    left_join(param_df, by = c("a", "b"))

  df$prop <- rep(props, each = length(x))

  ggplot(
    df,
    aes(x = x, y = y, group = interaction(a, b))
  ) +
    geom_line(
      aes(color = label), linewidth = 1
    ) +
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
    label = sprintf(
      "a=%.1f, b=%.1f",
      a_list, b_list
    )
  )

  # -----------------------
  # expand curves
  # -----------------------
  df <- param_df |>
    mutate(
      data = map2(
        a, b,
        ~ tibble(
          x = x,
          y = dgamma(x, shape = .x, rate = .y)
        )
      )
    ) |>
    unnest(data)

  ggplot(
    df,
    aes(x = x, y = y, group = interaction(a, b))
  ) +
    geom_line(
      aes(color = label),
      linewidth = 1
    ) +
     scale_color_manual(
      values = c(
        setNames(param_df$color, param_df$label),
        setNames("orange", "half-cauchy")
      )
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

make_graph_distributions <-
  function(data, palette_name = "Set1") {

    x <- data$x

    dinvgamma <- function(x, shape, scale) {
      ifelse(
        x <= 0,
        0,
        (scale^shape / gamma(shape)) *
          x^(-shape - 1) * exp(-scale / x)
      )
    }

    # Half-Cauchy: scale = sigma
    dhalfcauchy <- function(x, scale = 1) {
      ifelse(
        x < 0,
        0,
        (2 / scale) * dt(x / scale, df = 1)
      )
    }

    param_df <- tibble(
      dist = data$dist,
      param1 = data$param1,
      param2 = data$param2,
      label = data$label
    )

    n_colors <- nrow(param_df)

    if (n_colors < 3) {
      colors <- c("red", "blue", "green")[1:n_colors]
    } else {
      colors <- brewer.pal(n_colors, palette_name)
    }

    param_df$color <- colors
    df <- param_df |>
      mutate(
        data = pmap(
          list(dist, param1, param2),
          function(dist, p1, p2) {
            tibble(
              x = x,
              y = case_when(
                dist == "gamma" ~
                  dgamma(x, shape = p1, rate = p2),
                dist == "exponential" ~
                  dexp(x, rate = p1),
                dist == "chisq" ~
                  dchisq(x, df = p1),
                dist == "invgamma" ~
                  dinvgamma(x, shape = p1, scale = p2),
                dist == "halfcauchy"
                ~ dhalfcauchy(x, scale = p1),
                TRUE ~ NA_real_
              )
            )
          }
        )
      ) |>
      unnest(data)

    color_values <- setNames(param_df$color, param_df$label)

    ggplot(df, aes(x = x, y = y, color = label)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = color_values) +
      labs(
        x = "x",
        y = "density",
        title = paste(
          "Distribution Comparison (Color palette:",
          palette_name, ")"
        )
      ) +
      theme_minimal() +
      theme(legend.title = element_blank())
  }

x <- seq(0.01, 5, length.out = 500)
## Exponential and Chi-Sq are both exponential distributions
data <- list(
  x = x,
  dist = c("gamma", "gamma", "exponential", "chisq"),
  param1 = c(1, 3/2, 1, 3),
  param2 = c(1, 1/2, NA, NA),
  label = c(
    "Gamma(1,1)",
    "Gamma(3/2,1/2)",
    "Exp(rate=1)",
    "Chi-square(df=3)"
  )
)
make_graph_distributions(data, palette_name = "Set2")

## Half-Cauchy makes a better prior for variance as IG give low prob for low variances
x <- seq(0.01, 5, length.out = 500)
data <- list(
  x = x,
  dist = c("invgamma", "halfcauchy"),
  param1 = c(2, 1),
  param2 = c(1, NA),
  label = c(
    "Inverse-Gamma(2,1)",
    "Half-Cauchy(scale=1)"
  )
)
make_graph_distributions(data, palette_name = "Set2")

## Gamma and IG behave differently at 0 and right tail
x <- seq(0.01, 5, length.out = 500)
data <- list(
  x = x,
  dist = c("invgamma", "gamma"),
  param1 = c(2, 3),
  param2 = c(3, 2),
  label = c(
    "Inverse-Gamma(2,1)",
    "Gamma(2,1)"
  )
)
make_graph_distributions(data, palette_name = "Set2")
