library(tidyverse)
library(patchwork)

make_graph_r <- function(df, x_col, y_col, groupby_col) {

  # -----------------------
  # overall correlation
  # -----------------------
  overall_r <- cor(df[[x_col]], df[[y_col]])
  overall_label <- sprintf(
    "All %s (R = %.2f)",
    groupby_col, overall_r
  )

  # -----------------------
  # left plot (overall)
  # -----------------------
  p1 <- df |>
    ggplot(aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(alpha = 0.7, size = 2, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      x = x_col,
      y = y_col,
      title = overall_label
    ) +
    theme_minimal()

  # -----------------------
  # group correlations
  # -----------------------
  df_group <- df |>
    group_by(.data[[groupby_col]]) |>
    summarize(
      R = cor(.data[[x_col]], .data[[y_col]]),
      .groups = "drop"
    ) |>
    mutate(
      label = sprintf(
        "%s (R = %.2f)", .data[[groupby_col]], R
      )
    )

  df2 <- df |>
    left_join(
      df_group,
      by = setNames(groupby_col, groupby_col)
    )

  # -----------------------
  # right plot (by group)
  # -----------------------
  p2 <- df2 |>
    ggplot(aes(x = .data[[x_col]], y = .data[[y_col]], color = label)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = x_col,
      y = NULL,
      color = groupby_col
    ) +
    theme_minimal()

  p1 + p2
}

df <- iris |>
  as_tibble() |>
  rename(
    Species = Species,
    Sepal_Length = Sepal.Length,
    Sepal_Width  = Sepal.Width
  )


make_graph_r(
  df,
  x_col = "Sepal_Length",
  y_col = "Sepal_Width",
  groupby_col = "Species"
)
