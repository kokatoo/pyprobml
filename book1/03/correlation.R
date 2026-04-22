library(patchwork)
library(tidyverse)

# Core functions
rotate <- function(theta) {
  matrix(
    c(cos(theta), sin(theta),
      -sin(theta), cos(theta)),
    nrow = 2
  )
}

scale_matrix <- function(aspect) {
  matrix(
    c(1, 0,
      0, aspect),
    nrow = 2
  )
}

generate_points <- function(n, r) {
  points <- matrix(nrow = 0, ncol = 2)

  while (nrow(points) < n) {
    p <- runif(2, -r, r)

    if (sqrt(sum(p^2)) <= r) {
      points <- rbind(points, p)
    }
  }

  points
}

transform_points <- function(points, theta, aspect) {
  T_matrix <- rotate(theta) %*%
    scale_matrix(aspect)

  t(
    apply(
      points,
      1,
      function(p) (T_matrix %*% p)[, 1]
    )
  )
}

correlation_plot <-
  function(theta, aspect, n = 300, r = 4) {
    pts <- generate_points(n, r)

    transformed <- transform_points(
      pts, theta, aspect
    )

    df <- tibble(
      x = transformed[, 1],
      y = transformed[, 2]
    )

    model <- lm(y ~ x, data = df)

    df$y_pred <- predict(model)

    cor_val <- cor(df$x, df$y)
    cov_val <- cov(df$x, df$y)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(
        alpha = 0.5, size = 1.5,
        color = "steelblue"
      ) +
      geom_line(
        aes(y = y_pred),
        color = "red", linewidth = 0.8
      ) +
      coord_fixed() +
      labs(
        title = paste(
          "θ =", round(theta, 2),
          "| aspect =", aspect
        ),
        subtitle = paste0(
          "Correlation = ",
          round(cor_val, 3),
          "\n",
          "Beta = ",
          round(coef(model)[2], 2),
          "\n",
          "Cov(x, y) / Var(x) = ",
          round(cov_val / var(df$x), 2)
        )
      ) +
      theme_minimal() +
      xlim(-6, 6) + ylim(-6, 6)
  }

p1 <- correlation_plot(theta = 0, aspect = 1)
p2 <- correlation_plot(theta = pi/4, aspect = 0.5)
p3 <- correlation_plot(theta = pi/3, aspect = 0.3)
p4 <- correlation_plot(theta = pi/2, aspect = 1)

(p1 + p2) / (p3 + p4)

