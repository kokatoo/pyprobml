library(tidyverse)

theta <- seq(0, 1, length.out = 10000)

H <- function(theta) {
  -(theta * log2(theta) +
      (1 - theta) * log2(1 - theta))
}

# avoid NaN at 0 and 1
H_vals <- H(theta)
H_vals[is.nan(H_vals)] <- 0

df <- tibble(theta = theta, H = H_vals)

ggplot(
  df,
  aes(x = theta, y = H)
) +
  geom_line(color = "blue", linewidth = 1.2) +
  coord_cartesian(
    xlim = c(-0.01, 1.01),
    ylim = c(0, 1.01)
  ) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1)
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1)
  ) +
  labs(
    x = "p(X = 1)",
    y = "H(X)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
