library(tidyverse)

zero_one <- function(x) as.numeric(x <= 0)
hinge <- function(x) pmax(0, 1 - x)
log_loss <- function(x) log2(1 + exp(-x))
exp_loss <- function(x) exp(-x)

x <- seq(-2, 2, by = 0.01)

df <- tibble(x = x) |>
  mutate(
    `0-1 loss` = zero_one(x),
    `hinge loss` = hinge(x),
    `log loss` = log_loss(x),
    `exp loss` = exp_loss(x)
  ) |>
  pivot_longer(
    -x,
    names_to = "loss",
    values_to = "value"
  )

ggplot(
  df,
  aes(x, value, col = loss)
) +
  geom_line(linewidth = 1) +
  coord_cartesian(
    xlim = c(-2.1, 2.1),
    ylim = c(-0.1, 3.1)
  ) +
  theme_minimal()
