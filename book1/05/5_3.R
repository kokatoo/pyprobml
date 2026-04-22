library(tidyverse)

# data
delta <- 1.5

df <- tibble(
  err = seq(-3, 3, length.out = 60)
) |>
  mutate(
    L1 = abs(err),
    L2 = err^2,
    ind = abs(err) <= delta,
    huber = if_else(
      ind,
      0.5 * err^2,
      delta * abs(err) - (delta^2 / 2)
    ),
    vapnik = if_else(
      ind,
      0,
      abs(err) - delta
    )
  ) |>
  select(-ind) |>
  pivot_longer(
    -err,
    names_to = "loss",
    values_to = "value"
  )

# plot
ggplot(
  df,
  aes(x = err, y = value, col = loss)
) +
  geom_line(size = 1) +
  coord_cartesian(
    ylim = c(-0.5, 5)
  ) +
  labs(linetype = NULL) +
  theme_minimal()
