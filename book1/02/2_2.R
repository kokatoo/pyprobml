library(tidyverse)

make_graph <- function(data) {
  df <- tibble(
    x = data$x,
    y = data$y
  )

  ggplot(df, aes(x, y)) +
    geom_line(linewidth = 1) +
    labs(
      x = "x",
      y = data$ylabel
    ) +
    theme_minimal()


}

make_graph(list(
  x = x,
  y = pnorm(x),
  ylabel = "Pr(X ≤ x)"
))

x <- seq(-3, 3, length.out = 100)

# pdf
df <- tibble(
  x = x,
  y = dnorm(x)
)

# quantiles
x_sep_left  <- qnorm(0.025)
x_sep_right <- qnorm(0.975)

# regions to fill
df_left <- df |> filter(x <= x_sep_left)
df_right <- df |> filter(x >= x_sep_right)

ggplot(df, aes(x, y)) +
  geom_line(linewidth = 1) +

  # shaded tails
  geom_area(data = df_left, fill = "blue", alpha = 0.4) +
  geom_area(data = df_right, fill = "blue", alpha = 0.4) +

  # vertical markers
  geom_vline(xintercept = x_sep_left, linetype = "dashed") +
  geom_vline(xintercept = x_sep_right, linetype = "dashed") +

  annotate("text", x = -2.5, y = 0.2, label = expression(alpha / 2)) +
  annotate("text", x = 2.5, y = 0.2, label = expression(alpha / 2)) +

  scale_x_continuous(
    breaks = c(x_sep_left, 0, x_sep_right),
    labels = c(
      expression(Phi^{-1}*(alpha/2)),
      "0",
      expression(Phi^{-1}*(1 - alpha/2))
    )
  ) +

  coord_cartesian(ylim = c(0, 0.5)) +

  labs(x = "x", y = "p(x)") +
  theme_minimal()

