library(tidyverse)

## 5.10 (a)
# constants
sigma <- 1.5
xmin <- -4
xmax <- 8
ymin <- 0
ymax <- 0.3
res <- 0.01
xstar <- 2.3

# domain
x <- seq(xmin, xmax, by = res)

# densities (note: sd = sigma)
y1 <- dnorm(x, mean = 0, sd = sigma)
y2 <- dnorm(x, mean = 4, sd = sigma)

df <- tibble(
  x = x,
  y1 = y1,
  y2 = y2
)

# shading regions
x1 <- seq(xstar, xmax, by = res)
y1_shade <- dnorm(x1, mean = 0, sd = sigma)

x2 <- seq(xmin, xstar, by = res)
y2_shade <- dnorm(x2, mean = 4, sd = sigma)

df1 <- tibble(x = x1, y = y1_shade)
df2 <- tibble(x = x2, y = y2_shade)

ggplot() +
  geom_line(data = df, aes(x, y1), color = "blue") +
  geom_line(data = df, aes(x, y2), color = "red") +
  geom_area(data = df1, aes(x, y), alpha = 0.5) +
  geom_area(data = df2, aes(x, y), alpha = 0.5) +
  geom_vline(xintercept = xstar) +
  annotate(
    "text",
    x = xstar,
    y = -0.005,
    label = "X*",
    size = 6
  ) +
  annotate(
    "segment",
    x = 0,
    y = 0.07,
    xend = 1.2,
    yend = 0.02,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "segment",
    x = 4,
    y = 0.07,
    xend = 2.8,
    yend = 0.02,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = -0.4,
    y = 0.07,
    label = "beta",
    size = 5
  ) +
  annotate(
    "text",
    x = 4,
    y = 0.07,
    label = "alpha",
    size = 5
  ) +
  annotate(
    "text",
    x = -0.2,
    y = 0.28,
    label = "H0",
    size = 5
  ) +
  annotate(
    "text",
    x = 3.8,
    y = 0.28,
    label = "H1",
    size = 5
  ) +
  coord_cartesian(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

## 5.10 (b)
pi_val <- pi

# domain
x <- seq(-pi_val, pi_val, by = 0.01)

# functions
y1 <- -cos(x) + 0.03
y2 <- -cos(2 * x / 3)

df <- tibble(
  x = x,
  y1 = y1,
  y2 = y2
)

ggplot(
  df,
  aes(x = x)
) +
  geom_line(aes(y = y1), color = "blue") +
  geom_line(aes(y = y2), color = "red") +
  geom_segment(
    aes(x = -0.3, xend = 0.3, y = -1.025, yend = -1.025)
  ) +
  annotate(
    "segment",
    x = 0,
    y = -2.5,
    xend = 0,
    yend = 2,
    arrow = arrow(length = unit(0.25, "cm"))
  ) +
  annotate(
    "segment",
    x = -pi_val,
    y = -1.5,
    xend = pi_val,
    yend = -1.5,
    arrow = arrow(length = unit(0.25, "cm")
                  )) +
  annotate(
    "text",
    x = -0.3,
    y = 2.2,
    label = "1 - beta",
    size = 5
  ) +
  annotate(
    "text",
    x = -0.3,
    y = 0.5,
    label = "1",
    size = 5
  ) +
  annotate(
    "text",
    x = -0.3,
    y = -1.3,
    label = "alpha",
    size = 5
  ) +
  annotate(
    "text",
    x = -0.34,
    y = -1.8,
    label = "theta0",
    size = 4
  ) +
  annotate(
    "text",
    x = 3.1,
    y = -1.8,
    label = "theta",
    size = 5
  ) +
  annotate(
    "text",
    x = 2.5,
    y = -0.2,
    label = "A",
    size = 5
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 1.1,
    label = "B",
    size = 5
  ) +
  coord_cartesian(
    ylim = c(-2, 2.5)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
