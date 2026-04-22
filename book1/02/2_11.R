library(tidyverse)

# -----------------------------
# Data (petal width, binary target)
# -----------------------------
data(iris)
iris <- as_tibble(iris)

df <- iris |>
  mutate(
    x = Petal.Width,
    y = if_else(Species == "virginica", 1, 0)
  )  |>
  select(x, y, everything())

# -----------------------------
# Fit logistic regression
# -----------------------------
model <- glm(y ~ x, data = df, family = binomial())

# -----------------------------
# Prediction grid
# -----------------------------
x_new <- tibble(
  x = seq(0, 3, length.out = 1000)
)

y_prob <- predict(
  model,
  newdata = x_new,
  type = "response"
)

x_new <- x_new |>
  mutate(
    p1 = y_prob,
    p0 = 1 - y_prob
  )

# -----------------------------
# Decision boundary (p = 0.5)
# -----------------------------
decision_boundary <- x_new |>
  filter(p1 > 0.5) |>
  slice(1) |>
  pull(x)

# -----------------------------
# Plot
# -----------------------------
ggplot() +
  geom_point(
    data = df |> filter(y == 0),
    aes(x = x, y = y),
    shape = 15
  ) +
  geom_point(
    data = df |> filter(y == 1),
    aes(x = x, y = y),
    shape = 17
  ) +
  geom_line(
    data = x_new,
    aes(x = x, y = p1),
    color = "green"
  ) +
  geom_line(
    data = x_new,
    aes(x = x, y = p0),
    linetype = "dashed",
    color = "blue"
  ) +
  geom_vline(
    xintercept = decision_boundary,
    linetype = "dotdash"
  ) +
  labs(
    x = "Petal width (cm)",
    y = "Probability"
  ) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-0.02, 1.02)) +
  theme_minimal()


ggplot() +
  geom_point(
    data = df |> filter(y == 0),
    aes(x = x, y = y, color = "Not Virginica"),
    shape = 15
  ) +
  geom_point(
    data = df |> filter(y == 1),
    aes(x = x, y = y, color = "Virginica"),
    shape = 17
  ) +
  geom_line(
    data = x_new,
    aes(x = x, y = p1, color = "Virginica"),
    linewidth = 1
  ) +
  geom_line(
    data = x_new,
    aes(x = x, y = p0, color = "Not Virginica"),
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = decision_boundary,
    linetype = "dotdash",
    color = "black"
  ) +
  geom_segment(
    aes(
      x = decision_boundary,
      xend = decision_boundary - 0.3,
      y = 0.08,
      yend = 0.08,
      color = "Not Virginica"
    ),
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_segment(
    aes(
      x = decision_boundary,
      xend = decision_boundary + 0.3,
      y = 0.92,
      yend = 0.92,
      color = "Virginica"
    ),
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  scale_color_manual(
    values = c("Virginica" = "green", "Not Virginica" = "blue")
  ) +
  labs(
    x = "Petal width (cm)",
    y = "Probability",
    color = NULL
  ) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-0.02, 1.02)) +
  theme_minimal()


## 2 Features

train_x <- iris |>
  transmute(
    x1 = Petal.Length,
    x2 = Petal.Width,
    y = if_else(Species == "virginica", 1L, 0L)
  )

# -----------------------------
# Fit logistic regression (2D)
# -----------------------------
model <- glm(
  y ~ x1 + x2,
  data = train_x,
  family = binomial()
)

# -----------------------------
# Grid for contour
# -----------------------------
x1_seq <- seq(2.9, 7, length.out = 500)
x2_seq <- seq(0.8, 2.7, length.out = 200)

grid <- expand_grid(x1 = x1_seq, x2 = x2_seq)

grid$prob <- predict(
  model,
  newdata = grid,
  type = "response"
)

grid_mat <- matrix(
  grid$prob,
  nrow = length(x1_seq),
  byrow = FALSE
)

# -----------------------------
# Decision boundary (p = 0.5)
# -----------------------------
# Logistic regression model:
# logit(p) = b0 + b1*x1 + b2*x2
#
# Decision boundary occurs when:
# p = 0.5  ⟹  logit(p) = 0
# so:
# b0 + b1*x1 + b2*x2 = 0
#
# Solve for x2:
# x2 = -(b0 + b1*x1) / b2

b <- coef(model)

# choose a range of x1 values to draw the boundary line
x1_boundary <- c(2.9, 7)

# compute corresponding x2 values on decision boundary
x2_boundary <- -(b[1] + b[2] * x1_boundary) / b[3]

# -----------------------------
# Plot
# -----------------------------
ggplot() +
  geom_point(
    data = train_x |> filter(y == 0),
    aes(x1, x2),
    shape = 15,
    color = "blue"
  ) +
  geom_point(
    data = train_x |> filter(y == 1),
    aes(x1, x2),
    shape = 17,
    color = "green"
  ) +
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = prob, color = after_stat(level)),
    bins = 10,
    linewidth = 0.6
  ) +
  scale_color_gradient(low = "blue", high = "green") +
  geom_segment(
    aes(
      x = x1_boundary[1],
      xend = x1_boundary[2],
      y = x2_boundary[1],
      yend = x2_boundary[2]
    ),
    linetype = "dashed"
  ) +
  annotate("text", x = 3.75, y = 1.70, label = "Not Virginica", color = "blue") +
  annotate("text", x = 6.5, y = 1.5, label = "Virginica", color = "green") +
  coord_cartesian(xlim = c(2.9, 7), ylim = c(0.8, 2.7)) +
  labs(
    x = "Petal length (cm)",
    y = "Petal width (cm)"
  ) +
  theme_minimal()

