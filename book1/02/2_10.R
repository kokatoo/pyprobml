library(tidyverse)

# -----------------------------
# Activation functions
# -----------------------------
sigmoid <- function(x) 1 / (1 + exp(-x))
relu <- function(x) pmax(0, x)
heaviside <- function(x) as.numeric(x > 0)
softplus <- function(x) log(1 + exp(x))
lrelu <- function(x, lambda = 0.1) pmax(lambda * x, x)
elu <- function(x, alpha = 1) ifelse(
  x < 0,
  alpha * (exp(x) - 1),
  x
)
swish <- function(x) x * sigmoid(x)

# -----------------------------
# Grid
# -----------------------------
x <- seq(-5, 5, length.out = 200)

# -----------------------------
# Sigmoid
# -----------------------------
tibble(x = x) |>
  mutate(y = sigmoid(x)) |>
  ggplot(aes(x, y)) +
  geom_line(linewidth = 1) +
  labs(title = "Sigmoid function", x = "a", y = "σ(a)") +
  theme_minimal()

# -----------------------------
# Heaviside
# -----------------------------
tibble(x = x) |>
  mutate(y = heaviside(x)) |>
  ggplot(aes(x, y)) +
  geom_line(linewidth = 1) +
  labs(title = "Heaviside function", x = "a", y = "I(a > 0)") +
  theme_minimal()

# -----------------------------
# Sigmoid vs Tanh vs ReLU
# -----------------------------
tibble(x = x) |>
  mutate(
    sigmoid = sigmoid(x),
    tanh = tanh(x),
    relu = relu(x)
  ) |>
  pivot_longer(-x, names_to = "fn", values_to = "y") |>
  ggplot(aes(x, y, color = fn)) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-1.2, 1.2)) +
  labs(title = "Activation functions", x = "x", y = "output") +
  theme_minimal()

# -----------------------------
# ReLU family
# -----------------------------
tibble(x = x) |>
  mutate(
    relu = relu(x),
    lrelu = lrelu(x),
    elu = elu(x),
    swish = swish(x)
  ) |>
  pivot_longer(-x, names_to = "fn", values_to = "y") |>
  ggplot(aes(x, y, color = fn)) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1.2, 2)) +
  labs(title = "Activation functions", x = "x", y = "output") +
  theme_minimal()

# -----------------------------
# Sigmoid saturation plot
# -----------------------------
tibble(x = seq(-5, 5, length.out = 200)) |>
  mutate(y = sigmoid(x)) |>
  ggplot(aes(x, y)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_vline(xintercept = 0) +
  annotate("text", x = 3.5, y = 0.7, label = "Saturating") +
  annotate("text", x = -3.5, y = 0.3, label = "Saturating") +
  annotate("text", x = 2, y = 0.2, label = "Linear") +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-0.2, 1.2)) +
  labs(title = "Sigmoid activation function", x = "x", y = "σ(x)") +
  theme_minimal()
