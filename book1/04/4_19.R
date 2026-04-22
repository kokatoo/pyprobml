library(tidyverse)
library(patchwork)

gaussProb <- function(X, mu, Sigma) {

  d <- 1

  X <- matrix(X, ncol = d)
  mu <- matrix(mu, ncol = d)

  # -----------------------------
  # Centering: (x - μ)
  # -----------------------------
  X <- X -
    matrix(
      mu,
      nrow = nrow(X),
      ncol = d,
      byrow = TRUE
    )

  # -----------------------------
  # Quadratic term (Gaussian exponent)
  # -----------------------------
  # This corresponds to:
  #   -0.5 * (x - μ)^2 / σ^2
  logp <- -0.5 *
    rowSums((X / Sigma) * X)

  # -----------------------------
  # Normalization constant
  # -----------------------------
  # This corresponds to:
  #   log(sqrt(2πσ^2))
  # = 0.5 * log(2π) + 0.5 * log(σ^2)
  logZ <- (d / 2) * log(2 * pi) +
    0.5 * log(Sigma)

  # -----------------------------
  # Final log-density
  # -----------------------------
  exp(logp - logZ)
}

# mixture function
f <- function(x) {
  gaussProb(x, 0, 1) + gaussProb(x, 6, 1)
}

domain <- seq(-4, 10, by = 0.001)

df <- tibble(
  x = domain,
  y = f(domain)
)

# -------- First plot (central interval) --------
df_left <- df |>
  filter(x >= -4, x <= -2)
df_right <- df |>
  filter(x >= 8, x <= 10)

p1 <- ggplot(df, aes(x, y)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_area(fill = "gray", alpha = 0.2) +
  geom_area(data = df_left, fill = "white") +
  geom_area(data = df_right, fill = "white") +
  annotate(
    "segment",
    x = -3.5, y = 0.11,
    xend = -2.3, yend = 0.015,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = -3.7, y = 0.12,
    label = expression(alpha / 2),
    size = 5
  ) +
  # right arrow
  annotate(
    "segment",
    x = 9.5, y = 0.11,
    xend = 8.3, yend = 0.015,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 9.7, y = 0.12,
    label = expression(alpha / 2),
    size = 5
  ) +
  ylim(0, 0.5) +
  theme_minimal()

# -------- Second plot (HPD) --------
df_left2 <- df |>
  filter(x >= -4, x <= -1.43992)
df_right2 <- df |>
  filter(x >= 7.37782, x <= 10)
df_mid <- df |>
  filter(x >= 1.3544, x <= 4.5837)

p2 <- ggplot(df, aes(x, y)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_area(fill = "gray", alpha = 0.2) +
  geom_area(data = df_left2, fill = "white") +
  geom_area(data = df_right2, fill = "white") +
  geom_hline(yintercept = 0.15, color = "blue") +
  geom_area(data = df_mid, fill = "white") +
  scale_y_continuous(
    breaks = function(x) sort(unique(c(pretty(x), 0.15))),
    labels = function(x) ifelse(x == 0.15, "pMIN", x)
  ) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal()

p1 + p2

