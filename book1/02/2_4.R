library(tidyverse)

mu <- c(0, 2)
sigma <- c(0.5, 0.5)
weights <- c(0.5, 0.5)

x <- seq(-2, 2 * mu[2], length.out = 600)

# components
p1 <- weights[1] * dnorm(x, mu[1], sigma[1])
p2 <- weights[2] * dnorm(x, mu[2], sigma[2])

# mixture
p <- p1 + p2

# moments
mean_p <- sum(x * p) / sum(p)
var_p <- sum((x - mean_p)^2 * p) / sum(p)

df <- tibble(x = x)

ggplot(df, aes(x)) +
  geom_line(aes(y = p), linewidth = 1, color = "black") +            # mixture
  geom_line(aes(y = p1), linewidth = 1, linetype = "dashed", color = "green") +
  geom_line(aes(y = p2), linewidth = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean_p, linetype = "dashed") +
  labs(
    x = "x",
    y = "p(x)",
    title = "Bimodal mixture"
  ) +
  theme_minimal()

var_p1 <- weights[1] * sigma[1]^2 +
  weights[2] * sigma[2]^2
var_p2 <- weights[1] * (mu[1] - mean_p)^2 +
    weights[2] * (mu[2] - mean_p)

mean_p
var_p

var_p1 + var_p2


