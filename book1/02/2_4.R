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
  geom_line(
    aes(y = p), linewidth = 1, color = "black"
  ) +            # mixture
  geom_line(
    aes(y = p1), linewidth = 1, linetype = "dashed", color = "green"
  ) +
  geom_line(
    aes(y = p2), linewidth = 1, linetype = "dashed", color = "red"
  ) +
  geom_vline(
    xintercept = mean_p, linetype = "dashed"
  ) +
  labs(
    x = "x", y = "p(x)",
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

n_samples <- 10000
# Step 1: Choose which component for each sample
components <- sample(
  1:2, size = n_samples,
  replace = TRUE, prob = weights
)

# Step 2: Sample from the chosen components
samples <- ifelse(
  components == 1,
  rnorm(n_samples, mu[1], sigma[1]),
  rnorm(n_samples, mu[2], sigma[2])
)

# ---------- VISUALIZATION ----------
# Create a data frame for plotting
df <- tibble(x = x, density = p)
samples_df <- tibble(samples = samples)

# Plot histogram of samples + density curve
ggplot() +
  geom_histogram(
    data = samples_df,
    aes(x = samples, y = after_stat(density)),
    bins = 30, fill = "lightblue",
    color = "black", alpha = 0.7
  ) +
  geom_line(
    data = df,
    aes(x = x, y = density),
    color = "red", linewidth = 1.2
  ) +
  labs(
    title = "Mixture Distribution (samples + theoretical density)",
    x = "Value", y = "Density"
  ) +
  theme_minimal()

var(samples)
