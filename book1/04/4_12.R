library(ggplot2)
library(tidyverse)
library(extraDistr)

N <- 10

a <- 1
b <- 1
N1 <- 4
N0 <- 1

post_a <- a + N1
post_b <- b + N0

k <- 0:N

## Beta-Binomial Distribution
bb_pred <- choose(N, k) *
  beta(k + post_a, N - k + post_b) /
  beta(post_a, post_b)


mu_map <- (post_a - 1) /
  (post_a + post_b - 2)

plugin_pred <- dbinom(
  k,
  size = N,
  prob = mu_map
)

df <- tibble(
  k = k,
  posterior_predictive = bb_pred,
  plugin_predictive = plugin_pred
)

p1 <- ggplot(
  df,
  aes(k, posterior_predictive)
) +
  geom_col() +
  theme_minimal() +
  labs(title = "posterior predictive", x = "", y = "")

p2 <- ggplot(
  df,
  aes(k, plugin_predictive)
) +
  geom_col() +
  theme_minimal() +
  labs(title = "plugin predictive", x = "", y = "")

p1 + p2
