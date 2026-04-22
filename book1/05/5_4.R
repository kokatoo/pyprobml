library(tidyverse)
library(patchwork)

# parameters
N <- 5
alphaH <- 1
alphaT <- 1
eps <- 1e-7

# all binary sequences (0 = tail, 1 = head)
flips_mat <- expand.grid(
  rep(
    list(c(0, 1)),
    N
  )
) |>
  as.matrix()

# stats
Nh <- rowSums(flips_mat == 1)
Nt <- rowSums(flips_mat == 0)

marginal_lik <- exp(
  lbeta(alphaH + Nh, alphaT + Nt) -
    lbeta(alphaH, alphaT)
)
mle <- Nh / N

log_lik <- Nh * log10(mle + eps) +
  Nt * log10(1 - mle + eps)

log_BF <- lbeta(alphaH + Nh, alphaT + Nt) -
  lbeta(alphaH, alphaT) - N * log(0.5)

df <- tibble(
  Nh = Nh,
  Nt = Nt,
  marginal_lik = marginal_lik,
  log_lik = log_lik,
  log_BF = log_BF
) |>
  arrange(Nh)

p0 <- (1 / 2)^N

# plots
p1 <- ggplot(
  df,
  aes(x = Nh, y = marginal_lik)
) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = p0) +
  labs(
    title = "Marginal likelihood",
    x = "num heads"
  ) +
  theme_minimal()

p2 <- ggplot(
  df,
  aes(x = Nh, y = exp(log_BF))
) +
  geom_line() +
  geom_point() +
  labs(
    title = "BF(1,0)",
    x = "num heads"
  ) +
  theme_minimal()

p3 <- ggplot(
  df,
  aes(x = Nh, y = log_lik - 1)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "BIC approximation",
    x = "num heads"
  ) +
  theme_minimal()

p4 <- ggplot(
  df,
  aes(x = Nh, y = log10(marginal_lik))
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log marginal likelihood",
    x = "num heads"
  ) +
  theme_minimal()

# combine in one window
wrap_plots(list(p1, p2, p3, p4))
