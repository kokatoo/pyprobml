library(tidyverse)

normalize_logspace <- function(x) {
  ## x - log(sum(exp(x)))
  m <- max(x)
  log_sum_exp <- m + log(sum(exp(x - m)))
  x - log_sum_exp
}

eval_pdf <- function(thetas, weights, alpha) {
  M <- length(weights)
  p <- rep(0, length(thetas))

  for (k in seq_len(M)) {
    a <- alpha[k, 1]
    b <- alpha[k, 2]

    # mixture density formula: p(x) = sum_k w_k * Beta(x | a_k, b_k)
    p <- p + weights[k] * dbeta(thetas, a, b)
  }

  p
}

# # N_1 = 20 heads, N_0 = 10 tails
dataSS <- c(20, 10)

# Component 1: Beta(20,20) -> fair coin prior
# Component 2: Beta(30,10) -> biased towards heads prior
alphaPrior <- matrix(
  c(20, 20,
    30, 10),
  ncol = 2,
  byrow = TRUE
)

M <- 2
mixprior <- c(0.5, 0.5)

logmarglik <- numeric(M)

for (i in seq_len(M)) {
  # lbeta = log Beta function
  logmarglik[i] <-
    lbeta(alphaPrior[i, 1] + dataSS[1],   # a_k + N1
          alphaPrior[i, 2] + dataSS[2]) - # b_k + N0
    lbeta(alphaPrior[i, 1],               # a_k
          alphaPrior[i, 2])               # b_k
}

# Compute posterior mixing weights
# p(h=k | D) ∝ p(h=k) * p(D | h=k)
# We do this in log space: log(prior) + log(marginal)
# Then normalize using log-sum-exp trick, then exponentiate
mixpost <- exp(
  normalize_logspace(
    logmarglik +           # log p(D | h=k)
      log(mixprior)        # log p(h=k)
  )
)

# Posterior parameters: a' = a + N1, b' = b + N0
alphaPost <- alphaPrior +
  matrix(
    rep(dataSS, each = M),   # repeat dataSS for each component: [20,10,20,10]
    ncol = 2
  )

grid <- seq(0.0001, 0.9999, by = 0.01)

post <- eval_pdf(grid, mixpost, alphaPost)
prior <- eval_pdf(grid, mixprior, alphaPrior)

df <- tibble(
  theta = grid,
  prior = prior,
  posterior = post
) |>
  pivot_longer(
    -theta,
    names_to = "type",
    values_to = "value"
  )

ggplot(
  df,
  aes(theta, value, colour = type)
) +
  geom_line(linewidth = 1) +
  theme_minimal()

pbiased <- 0

for (k in seq_len(M)) {
  a <- alphaPost[k, 1]
  b <- alphaPost[k, 2]

  pbiased <- pbiased +
    mixpost[k] * (1 - pbeta(0.5, a, b))
}
pbiased

pbiasedSimple <- 1 -
  pbeta(
    0.5,
    alphaPost[1, 1],
    alphaPost[1, 2]
  )
pbiasedSimple
