library(tidyverse)
# -----------------------------
# Normalize
# -----------------------------
normalize <- function(x) {
  x / sum(x)
}

# -----------------------------
# Bayesian update model
# -----------------------------
posterior_covid <- function(observed,
                            prevalence = 0.1,
                            sensitivity = 0.875) {

  specificity <- 0.975

  TPR <- sensitivity
  FNR <- 1 - TPR
  TNR <- specificity
  FPR <- 1 - TNR

  # likelihood matrix: [hidden, observed]
  likelihood_fn <- matrix(
    c(TNR, FPR,
      FNR, TPR),
    nrow = 2,
    byrow = TRUE
  )

  prior <- c(1 - prevalence, prevalence)

  likelihood <- likelihood_fn[, observed + 1]

  posterior <- normalize(prior * likelihood)

  posterior
}

# -----------------------------
# Posterior calculations
# -----------------------------
posterior_covid(1)[2]
posterior_covid(0)[2]

posterior_covid(1, prevalence = 0.01)[2]
posterior_covid(0, prevalence = 0.01)[2]

