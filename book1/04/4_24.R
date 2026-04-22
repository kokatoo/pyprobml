library(tidyverse)
library(patchwork)

colors <- c(
  "blue","red","black","green","cyan","yellow","magenta",
  "red","blue","black","green","cyan","yellow","magenta"
)

# -----------------------------
# Posterior mean sampling distribution
# -----------------------------
plot_posterior_mean <- function(k0 = 4, n = 5) {

  k0s <- 0:(k0 - 1)
  thetaTrue <- 1
  sigmaTrue <- 1
  thetaPrior <- 0

  xrange <- seq(-1, 2.5, by = 0.05)

  df <- map_dfr(
    seq_along(k0s),
    function(i) {

      k0 <- k0s[i]
      w <- n / (n + k0)
      v <- w^2 * sigmaTrue^2 / n
      thetaEst <- w * thetaTrue + (1 - w) * thetaPrior

      tibble(
        x = xrange,
        y = dnorm(
          xrange,
          mean = thetaEst,
          sd = sqrt(v)
        ),
        kappa = paste0("k0=", k0),
        color = colors[i]
      )
    }
  )

  ggplot(
    df,
    aes(x, y, color = kappa)
  ) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = paste(
        "Sampling Distribution\n truth =",
        thetaTrue,
        ", prior =",
        thetaPrior,
        ", n =",
        n
      ),
      x = "x",
      y = "P(x) (Posterior mean)"
    )
}

# -----------------------------
# Relative MSE plot
# -----------------------------
plot_relative_mean <- function(k0 = 4) {

  k0s <- 0:(k0 - 1)
  ns <- seq(1, 49, by = 2)

  thetaTrue <- 1
  sigmaTrue <- 1
  thetaPrior <- 0

  df <- map_dfr(
    seq_along(k0s),
    function(i) {

      k0 <- k0s[i]

      ws <- ns / (ns + k0)

      mseE <- sigmaTrue^2 / ns
      # variance + bias
      mseB <- ws^2 * sigmaTrue^2 / ns +
        (1 - ws)^2 * (thetaPrior - thetaTrue)^2

      tibble(
        n = ns,
        ratio = mseB / mseE,
        kappa = paste0("k0=", k0)
      )
    }
  )

  ggplot(
    df,
    aes(n, ratio, color = kappa)
  ) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "MSE of postmean / MSE of MLE",
      x = "Sample Size",
      y = "Relative MSE"
    )
}

p1 <- plot_posterior_mean()
p2 <- plot_relative_mean()

p1 + p2
