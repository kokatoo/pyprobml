library(patchwork)
library(tidyverse)

HDIofInvCDF <-
  function(qfun,
           mass_inside = 0.95,
           ...) {

    tail_mass <- 1 - mass_inside

    # The interval is:
    #   [ Q(low_tail_prob), Q(low_tail_prob + mass_inside) ]
    interval_width <- function(low_tail_prob) {
      qfun(mass_inside + low_tail_prob, ...) -
        qfun(low_tail_prob, ...)
    }

    opt <- optimize(
      interval_width,
      interval = c(0, tail_mass)
    )

    low <- opt$minimum

    c(
      qfun(low, ...),
      qfun(low + mass_inside, ...)
    )
  }

plot_interval <- function(lower, upper, title) {

  y1 <- dbeta(lower, a, b)
  y2 <- dbeta(upper, a, b)

  ggplot(df, aes(x, y)) +
    geom_line(color = "black") +
    geom_segment(
      aes(
        x = lower,
        xend = lower,
        y = 0,
        yend = y1
      ),
      color = "blue"
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        y = y1,
        yend = y2
      ),
      color = "blue"
    ) +
    geom_segment(
      aes(
        x = upper,
        xend = upper,
        y = y2,
        yend = 0
      ),
      color = "blue"
    ) +
    labs(title = title) +
    theme_minimal()
}

a <- 3
b <- 9
alpha <- 0.05

CI <- c(
  qbeta(alpha / 2, a, b),
  qbeta(1 - alpha / 2, a, b)
)

HPD <- HDIofInvCDF(
  qbeta,
  mass_inside = 0.95,
  shape1 = a,
  shape2 = b
)

x <- seq(0.001, 0.999, length.out = 200)

df <- tibble(
  x = x,
  y = dbeta(x, a, b)
)

intervals <- tibble(
  type = c("CI", "HPD"),
  lower = c(CI[1], HPD[1]),
  upper = c(CI[2], HPD[2])
)

plots <- intervals |>
  mutate(
    plot = map2(
      lower,
      upper,
      plot_interval,
      title = type
    )
  ) |>
  pull(plot)

wrap_plots(plots)


### beta_credible_int_demo
set.seed(42)

# -----------------------------
# Data / posterior parameters
# -----------------------------
N1 <- 2
N0 <- 8
N <- N0 + N1

aprior <- 1
bprior <- 1

apost <- aprior + N1
bpost <- bprior + N0

alpha <- 0.05

# -----------------------------
# CI via quantiles (explicit)
# -----------------------------
l <- qbeta(alpha / 2, apost, bpost)
u <- qbeta(1 - alpha / 2, apost, bpost)

CI2 <- c(l, u)

sprintf("%.2f--%.2f", CI2[1], CI2[2])

# -----------------------------
# Monte Carlo CI
# -----------------------------
samples <- rbeta(1000, apost, bpost)
samples <- sort(samples)

CI3 <- quantile(
  samples,
  probs = c(alpha / 2, 1 - alpha / 2)
)

sprintf("%.2f--%.2f", CI3[1], CI3[2])
