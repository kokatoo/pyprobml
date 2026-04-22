library(tidyverse)
library(patchwork)

make_graph <- function(data, save_name) {

  a0 <- data$prior$a
  b0 <- data$prior$b

  n0 <- data$likelihood$n_0
  n1 <- data$likelihood$n_1

  post_a <- data$posterior$a
  post_b <- data$posterior$b

  prior <- dbeta(x, a0, b0)
  posterior <- dbeta(x, post_a, post_b)

  n <- n0 + n1

  marginal <- choose(n, n1) *
    beta(n1 + a0, n0 + b0) /
    beta(a0, b0)

  # divide by marginal so when prior is uniform, likelihood is the same as posterior
  likelihood <- sapply(
    x,
    function(p) {
      dbinom(n1, size = n0 + n1, prob = p) /
        marginal
    }
  )

  df <- tibble(
    x = x,
    prior = prior,
    posterior = posterior,
    likelihood = likelihood
  ) |>
    pivot_longer(
      -x,
      names_to = "type",
      values_to = "value"
    )

  df |>
    ggplot(aes(x, value, colour = type)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(y = "prior / posterior", colour = NULL)
}

x <- seq(0.001, 0.999, length.out = 100)

data1 <- list(
  prior = list(
    a = 1,
    b = 1
  ),
  likelihood = list(
    n_0 = 1,
    n_1 = 4
  ),
  posterior = list(
    a = 5,
    b = 2
  )
)

data2 <- list(
  prior = list(
    a = 1,
    b = 1
  ),
  likelihood = list(
    n_0 = 10,
    n_1 = 40
  ),
  posterior = list(
    a = 41,
    b = 11
  )
)

data3 <- list(
  prior = list(
    a = 2,
    b = 2
  ),
  likelihood = list(
    n_0 = 1,
    n_1 = 4
  ),
  posterior = list(
    a = 6,
    b = 3
  )
)

data4 <- list(
  prior = list(
    a = 2,
    b = 2
  ),
  likelihood = list(
    n_0 = 10,
    n_1 = 40
  ),
  posterior = list(
    a = 42,
    b = 12
  )
)

make_graph(data1)
make_graph(data2)
make_graph(data3)
make_graph(data4)
