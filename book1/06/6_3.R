library(mvtnorm)
library(tidyverse)
library(patchwork)

# helper: multivariate normal density
dmvnorm2 <- function(X, mu, Sigma) {
  d <- ncol(X)

  Sigma_inv <- solve(Sigma)
  det_Sigma <- det(Sigma)

  diff <- sweep(X, 2, mu)

  expo <- rowSums((diff %*% Sigma_inv) * diff)

  (1 / sqrt((2 * pi)^d * det_Sigma)) *
    exp(-0.5 * expo)
}

make_plot <- function(z) {
  ggplot(df, aes(x, y)) +
    geom_contour(aes(z = f), color = "blue") +
    geom_contour(aes(z = !!sym(z)), color = "red") +
    theme_void()
}


# parameters
mu <- list(
  c(-1, -1),
  c(1, 1)
)

Sigma1 <- matrix(
  c(1/2, 1/4,
    1/4, 1),
  2, 2
)

Sigma2 <- matrix(
  c(1/2, -1/4,
    -1/4, 1),
  2, 2
)

SigmaKL <- matrix(
  c(3, 2,
    2, 3),
  2, 2
)

# grid
x1 <- seq(-4, 4, by = 0.1)
x2 <- x1
grid <- expand.grid(x1 = x1, x2 = x2) |>
  as.matrix()

# densities
f1 <- dmvnorm(grid, mean = mu[[1]], sigma = Sigma1)
f2 <- dmvnorm(grid, mean = mu[[2]], sigma = Sigma2)
## f1 <- dmvnorm2(grid, mu[[1]], Sigma1)
## f2 <- dmvnorm2(grid, mu[[2]], Sigma2)
f  <- f1 + f2

klf <- dmvnorm(grid, c(0,0), SigmaKL)
kll <- dmvnorm(grid, mu[[1]], Sigma1 * 0.6)
klr <- dmvnorm(grid, mu[[2]], Sigma2 * 0.6)

## klf <- dmvnorm2(grid, c(0,0), SigmaKL)
## kll <- dmvnorm2(grid, mu[[1]], Sigma1 * 0.6)
## klr <- dmvnorm2(grid, mu[[2]], Sigma2 * 0.6)


# dataframe
df <- tibble(
  x = grid[,1],
  y = grid[,2],
  f = f,
  klf = klf,
  kll = kll,
  klr = klr
)

p1 <- make_plot("klf")
p2 <- make_plot("kll")
p3 <- make_plot("klr")

wrap_plots(p1, p2, p3)
