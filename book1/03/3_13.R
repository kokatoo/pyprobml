library(keras)
library(flexmix)
library(tidyverse)

mnist <- dataset_mnist()

x <- mnist$train$x |>
  array_reshape(
    dim = c(nrow(mnist$train$x), 28 * 28)
  ) |>
  as.matrix()

set.seed(1)
idx <- sample(1:nrow(x), 1000)
X <- x[idx, ] / 255
X <- ifelse(X > 0.5, 1, 0)

K <- 12

bmm <- flexmix(
  X ~ 1,
  k = K,
  model = FLXMCmvbinary()
)

post <- posterior(bmm)
clusters <- max.col(post)

centers <- parameters(bmm)

plot_digit <- function(v) {
  image(
    matrix(v, 28, 28)[, 28:1],
    col = gray.colors(256),
    axes = FALSE
  )
}

par(mfrow = c(3, 4), mar = c(0, 0, 2, 0))

for (k in 1:K) {
  plot_digit(centers[[k]])
  title(paste0("Cluster ", k))
}
