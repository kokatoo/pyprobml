library(glmnet)
library(patchwork)
library(tidyverse)

make_1dregression_data <- function(n = 21) {
  set.seed(0)

  xtrain <- seq(0, 20, length.out = n)
  xtest <- seq(0, 20, by = 0.1)

  sigma2 <- 4
  w <- c(-1.5, 1/9)

  fun <- function(x) w[1] * x + w[2] * x^2

  ytrain <- fun(xtrain) +
    rnorm(length(xtrain), sd = sqrt(sigma2))
  ytest <- fun(xtest) +
    rnorm(length(xtest), sd = sqrt(sigma2))

  list(
    xtrain = xtrain,
    ytrain = ytrain,
    xtest = xtest,
    ytest = ytest
  )
}

scale01 <- function(x) {
  2 * (x - min(x)) /
    (max(x) - min(x)) - 1
}

poly_design <- function(x, deg) {
  sapply(
    1:deg,
    function(d) x^d
  ) |>
    as.matrix()
}

mse <- function(yhat, y) mean((yhat - y)^2)

dat <- make_1dregression_data()

xtrain <- dat$xtrain
ytrain <- dat$ytrain
xtest <- dat$xtest
ytest <- dat$ytest

Xtrain <- scale01(xtrain)
Xtest <- scale01(xtest)

deg <- 14

Xtrain_poly <- poly_design(Xtrain, deg)
Xtest_poly <- poly_design(Xtest, deg)

lambdas <- exp(
  seq(log(1e-10), log(10^1.3), length.out = 10)
)

mse_train <- numeric(length(lambdas))
mse_test <- numeric(length(lambdas))
pred_store <- list()

for (i in seq_along(lambdas)) {
  model <- glmnet(
    x = Xtrain_poly,
    y = ytrain,
    alpha = 0, # alpha = 0 → ridge (pure L2)
    lambda = lambdas[i],
    intercept = FALSE,
    standardize = FALSE
  )

  ytrain_pred <- as.numeric(
    predict(model, Xtrain_poly)
  )
  ytest_pred <- as.numeric(
    predict(model, Xtest_poly)
  )

  mse_train[i] <- mse(ytrain_pred, ytrain)
  mse_test[i] <- mse(ytest_pred, ytest)

  pred_store[[i]] <- ytest_pred
}

df_mse <- tibble(
  lambda = lambdas,
  train = mse_train,
  test = mse_test
) |>
  pivot_longer(
    -lambda,
    names_to = "set",
    values_to = "mse"
  )

p1 <- ggplot(
  df_mse,
  aes(lambda, mse, colour = set, shape = set)
) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "L2 regularizer",
    y = "mse",
    colour = NULL,
    shape = NULL
  )

chosen <- c(1, 6, 9)

plots <- lapply(
  seq_along(chosen),
  function(i) {

    idx <- chosen[i]

    tibble(
      x = xtest,
      y = pred_store[[idx]]
    ) |>
      ggplot(aes(x, y)) +
      geom_line(color = "blue") +
      geom_point(
        data = tibble(x = xtrain, y = ytrain),
        color = "blue",
        aes(x, y)
      ) +
      theme_minimal() +
      labs(
        title = paste0(
          "L2 regularizer ",
          round(lambdas[idx], 5)
        )
      )
  }
)

(plots[[1]] + plots[[2]]) /
  (plots[[3]] + p1)
