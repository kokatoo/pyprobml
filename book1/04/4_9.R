library(glmnet)
library(rsample)
library(yardstick)
library(tidyverse)
library(patchwork)

make_1dregression_data <- function(n = 21) {
  xtrain <- seq(0, 20, length.out = n)
  xtest  <- seq(0, 20, by = 0.1)

  w <- c(-1.5, 1/9)

  fun <- function(x) w[1] * x + w[2] * x^2

  sigma2 <- 4

  ytrain <- fun(xtrain) +
    rnorm(length(xtrain), sd = sqrt(sigma2))
  ytest  <- fun(xtest) +
    rnorm(length(xtest), sd = sqrt(sigma2))

  list(
    xtrain = xtrain,
    ytrain = ytrain,
    xtest = xtest,
    ytest = ytest
  )
}

expand_to_deg <- function(x, deg) {
  sapply(
    0:deg,
    function(i) x^i
  )
}

scale01 <- function(x) {
  2 * (x - min(x)) /
    (max(x) - min(x)) - 1
}

set.seed(0)

degrees <- c(1, 2, 10, 20)

plot_list <- list()

for (deg in degrees) {

  ns <- round(seq(10, 210, length.out = 20))

  err <- numeric(length(ns))
  err_train <- numeric(length(ns))

  for (i in seq_along(ns)) {
    n <- ns[i]
    dat <- make_1dregression_data(n)

    xtrain <- scale01(dat$xtrain)
    xtest  <- scale01(dat$xtest)

    ytrain <- dat$ytrain
    ytest  <- dat$ytest

    Xtr <- expand_to_deg(xtrain, deg)
    Xte <- expand_to_deg(xtest, deg)

    ## fit <- glmnet(
    ##   Xtr,
    ##   ytrain,
    ##   alpha = 0,
    ##   lambda = 0,
    ##   intercept = FALSE,
    ##   standardize = FALSE
    ## )

    ## pred_test <- as.numeric(predict(fit, Xte))
    ## pred_train <- as.numeric(predict(fit, Xtr))

    ## err[i] <- mean((ytest - pred_test)^2)
    ## err_train[i] <- mean((ytrain - pred_train)^2)

    # use lm() because λ = 0 reduces glmnet to OLS, but glmnet is built for λ > 0 regularization paths
    fit <- lm(ytrain ~ Xtr - 1)

    pred_test <- as.numeric(Xte %*% coef(fit))
    pred_train <- as.numeric(Xtr %*% coef(fit))

    err[i] <- mean((ytest - pred_test)^2)
    err_train[i] <- mean((ytrain - pred_train)^2)
  }

  df <- tibble(
    n = ns,
    test = err,
    train = err_train
  ) |>
    pivot_longer(
      -n,
      names_to = "set",
      values_to = "mse"
    )

  p <- ggplot(
    df,
    aes(n, mse, colour = set)
  ) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 4, linetype = "dashed") +
    scale_y_continuous(
      breaks = function(x)
        sort(
          unique(
            c(pretty(x), 4)
          )
        )
    ) +
    theme_minimal() +
    labs(
      title = paste0("true deg 2, model deg ", deg),
      x = "size of training set", y = "mse"
    )

  plot_list[[length(plot_list) + 1]] <- p
}

wrap_plots(plot_list)
