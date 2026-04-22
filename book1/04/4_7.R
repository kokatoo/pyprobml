library(glmnet)
library(rsample)
library(yardstick)
library(tidyverse)
library(patchwork)

func <- function(x, w) w[1] * x + w[2] * x^2

polydatemake <- function() {
  n <- 21
  sigma2 <- 4

  xtrain <- seq(0, 20, length.out = n)
  xtest <- seq(0, 20, by = 0.1)

  w <- c(-1.5, 1/9)

  ytrain <- func(xtrain, w) +
    rnorm(
      length(xtrain),
      sd = sqrt(sigma2)
    )

  ytest_noisefree <- func(xtest, w)

  ytest <- ytest_noisefree +
    rnorm(
      length(xtest),
      sd = sqrt(sigma2)
    )

  list(
    xtrain = xtrain,
    ytrain = ytrain,
    xtest = xtest,
    ytest = ytest,
    ytest_nf = ytest_noisefree,
    sigma2 = sigma2
  )
}

scale01 <- function(x) {
  2 * (x - min(x)) /
    (max(x) - min(x)) - 1
}

poly_features <- function(x, deg = 14) {
  sapply(
    1:deg,
    function(d) x^d
  ) |>
    as.matrix()
}

mse <- function(a, b) mean((a - b)^2)

set.seed(0)

dat <- polydatemake()

xtrain <- dat$xtrain
ytrain <- dat$ytrain
xtest <- dat$xtest
ytest <- dat$ytest

deg <- 14

Xtrain <- poly_features(scale01(xtrain), deg)
Xtest <- poly_features(scale01(xtest), deg)

# center y so no need for bias/intercept term
ytrain <- ytrain - mean(ytrain)
ytest <- ytest - mean(ytest)

lambdas <- exp(
  seq(log(1e-10), log(10^1.3), length.out = 10)
)

train_mse <- numeric(length(lambdas))
test_mse <- numeric(length(lambdas))

for (i in seq_along(lambdas)) {

  fit <- glmnet(
    Xtrain, ytrain,
    alpha = 0,
    lambda = lambdas[i],
    intercept = FALSE,
    standardize = FALSE
  )

  pred_train <- as.numeric(predict(fit, Xtrain))
  pred_test <- as.numeric(predict(fit, Xtest))

  train_mse[i] <- mse(pred_train, ytrain)
  test_mse[i] <- mse(pred_test, ytest)
}

df <- tibble(
  lambda = lambdas,
  train = train_mse,
  test = test_mse
) |>
  pivot_longer(
    -lambda,
    names_to = "set",
    values_to = "mse"
  )

p1 <- ggplot(
  df,
  aes(lambda, mse, colour = set)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "log lambda",
    y = "mean squared error",
    colour = NULL
  )

# ---------------- CV ----------------

n_folds <- 5

cv_means <- numeric(length(lambdas))
cv_se <- numeric(length(lambdas))

df <- tibble(
  id = seq_along(ytrain),
  y = ytrain
)

folds <- vfold_cv(
  df,
  v = n_folds
)

for (i in seq_along(lambdas)) {

  lam <- lambdas[i]
  errs <- c()

  for (f in folds$splits) {

    train_data <- analysis(f)
    test_data  <- assessment(f)

    idx_train <- train_data$id
    idx_test  <- test_data$id

    Xtr <- Xtrain[idx_train, ]
    ytr <- ytrain[idx_train]

    Xva <- Xtrain[idx_test, ]
    yva <- ytrain[idx_test]

    fit <- glmnet(
      Xtr,
      ytr,
      alpha = 0,
      lambda = lam,
      intercept = FALSE,
      standardize = FALSE
    )

    pred <- as.numeric(predict(fit, Xva))

    errs <- c(
      errs,
      mean((pred - yva)^2)
    )
  }

  cv_means[i] <- mean(errs)
  cv_se[i] <- sd(errs) /
    sqrt(n_folds)
}

df_cv <- tibble(
  lambda = lambdas,
  mean = cv_means,
  se = cv_se
)

p2 <- ggplot(
  df_cv,
  aes(lambda, log(mean))
) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = log(mean) - log(se) / 2,
      ymax = log(mean) + log(se) / 2
    )
  ) +
  scale_x_log10() +
  theme_minimal() +
  labs(x = "log lambda", y = "mean squared error")

p1 + p2

## contrast with cv.glmnet
cv_fit <- cv.glmnet(
  Xtrain,
  ytrain,
  alpha = 0,
  intercept = FALSE,
  standardize = FALSE
)
cv_fit$lambda.min   # lambda with minimum CV error
cv_fit$lambda.1se   # largest lambda within 1 SE of minimum

df_cv |>
  mutate(
    lambda = sprintf("%.10f", lambda)
  )
