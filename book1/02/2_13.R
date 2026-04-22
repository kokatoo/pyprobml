library(tidyverse)

iris <- as_tibble(iris)

# -----------------------------
# Data (2 features, 3 classes)
# -----------------------------
df <- iris |>
  mutate(
    x1 = Petal.Length,
    x2 = Petal.Width,
    y  = Species,
    Species = as_factor(Species)
  ) |>
  select(x1, x2, everything())

train_x <- df |>
  select(x1, x2)
train_y <- df |>
  select(y)

# -----------------------------
# Fit multinomial logistic regression
# -----------------------------
model <- nnet::multinom(
  y ~ x1 + x2,
  data = df,
  decay = 0.1,
  trace = FALSE
)

# -----------------------------
# Grid for decision surface
# -----------------------------
x1_seq <- seq(min(train_x$x1) - 1, max(train_x$x1) + 1, length.out = 500)
x2_seq <- seq(min(train_x$x2) - 1, max(train_x$x2) + 2, length.out = 500)

grid <- expand_grid(x1 = x1_seq, x2 = x2_seq)

# class predictions + probabilities
grid_prob <- predict(
  model,
  newdata = grid,
  type = "prob"
)

grid <- grid |>
  mutate(
    class = predict(
      model,
      newdata = grid,
      type = "class"
  )
)

grid <- bind_cols(grid, as_tibble(grid_prob))

# Choose probability for label Iris-Versicolor
grid$prob_iris <- grid$virginica
grid$prob_iris <- grid$setosa
grid$prob_iris <- grid$versicolor

# -----------------------------
# Plot
# -----------------------------
ggplot() +
  # decision regions (like contourf)
  geom_tile(
    data = grid,
    aes(x = x1, y = x2, fill = class),
    alpha = 0.5
  ) +
  scale_fill_manual(values = c(
    setosa = "#9898ff",
    versicolor = "#fafab0",
    virginica = "#a0faa0"
  )) +  # training points
  geom_point(
    data = df,
    aes(x = x1, y = x2, color = Species, shape = Species),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      setosa = "blue",
      versicolor = "orange",
      virginica = "green"
    )
  ) +
  scale_shape_manual(
    values = c(
      setosa = 15,
      versicolor = 16,
      virginica = 17
    )
  ) +
  # merge legend
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    fill  = guide_legend(order = 2)
  ) +
  # probability contours (like plt.contour + clabel idea)
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = prob_iris),
    color = "black",
    bins = 6
  ) +
  coord_cartesian(xlim = c(0, 7)) +
  labs(
    x = "Petal length",
    y = "Petal width",
    shape = "Species",
    color = "Species",
    fill = "Predicted class"
  ) +
  theme_minimal()

###############

# =========================================================
# Multinomial Logistic Regression in R
# (LBFGS + explicit bias column + softmax + L2 penalty)
# =========================================================

# -----------------------------
# Softmax
# -----------------------------
softmax <- function(Z) {
  expZ <- exp(Z)
  expZ / rowSums(expZ)
}

lse <- function(a) {
  m <- max(a)
  m + log(sum(exp(a - m)))
}

softmax <- function(a) {
  exp(a - lse(a))
}


# -----------------------------
# One-hot encoding
# -----------------------------
one_hot <- function(y) {
  classes <- sort(unique(y))
  t(sapply(y, function(v) as.numeric(classes == v)))
}

# -----------------------------
# Multiclass loss (same as Python)
# -----------------------------
multi_loss_function <- function(w, X, Y, lambda) {

  n_class <- ncol(Y)
  n_feat  <- ncol(X)

  W <- matrix(w, nrow = n_feat, ncol = n_class)

  logits <- X %*% W
  P <- softmax(logits)

  # negative log-likelihood
  nll <- -sum(Y * log(P + 1e-12))

  # L2 regularization (exclude bias row)
  reg <- lambda * sum(W[-1, ]^2)

  nll + reg
}

# -----------------------------
# Fit function (Python-style LBFGS)
# -----------------------------
fit_softmax_lbfgs <- function(X, y, lambda = 1) {

  # explicit bias column
  X <- cbind(1, X)

  Y <- one_hot(y)

  n_feat  <- ncol(X)
  n_class <- ncol(Y)

  # same idea as JAX random init
  set.seed(0)
  w_init <- rnorm(n_feat * n_class, 0, 0.01)

  loss_fn <- function(w) {
    multi_loss_function(w, X, Y, lambda)
  }

  res <- optim(
    par = w_init,
    fn = loss_fn,
    method = "L-BFGS-B",
    control = list(maxit = 1000, factr = 1e7)
  )

  W <- matrix(res$par, nrow = n_feat, ncol = n_class)

  list(
    weights = W,
    bias = W[1, ],
    coef = W[-1, , drop = FALSE]
  )
}

# -----------------------------
# Prediction: probabilities
# -----------------------------
predict_proba <- function(model, X) {
  X <- cbind(1, X)
  softmax(X %*% model$weights)
}

# -----------------------------
# Prediction: class labels
# -----------------------------
predict_class <- function(model, X) {
  probs <- predict_proba(model, X)
  max.col(probs) - 1
}

df <- iris

X <- as.matrix(
  df |> select(Petal.Length, Petal.Width)
)
y <- as.numeric(df$Species) - 1

model <- fit_softmax_lbfgs(X, y, lambda = 0.1)

probs <- predict_proba(model, X)
preds <- predict_class(model, X)

# -----------------------------
# Grid (same idea as Python meshgrid)
# -----------------------------
x1_seq <- seq(0, 7, length.out = 400)
x2_seq <- seq(-1, 4, length.out = 400)

grid <- expand_grid(x1 = x1_seq, x2 = x2_seq)
Xg <- as.matrix(grid)

# -----------------------------
# Predict probabilities on grid
# -----------------------------
grid_probs <- predict_proba(model, Xg)

grid <- grid |>
  mutate(
    setosa = grid_probs[,1],
    versicolor = grid_probs[,2],
    virginica = grid_probs[,3],
    class = factor(
      max.col(grid_probs) - 1,
      levels = c(0,1,2),
      labels = c("setosa","versicolor","virginica")
    )
  )

# -----------------------------
# PLOT (Python-faithful version)
# -----------------------------
ggplot() +

  # ---- contourf equivalent (decision regions)
  geom_tile(
    data = grid,
    aes(x = x1, y = x2, fill = class),
    alpha = 0.35
  ) +

  scale_fill_manual(values = c(
    setosa = "#9898ff",
    versicolor = "#fafab0",
    virginica = "#a0faa0"
  )) +

  # ---- probability contours (this is the key Python part)
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = setosa),
    color = "blue",
    breaks = seq(0.1, 0.9, by = 0.2),
    linewidth = 0.4
  ) +

  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = versicolor),
    color = "purple",
    breaks = seq(0.1, 0.9, by = 0.2),
    linewidth = 0.4
  ) +

  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = virginica),
    color = "green",
    breaks = seq(0.1, 0.9, by = 0.2),
    linewidth = 0.4
  ) +

  # ---- training points (same markers idea as Python)
  geom_point(
    data = df |>
      mutate(species = factor(Species,
                              levels = c("setosa","versicolor","virginica"))),
    aes(x = Petal.Length,
        y = Petal.Width,
        color = species,
        shape = species),
    size = 2
  ) +

  scale_color_manual(values = c(
    setosa = "blue",
    versicolor = "orange",
    virginica = "green"
  )) +

  scale_shape_manual(values = c(
    setosa = 15,
    versicolor = 16,
    virginica = 17
  )) +

  # ---- merge legend (fix your earlier issue)
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    fill  = guide_legend(order = 2)
  ) +

  coord_cartesian(xlim = c(0, 7), ylim = c(-1, 4)) +

  labs(
    x = "Petal length",
    y = "Petal width",
    color = "Species",
    shape = "Species",
    fill  = "Predicted class"
  ) +

  theme_minimal()
