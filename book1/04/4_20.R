## 4.20 (a)
library(tidyverse)

df_iris <- iris |>
  mutate(
    species = as.factor(Species)
  ) |>
  select(
    sepal_length = Sepal.Length,
    sepal_width  = Sepal.Width,
    petal_length = Petal.Length,
    petal_width  = Petal.Width,
    species
  )

# -----------------------------
# Filter classes: setosa vs versicolor
# -----------------------------
df <- df_iris |>
  filter(
    species %in% c("setosa", "versicolor")
  ) |>
  mutate(
    y = as.integer(species == "versicolor")
  )

df <- df |>
  mutate(
    X = sepal_length
  )

# -----------------------------
# Logistic regression
# -----------------------------
log_reg <- glm(
  y ~ X,
  family = binomial(),
  data = df
)

X <- df$X
X_new <- seq(min(X), max(X), length.out = 1000)

p1 <- predict(
  log_reg,
  newdata = tibble(X = X_new),
  type = "response"
)

# decision boundary (p = 0.5)
decision_boundary <- X_new[which(p1 >= 0.5)[1]]
decision_boundary
coefs <- coef(log_reg)
-coefs[1] / coefs[2]

df_plot <- tibble(
  x = X_new,
  p = p1
)

ggplot(
  df_plot,
  aes(x, p)
) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = decision_boundary) +
  geom_point(
    data = df,
    aes(
      x = sepal_length,
      y = y + rnorm(nrow(df), 0, 0.02),
      color = factor(y))
  ) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "tomato")
  ) +
  labs(
    x = "sepal_length",
    y = "p(y = 1)"
  ) +
  theme_minimal()

## 4.20 (b)
library(cmdstanr)
library(posterior)
library(tidyverse)

# -----------------------------
# data
# -----------------------------
df <- iris |>
  filter(
    Species %in% c("setosa", "versicolor")
  ) |>
  mutate(
    y = as.integer(Species == "versicolor")
  )

## xmean <- mean(df$Sepal.Length)
## x_c <- df$Sepal.Length - xmean
x <- df$Sepal.Length
y <- df$y

df_model <- tibble(
  N = nrow(df),
  x = x,
  y = y
)

# -----------------------------
# Stan model
# -----------------------------
stan_code <- "
data {
  int<lower=0> N;
  vector[N] x;
  array[N] int<lower=0, upper=1> y;
}

parameters {
  real alpha;
  real beta;
}

model {
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);

  y ~ bernoulli_logit(alpha + beta * x);
}

generated quantities {
  real bd;

  bd = -alpha / beta;
}
"

mod <- cmdstan_model(write_stan_file(stan_code))

fit <- mod$sample(
  data = list(
    N = nrow(df_model),   # FIX
    x = df_model$x,
    y = df_model$y
  ),
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 0
)

draws <- fit$draws()

alpha <- as_draws_df(draws)$alpha
beta  <- as_draws_df(draws)$beta
bd    <- as_draws_df(draws)$bd

# -----------------------------
# prediction grid
# -----------------------------
x_grid <- seq(min(x), max(x), length.out = 200)

pred_grid <- sapply(
  seq_along(alpha),
  function(i) {
    plogis(alpha[i] + beta[i] * x_grid)
  }
)

pred_mean <- rowMeans(pred_grid)

df_plot <- tibble(
  x = x_grid,
  pred = pred_mean
)

df_hdi <- tibble(
  x = x_grid,
  low = apply(pred_grid, 1, quantile, 0.025),
  high = apply(pred_grid, 1, quantile, 0.975)
)

# -----------------------------
# plot
# -----------------------------
ggplot() +
  geom_line(
    data = df_plot,
    aes(x, pred),
    color = "darkgreen",
    linewidth = 1.2
  ) +
  geom_ribbon(
    data = df_hdi,
    aes(x, ymin = low, ymax = high),
    alpha = 0.2,
    fill = "darkgreen"
  ) +
  geom_vline(xintercept = mean(bd)) +
  geom_rect(
    aes(
      xmin = quantile(bd, 0.025),
      xmax = quantile(bd, 0.975),
      ymin = 0, ymax = 1
    ),
    fill = "black",
    alpha = 0.2
  ) +
  geom_point(
    data = df,
    aes(
      x = x,
      y = y + rnorm(nrow(df), 0, 0.02),
      color = factor(y)
    )
  ) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "tomato")
  ) +
  theme_minimal() +
  labs(x = "sepal_length (centered)", y = "p(y=1)")
