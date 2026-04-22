# 5.2 (a)

library(tidyverse)

# ROC curves for two hypothetical classification systems
# A is better than B. Plots true positive rate (TPR) vs false positive rate (FPR)

fA <- function(x) x^(1/3)
fB <- function(x) x^(2/3)

x <- seq(0, 1, by = 0.01)

df <- tibble(
  x = x,
  A = fA(x),
  B = fB(x),
  diag = 1 - x
)

inter_a <- 0.3177
inter_b <- 0.4302

ggplot(df, aes(x)) +
  geom_line(aes(y = A), linewidth = 1) +
  geom_line(aes(y = B), linewidth = 1) +
  geom_ribbon(
    aes(ymin = 0, ymax = B),
    alpha = 0.2
  ) +
  geom_line(aes(y = diag), linetype = "dashed") +
  geom_point(
    aes(x = inter_a, y = fA(inter_a)),
    size = 3
  ) +
  geom_point(
    aes(x = inter_b, y = fB(inter_b)),
    size = 3
  ) +
  annotate(
    "text",
    x = inter_a,
    y = fA(inter_a) + 0.1,
    label = "A",
    size = 6
  ) +
  annotate(
    "text",
    x = inter_b,
    y = fB(inter_b) + 0.1,
    label = "B",
    size = 6
  ) +
  labs(x = "FPR", y = "TPR") +
  theme_minimal()

# 5.2 (b)
# Precision-recall curve for two hypothetical classification systems
# A is better than B

fA <- function(x) 1 - x^3
fB <- function(x) 1 - x^(3/2)

x <- seq(0, 1, by = 0.01)

df <- tibble(
  x = x,
  A = fA(x),
  B = fB(x)
)

ggplot(df, aes(x)) +
  geom_line(aes(y = A), linewidth = 1) +
  geom_line(aes(y = B), linewidth = 1) +
  annotate(
    "text",
    x = 0.6,
    y = 0.8,
    label = "A",
    size = 8
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 0.8,
    label = "B",
    size = 8
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1.01)) +
  labs(x = "recall", y = "precision") +
  theme_minimal()
