library(patchwork)
library(tidyverse)

# -----------------------------
# 1. Grid
# -----------------------------
x <- seq(-4, 4, length.out = 100)

# -----------------------------
# 2. PDFs
# -----------------------------
laplace <- function(x, mu = 0, b = 1 / sqrt(2)) {
  # Laplace PDF
  (1 / (2 * b)) *
    exp(-abs(x - mu) / b)
}
laplace_ <- laplace(x)

normal_pdf <- function(x, mu = 0, sigma = 1) {
  (1 / sqrt(2 * pi * sigma^2)) *
    exp(
      -(x - mu)^2 / (2 * sigma^2)
    )
}

student_t_pdf <-
  function(x, df = 1, mu = 0, sigma = 1) {

    num <- gamma((df + 1) / 2)
    den <- gamma(df / 2) *
      sqrt(df * pi) *
      sigma

    z <- (x - mu) / sigma

    num / den *
      (1 + (z^2 / df))^(-(df + 1) / 2)
  }

# R version
normal <- dnorm(x, mean = 0, sd = 1)
student_t1 <- dt(x, df = 1)  # scale = 1 already built-in form
student_t2 <- dt(x, df = 2)

# alternate implementation
normal_2 <- normal_pdf(x, mu = 0, sigma = 1)
student_t1 <- student_t_pdf(x, df = 1)  # scale = 1 already built-in form
student_t2 <- student_t_pdf(x, df = 20)

df <- tibble(
  x = x,
  normal = normal,
  laplace = laplace_,
  t1 = student_t1,
  t2 = student_t2
)

df_long <- df |>
  pivot_longer(
    -x, names_to = "dist", values_to = "pdf"
  )

p1 <- ggplot(
  df_long,
  aes(x, pdf, color = dist)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      normal = "black",
      laplace = "red",
      t1 = "blue",
      t2 = "green",
      half_cauchy = "orange"
    )
  ) +
  labs(
    x = "x",
    y = "pdf",
    title = "Gaussian vs Laplace vs Student-t Distributions"
  ) +
  theme_minimal()

p2 <- ggplot(
  df_long,
  aes(x, log(pdf), color = dist)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      normal = "black",
      laplace = "red",
      t1 = "blue",
      t2 = "green"
    )
  ) +
  labs(
    x = "x",
    y = "pdf",
    title = "Gaussian vs Laplace vs Student-t Distributions"
  ) +
  theme_minimal()
p2

p1 + p2
