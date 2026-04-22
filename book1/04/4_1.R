library(GGally)
library(corrplot)
library(tidyverse)
library(patchwork)

df_iris <- iris |>
  as_tibble()

cov_mat <- df_iris |>
  select(where(is.numeric)) |>
  cov()

cov_df <- as.data.frame(as.table(cov_mat)) |>
  rename(var1 = Var1, var2 = Var2, value = Freq)

p1 <- ggplot(
  cov_df,
  aes(var1, var2, fill = value)
) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(
    low = "blue",
    high = "yellow",
    mid = "white"
  ) +
  coord_fixed() +
  labs(title = "Covariance Matrix", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

corr <- df_iris |>
  select(where(is.numeric)) |>
  cor()

corr_df <- as.data.frame(as.table(corr)) |>
  rename(var1 = Var1, var2 = Var2, value = Freq) |>
  mutate(mask = as.vector(mask))

p2 <- ggplot(
  corr_df,
  aes(var1, var2, fill = value)
) +
  geom_tile(
    data = corr_df |>
      filter(!mask)
  ) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(
    low = "blue",
    high = "yellow",
    mid = "white"
  ) +
  coord_fixed() +
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

p1 + p2
