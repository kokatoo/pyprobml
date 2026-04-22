library(tidyverse)
library(patchwork)
library(ggseqlogo)

# sequences
seqs <- c(
  "atagccggtacggca",
  "ttagctgcaaccgca",
  "tcagccactagagca",
  "ataaccgcgaccgca",
  "ttagccgctaaggta",
  "taagcctcgtacgta",
  "ttagccgttacggcc",
  "atatccggtacagta",
  "atagcaggtaccgaa",
  "acatccgtgacggaa"
)

seq_len <- nchar(seqs[1])
letters <- c("A","C","G","T")

# count matrix
count_mat <- matrix(
  0,
  nrow = seq_len,
  ncol = 4
)

for (s in seqs) {
  chars <- strsplit(s, "")[[1]]

  for (i in seq_len(seq_len)) {
    idx <- match(
      toupper(chars[i]),
      letters
    )

    count_mat[i, idx] <- count_mat[i, idx] + 1
  }
}

# probability matrix
prob_mat <- count_mat / rowSums(count_mat)

# ---- Plot 1: probability logo ----
df_prob <- as_tibble(prob_mat)
colnames(df_prob) <- letters
df_prob <- df_prob |>
  mutate(
    pos = 1:seq_len
  ) |>
  select(
    pos, everything()
  )

mat_prob <- t(prob_mat)
rownames(mat_prob) <- c("A", "C", "G", "T")

p1 <- ggseqlogo(
  as.matrix(mat_prob),
  method = "prob"
) +
  scale_x_continuous(
    breaks = 1:seq_len
  ) +
  labs(
    x = "Sequence Position",
    y = "Probability"
  ) +
  theme_minimal()
p1

# ---- entropy ----
entropy <- numeric(seq_len)

for (i in 1:seq_len) {
  p <- prob_mat[i, ]

  entropy[i] <- -sum(p[p > 0] * log2(p[p > 0]))
}

# information content (bits)
# Max entropy for 4 letters = log_2(4) = 2 bits
pos_height_mat <- prob_mat * (2 - entropy)

mat_bits <- t(pos_height_mat)
rownames(mat_bits) <- c("A", "C", "G", "T")

# ---- Plot 2: information logo ----
p2 <- ggseqlogo(
  mat_bits,
  method = "custom"
) +
  scale_x_continuous(
    breaks = 1:seq_len
  ) +
  labs(
    x = "Sequence Position",
    y = "Bits"
  ) +
  theme_minimal()

# combine
p1 / p2
