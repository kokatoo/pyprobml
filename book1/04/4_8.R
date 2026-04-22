library(keras)
library(tidyverse)
library(patchwork)
library(tensorflow)

make_model <- function(embed_size) {

  tf$random$set_seed(42)

  model <- keras_model_sequential() |>
    layer_embedding(
      input_dim = vocab_size,
      output_dim = embed_size
    ) |>
    layer_global_average_pooling_1d() |>
    layer_dense(16, activation = "relu") |>
    layer_dense(1, activation = "sigmoid")

  model |>
    compile(
      optimizer = "adam",
      loss = "binary_crossentropy",
      metrics = c("accuracy")
    )

  model
}

set.seed(0)
tf$random$set_seed(42)

vocab_size <- 10000

imdb <- dataset_imdb(num_words = vocab_size)

train_data <- imdb$train$x
train_labels <- imdb$train$y
test_data <- imdb$test$x
test_labels <- imdb$test$y

# pad sequences
train_data <- pad_sequences(
  train_data,
  maxlen = 256,
  padding = "post",
  value = 0
)

test_data  <- pad_sequences(
  test_data,
  maxlen = 256,
  padding = "post",
  value = 0
)

embed_size <- 16

model <- make_model(embed_size)

x_val <- train_data[1:10000, ]
x_train <- train_data[10001:nrow(train_data), ]

y_val <- train_labels[1:10000]
y_train <- train_labels[10001:length(train_labels)]

history <- model |>
  fit(
    x_train,
    y_train,
    epochs = 50,
    batch_size = 512,
    validation_data = list(
      x_val,
      y_val
    ),
    verbose = 1
  )

history_df <- as_tibble(history$metrics) |>
  mutate(epoch = row_number())

p1 <- ggplot(history_df, aes(epoch)) +
  geom_point(aes(y = loss), color = "blue") +
  geom_line(aes(y = val_loss), color = "red") +
  theme_minimal() +
  labs(title = "Loss", y = "loss")

p2 <- ggplot(history_df, aes(epoch)) +
  geom_point(aes(y = accuracy), color = "blue") +
  geom_line(aes(y = val_accuracy), color = "red") +
  theme_minimal() +
  labs(title = "Accuracy", y = "accuracy")

p1 + p2

model <- make_model(embed_size)

early_stop <- callback_early_stopping(
  monitor = "val_accuracy",
  patience = 2
)

checkpoint <- callback_model_checkpoint(
  filepath = "imdb_best_model.keras",
  monitor = "val_accuracy",
  save_best_only = TRUE
)

history2 <- model |>
  fit(
    x_train, y_train,
    epochs = 50,
    batch_size = 512,
    validation_data = list(x_val, y_val),
    verbose = 0,
    callbacks = list(early_stop, checkpoint)
  )

history_df2 <- as_tibble(history2$metrics) |>
  mutate(epoch = row_number())

ggplot(history_df2, aes(epoch)) +
  geom_point(aes(y = loss), color = "blue") +
  geom_line(aes(y = val_loss), color = "red") +
  theme_minimal() +
  labs(title = "Early stopping loss")
