# Install and Load library
#install.packages("devtools")
#devtools::install_github("rstudio/keras")
#install.packages("keras")
library(keras)
install_keras()

# Load dataset
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Preprocess so that the training and test data for features are in matrix form
train_images <- array_reshape(train_images, c(60000, 28*28)) # matrix
train_images <- train_images/255 # ensures all values are in [0, 1]
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images/255

# Make the response categorical
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

# Fit a neural network model with 1 hidden layer with 512 hidden units and 5 epochs
network1 <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = "softmax")

# Compile the model
network1 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy", 
  metrics = c("accuracy")
)

# Fit the model
fit1_train <- network1 %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# training error
accuracy <- data.frame(fit1_train$metrics)
train.err.a <- 1-accuracy[5,2]
train.err.a

# test error:
metrics <- network1 %>% evaluate(test_images, test_labels, epochs = 5)
metrics <- data.frame(metrics)
test.err.a <- 1-metrics[2,1]
test.err.a

# Repeat with 1 hidden layer with 512 hidden units and 10 epochs
# Fit the model
fit2_train <- network1 %>% fit(train_images, train_labels, epochs = 10, batch_size = 128)

# training error
accuracy <- data.frame(fit2_train$metrics)
train.err.b <- 1-accuracy[5,2]
train.err.b

# test error
metrics <- network1 %>% evaluate(test_images, test_labels, epochs = 10)
metrics <- data.frame(metrics)
test.err.b <- 1-metrics[2,1]
test.err.b

# Repeat with 1 hidden layer with 256 hidden units and 5 epochs
# set up the network architecture
network2 <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = "softmax")

# Compile the model
network2 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",  # loss function to minimize
  metrics = c("accuracy") # monitor classification accuracy
)

# Fit the model
fit3_train <- network2 %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# training error
accuracy <- data.frame(fit3_train$metrics)
train.err.c <- 1-accuracy[5,2]
train.err.c

# test error
metrics <- network2 %>% evaluate(test_images, test_labels, epochs = 5)
metrics <- data.frame(metrics)
test.err.c <- 1-metrics[2,1]
test.err.c

# Repeat with 1 hidden layer with 256 hidden units and 10 epochs
# Fit the model
fit4_train <- network2 %>% fit(train_images, train_labels, epochs = 10, batch_size = 128)

# training error
accuracy <- data.frame(fit4_train$metrics)
train.err.d <- 1-accuracy[5,2]
train.err.d

# test error
metrics <- network2 %>% evaluate(test_images, test_labels, epochs = 10)
metrics <- data.frame(metrics)
test.err.d <- 1-metrics[2,1]
test.err.d

# Repeat ith 2 hidden layers, each with 512 hidden units, and 5 epochs.
# set up the network architecture
network3 <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# Compile the model
network3 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",  # loss function to minimize
  metrics = c("accuracy") # monitor classification accuracy
)

# Fit the model
fit5_train <- network3 %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# training error
accuracy <- data.frame(fit5_train$metrics)
train.err.e <- 1-accuracy[5,2]
train.err.e

# test error
metrics <- network3 %>% evaluate(test_images, test_labels, epochs = 5)
metrics <- data.frame(metrics)
test.err.e <- 1-metrics[2,1]
test.err.e

# Repeat with 2 hidden layers, each with 512 hidden units, and 10 epochs
# Fit the model
fit6_train <- network3 %>% fit(train_images, train_labels, epochs = 10, batch_size = 128)

# training error
accuracy <- data.frame(fit6_train$metrics)
train.err.f <- 1-accuracy[5,2]
train.err.f

# test error
metrics <- network3 %>% evaluate(test_images, test_labels, epochs = 10)
metrics <- data.frame(metrics)
test.err.f <- 1-metrics[2,1]
test.err.f

# Repeat with 2 hidden layers, each with 256 hidden units, and 5 epochs
# set up the network architecture
network4 <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# Compile the model
network4 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",  # loss function to minimize
  metrics = c("accuracy") # monitor classification accuracy
)

# Fit the model
fit7_train <- network4 %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# training error
accuracy <- data.frame(fit7_train$metrics)
train.err.g <- 1-accuracy[5,2]
train.err.g

# test error
metrics <- network4 %>% evaluate(test_images, test_labels, epochs = 5)
metrics <- data.frame(metrics)
test.err.g <- 1-metrics[2,1]
test.err.g

# Repeat with 2 hidden layers, each with 256 hidden units, and 10 epochs
# Fit the model
fit8_train <- network4 %>% fit(train_images, train_labels, epochs = 10, batch_size = 128)

# training error
accuracy <- data.frame(fit8_train$metrics)
train.err.h <- 1-accuracy[5,2]
train.err.h

# test error
metrics <- network4 %>% evaluate(test_images, test_labels, epochs = 10)
metrics <- data.frame(metrics)
test.err.h <- 1-metrics[2,1]
test.err.h

# Repeat with L2 weight regularization with lambda = 0.001
model.reg <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28*28),
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 10, activation = "softmax")

# Compile the model
model.reg %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy", 
  metrics = c("accuracy") 
)

# training error
model.reg.train <- model.reg %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)
accuracy <- data.frame(model.reg.train$metrics)
train.err.i <- 1-accuracy[5,2]
train.err.i

# test error
results.reg <- model.reg %>% evaluate(test_images, test_labels)
results.reg <- data.frame(results.reg)
test.err.i <- 1-results.reg[2,1]
test.err.i

# Repeat with 50% dropout
model.dropout <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28*28)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = "softmax")

# Compile and fit
model.dropout %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# training error
model.dropout.train <- model.dropout %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)
accuracy <- data.frame(model.dropout.train$metrics)
train.err.j <- 1-accuracy[5,2]
train.err.j

# test error
results.dropout <- model.dropout %>% evaluate(test_images, test_labels)
results.dropout <- data.frame(results.dropout)
test.err.j <- 1-results.dropout[2,1]
test.err.j

# Make a tabular summary of the results from all the above models and compare them
train.err <- data.frame(cbind(train.err.a, train.err.b, train.err.c, train.err.d, train.err.e, train.err.f, train.err.g, train.err.h, train.err.i, train.err.j))
colnames(train.err) <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)")
test.err <- data.frame(cbind(test.err.a, test.err.b, test.err.c, test.err.d, test.err.e, test.err.f, test.err.g, test.err.h, test.err.i, test.err.j))
colnames(test.err) <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)")
err <- data.frame(rbind(train.err, test.err))
rownames(err) <- c("Training Error","Test Error")
colnames(err) <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)")
err

which.min(test.err)

# Load dataset
boston <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% boston

# Standardize the training and test features
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

# Fit a neural network model with 2 hidden layers, each with 64 hidden units, and 200 epochs
build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data[[2]])) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") 
  )
}

# 4-fold CV
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200
all_mae_histories <- NULL
for (i in 1:k){
  cat("Processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 16, verbose = 0
  )
  
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

# Get the mean 4-fold validation MAE for each epoch
average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

# Make a plot of validation MAE against epoch
plot(validation_mae ~ epoch, average_mae_history, ylim = c(2, 5), 
     type ="l")
# Validation MAE stops improving much after 50 or so

# Fit the final model on all training data with best parameters (epochs = 50)
model <- build_model()
model %>% fit(train_data, train_targets, epochs = 50, 
              batch_size = 16, verbose = 0)

# Evaluate on test data
result <- model %>% evaluate(test_data, test_targets)
mae.a <- result[2]
mae.a
### MAE on test data is about $2690

build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 128, activation = "relu", 
                input_shape = dim(train_data[[2]])) %>%
    layer_dense(units = 1)

  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") # mean absolute error
  )
}

# K-fold CV
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200 
all_scores <- c()
for (i in 1:k){
  cat("Processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model() 
  
  model %>% fit(partial_train_data, partial_train_targets, 
                epochs = num_epochs, batch_size = 16, 
                verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0) 
  all_scores <- c(all_scores, results["mae"])
}
all_scores
mae.b <- mean(all_scores)
mae.b

build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data[[2]]), 
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 64, activation = "relu", 
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1)

  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") # mean absolute error
  )
}

# K-fold CV
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200
all_scores <- c()
for (i in 1:k){
  cat("Processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model() 
  
  model %>% fit(partial_train_data, partial_train_targets, 
                epochs = num_epochs, batch_size = 16, 
                verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0) 
  all_scores <- c(all_scores, results["mae"])
}
all_scores
mae.c <- mean(all_scores)
mae.c

build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 128, activation = "relu", 
                input_shape = dim(train_data[[2]]), 
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") # mean absolute error
  )
}

# K-fold CV
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200 
all_scores <- c()
for (i in 1:k){
  cat("Processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model() 
  
  model %>% fit(partial_train_data, partial_train_targets, 
                epochs = num_epochs, batch_size = 16, 
                verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0) 
  all_scores <- c(all_scores, results["mae"])
}
all_scores
mae.d <- mean(all_scores)
mae.d

# Compare the above models
mae <- data.frame(cbind(mae.a, mae.b, mae.c, mae.d))
rownames(mae) <- "MAE"
colnames(mae) <- cbind("(a)", "(b)", "(c)", "(d)")
mae

# Compute MAE of the recommended model (model (c)) from the test data
build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data[[2]]), 
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 64, activation = "relu", 
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae") # mean absolute error
  )
}

# K-fold CV
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200
all_scores <- c()
for (i in 1:k){
  cat("Processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model() 
  
  model %>% fit(partial_train_data, partial_train_targets, 
                epochs = num_epochs, batch_size = 16, 
                verbose = 0)
  
  results <- model %>% evaluate(test_data, test_targets, verbose = 0) 
  all_scores <- c(all_scores, results["mae"])
}
all_scores
mae.c <- mean(all_scores)
mae.c
# MAE on test data is about $2640 