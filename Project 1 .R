# read data
train <- read.csv("training data.csv")
test <- read.csv("test data.csv")

# explore training data
head(train)

# split x & y 
train.x <- train[, -3]
train.y <- train[, 3]
test.x <- test[, -3]
test.y <- test[, 3]

# KNN with K = 1,6,...,200
K <- c(seq(from=1, to=200, by=5))
K.size <- length(K)
train.err <- rep(NA, K.size)
test.err <- rep(NA, K.size)

library(class)
for (i in seq_along(K)) {
  set.seed(1)
  knn.train <- knn(train=train.x, test=train.x, cl=train.y, k=i)
  train.err[i] <- mean(knn.train != train.y)
  
  set.seed(1)
  knn.test <- knn(train=train.x, test=test.x, cl=train.y, k=i)
  test.err[i] <- mean(knn.test != test.y)
}

# training & test error rates with K = 1,6,...,200
err <- data.frame(K, train.err, test.err)
rownames.err <- c("k", "training error", "test error")
err

# plot training & test error rates against K = 1,6,...,200 
library(ggplot2)
plot.err <- ggplot(err, aes(x=K)) + 
  geom_line(aes(y=train.err, color="maroon"), size=1) + 
  geom_line(aes(y=test.err, color="olivedrab"), size=1) +
  geom_point(aes(y=train.err, color="maroon")) +
  geom_point(aes(y=test.err, color="olivedrab")) + 
  theme(legend.position="bottom") +
  scale_color_discrete(name="Error Rates", labels = c("train.err", "test.err"))

plot.err

# find the optimal K
opt <- err[test.err == min(err$test.err), ]
opt

knn.opt <- knn(train=train.x, test=test.x, cl=train.y, k=121)
table(knn.opt, test.y)

# plot training data with the decision boundary for optimal K=121
x1.grid <- seq(from = min(train.x[, 1]), to = max(train.x[, 1]), length = 50)
x2.grid <- seq(from = min(train.x[, 2]), to = max(train.x[, 2]), length = 50)
grid <- expand.grid(x1.grid, x2.grid)
knn.121 <- knn(train=train.x, test=grid, cl=train.y, k=121, prob=TRUE)
p <- attr(knn.121, "p")
p <- ifelse(knn.121 == "yes", p, 1-p)
p <- matrix(p, 50, 50)

plot(train.x, col=ifelse(train.y == "yes", "olivedrab", "maroon"))
contour(x1.grid, x2.grid, p, levels = 0.5, add = T)

####################

library(keras)

cifar <- dataset_cifar10()
str(cifar)

x.train <- cifar$train$x
y.train <- cifar$train$y
x.test <- cifar$test$x
y.test <- cifar$test$y

# reshape the images as vectors (column-wise)
# (aka flatten or convert into wide format)
# (for row-wise reshaping, see ?array_reshape)
dim(x.train) <- c(nrow(x.train), 32*32*3) # 50000 x 3072
dim(x.test) <- c(nrow(x.test), 32*32*3) # 50000 x 3072

# rescale the x to lie between 0 and 1
x.train <- x.train/255
x.test <- x.test/255

# categorize the response
y.train <- as.factor(y.train)
y.test <- as.factor(y.test)

# randomly sample 1/100 of test data to reduce computing time
set.seed(2021)
id.test <- sample(1:10000, 100)
x.test <- x.test[id.test,]
y.test <- y.test[id.test]

# KNN with K = 50,100,200,300,400
K <- c(50,100,200,300,400)
train.err <- rep(NA, 5)
test.err <- rep(NA, 5)

library(class)
for (i in 1:5) {
  set.seed(2021)
  knn.test <- knn(train=x.train, test=x.test, cl=y.train, k=K[i])
  test.err[i] <- mean(knn.test != y.test)
}

# test error rates with K
err <- data.frame(K, test.err)
rownames.err <- c("k", "Test Error")
err

# KNN with K = 50,60,75,85,100,200,250,300,400
K <- c(50,60,75,85,100,200,250,300,400)
train.err <- rep(NA, 9)
test.err <- rep(NA, 9)

library(class)
for (i in 1:9) {
  set.seed(2021)
  knn.test <- knn(train=x.train, test=x.test, cl=y.train, k=K[i])
  test.err[i] <- mean(knn.test != y.test)
}

# test error rates with K
err <- data.frame(K, test.err)
rownames.err <- c("k", "Test Error")
err

# confusion matrix for the best value of K=75
confusionMatrix <- table(knn.test.75, y.test)
confusionMatrix

# calculate sum of matirx elements to evaluate the model
sum(diag(confusionMatrix))
sum(confusionMatrix)