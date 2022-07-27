# Load the data
diabetes <- read.csv("diabetes.csv")
diabetes <- data.frame(diabetes)

# take Outcome as the binary response
diabetes$Outcome <- as.factor(diabetes$Outcome)

# Fit a support vector classifier to the data with cost parameter chosen optimally
library(e1071)
set.seed(1)
tune.out <- tune(svm, Outcome ~ ., data = diabetes, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# cost = 0.01

# Summarize key features of the fit
tune.out$best.model

# test MSE using k folds
data <- diabetes[sample(nrow(diabetes)),]
folds <- cut(seq(1,nrow(data)), breaks=10, labels=FALSE)

mse <- rep(0,10)
for(i in 1:10){
  i <- which(folds == i, arr.ind = TRUE)
  dataTest <- data[i, ]
  dataTrain <- data[-i, ]
  tune.out <- tune(svm, Outcome ~ ., data = dataTrain, kernel = "linear", cost = 0.01)
  bestmod <- tune.out$best.model
  pred <- predict(bestmod, dataTest)
  mse[i] <- mean(pred != dataTest$Outcome)
}
mse.a <- mean(mse)
mse.a

# Fit a support vector machine with a polynomial kernel of degree two and cost parameter chosen optimally
set.seed(1)
tune.poly <- tune(svm, Outcome ~ ., data = diabetes, kernel = "polynomial", degree = 2, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.poly)
# cost = 5

# Summarize key features of the fit
tune.poly$best.model

# test MSE using 10-fold cv 
mse <- rep(0,10)
for(i in 1:10){
  i <- which(folds == i, arr.ind = TRUE)
  dataTest <- data[i, ]
  dataTrain <- data[-i, ]
  tune.out <- tune(svm, Outcome ~ ., data = dataTrain, kernel = "polynomial", degree = 2, cost = 5)
  bestmod <- tune.out$best.model
  pred <- predict(bestmod, dataTest)
  mse[i] <- mean(pred != dataTest$Outcome)
}
mse.b <- mean(mse)
mse.b

# Fit a support vector machine with a radial kernel with both and cost parameter chosen optimally
set.seed(1)
tune.radial <- tune(svm, Outcome ~ ., data = diabetes, kernel = "radial", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.radial)
# cost = 1, gamma = 5

# Summarize key features of the fit
tune.radial$best.model

# test MSE using 10-fold cv 
mse <- rep(0,10)
for(i in 1:10){
  i <- which(folds == i, arr.ind = TRUE)
  dataTest <- data[i, ]
  dataTrain <- data[-i, ]
  tune.out <- tune(svm, Outcome ~ ., data = dataTrain, kernel = "radial", cost = 1, gamma = 5)
  bestmod <- tune.out$best.model
  pred <- predict(bestmod, dataTest)
  mse[i] <- mean(pred != dataTest$Outcome)
}
mse.c <- mean(mse)
mse.c
# 0.011

mse <- data.frame(cbind(mse.a, mse.b, mse.c))
rownames(mse) <- "Test MSE"
colnames(mse) <- cbind("Classifier", "Polynomial", "Radial")
mse