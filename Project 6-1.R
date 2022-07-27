# Load the data
library(tree)
library(ISLR)

# Remove missing data
Hitters <- data.frame(Hitters)
Hitters <- na.omit(Hitters)

# Change League, Division, New League to factor
Hitters$League <- as.factor(Hitters$League)
Hitters$Division <- as.factor(Hitters$Division)
Hitters$NewLeague <- as.factor(Hitters$NewLeague)

# Take log(Salary) as response & the remaining variables as predictors
Hitters$Salary <- log(Hitters$Salary)

# Fit a tree to the data
tree.hitters <- tree(Salary ~ ., Hitters)

# Summarize the results
summary(tree.hitters)
# ??? R1: CAtBat < 1452
# R2
# R3

# Plot the results
plot(tree.hitters)
text(tree.hitters, pretty = 0, cex = 0.7)

# Test MSE using LOOCV
tree.mse <- rep(0,nrow(Hitters))
for(i in 1:nrow(Hitters)) {
  dataTest = Hitters[i, ]
  dataTrain = Hitters[-i, ]
  pred <- data.frame(predict(tree(Salary ~ ., dataTrain)), dataTest)[,1]
  tree.mse[i] <- mean((dataTest[,19] - pred)^2)}

tree.mse <- mean(tree.mse)
# 1.418271

# Use LOOCV to determine whether pruning is helpful and determine the optimal size for the pruned tree.
cv.hitters <- cv.tree(tree.hitters, FUN = prune.tree)

# Find the optimal size for the pruned tree
cv.hitters
plot(cv.hitters$size, cv.hitters$dev, type = "b")
# best size = 8

# Fit and plot the best pruned tree
pruned.hitters = prune.tree(tree.hitters, best = 8)
par(mfrow = c(1,1))
plot(pruned.hitters)
text(pruned.hitters, pretty = 0)

# Test MSE using LOOCV
pruned.mse <- rep(0,nrow(Hitters))
for(i in 1:nrow(Hitters)) {
  dataTest = Hitters[i, -11]
  dataTrain = Hitters[-i, -11]
  pred <- data.frame(predict(tree(Salary ~ ., dataTrain)), dataTest)[,1]
  pruned.mse[i] <- mean((dataTest[,18] - pred)^2)}

pruned.mse <- mean(pruned.mse)
# 1.417849

# unpruned tree is better
# Cruns is important

# Use a bagging approach to analyze the data with B = 1000
library(randomForest)
set.seed(1)
bag.hitters <- randomForest(Salary ~ ., data = Hitters, mtry = 13, ntree = 1000, importance = TRUE)
bag.hitters

# Test MSE using LOOCV
bag.mse <- rep(0,nrow(Hitters))
for(i in 1:nrow(Hitters)) {
  dataTest = Hitters[i, ]
  dataTrain = Hitters[-i, ]
  pred <- data.frame(predict(bag.hitters, dataTrain), dataTest)[,1]
  bag.mse[i] <- mean((dataTest[,19] - pred)^2)}

bag.mse <- mean(bag.mse)
#1.463097

# Important predictors
importance(bag.hitters)
# CAtBat, CHits, CRuns 

# Use a random forest approach to analyze the data with B = 1000 and m= p=3.
set.seed(1)
rf.hitters <- randomForest(Salary ~ ., data = Hitters, mtry = 6, ntree = 1000, importance = TRUE)
rf.hitters

# Test MSE using LOOCV
rf.mse <- rep(0,nrow(Hitters))
for(i in 1:nrow(Hitters)) {
  dataTest = Hitters[i, ]
  dataTrain = Hitters[-i, ]
  pred <- data.frame(predict(rf.hitters, dataTrain), dataTest)[,1]
  rf.mse[i] <- mean((dataTest[,19] - pred)^2)}

rf.mse <- mean(rf.mse)
# 1.45982

# Important predictors
importance(rf.hitters)
# CAtBat, CHits, CRuns 

# Use a boosting approach to analyze the data with B = 1000, d = 1, and lambda = 0.01.
library(gbm)
set.seed(1)
boost.hitters <- gbm(Salary ~ ., data = Hitters, distribution = "gaussian", 
                    n.trees = 1000, interaction.depth = 1, shrinkage = 0.01)

# Test MSE using LOOCV
boost.mse <- rep(0,nrow(Hitters))
for(i in 1:nrow(Hitters)) {
  dataTest = Hitters[i, ]
  dataTrain = Hitters[-i, ]
  pred <- data.frame(predict(boost.hitters, dataTrain), dataTest)[,1]
  boost.mse[i] <- mean((dataTest[,19] - pred)^2)}

boost.mse <- mean(boost.mse)
#1.353204

# important predictors
summary(boost.hitters)
# CAtBat, CHits, CRuns 

mse <- data.frame(cbind(tree.mse, pruned.mse, bag.mse, rf.mse, boost.mse))
rownames(mse) <- "Test MSE"
colnames(mse) <- cbind("Full Tree", "Pruned", "Bagging", "Random Forest", "Boosting")
mse