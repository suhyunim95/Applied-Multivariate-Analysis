# load & explore the data
adm = read.csv("admission.csv")
adm <- data.frame(adm)

str(adm)
head(adm)
summary(adm)

# split training & test data
train <- adm[6:85,]
test <- adm[1:5,]

attach(adm)
train.x <- train[,-3]
train.y <- train[,3]
test.x <- test[,-3]
test.y <- test[,3]

dim(train.x)
table(train.y)

# EDA of the training data
par(mfrow = c(1,1))
plot(train.x, xlab = "GPA", ylab = "GMAT", 
     col = train.y,
     main = "Classification of Applicants")
legend("topleft", 
       legend = c("Group 1: admit", "Group 2: do not admit", "Group 3: borderline"), 
       col = c("1", "2", "3"), 
       pch = 1, 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.05, 0.05))

par(mfrow = c(1,2))
boxplot(train.x[,"GPA"] ~ train.y, title = "GPA of Each Group of Applicants", xlab = "Group", ylab = "GPA", ylim = c(2,4), col=c("gray","red","green"))
boxplot(train.x[,"GMAT"] ~ train.y, title = "GMAT of Each Group of Applicants", xlab = "Group", ylab = "GMAT", ylim = c(300,700), col=c("gray","red","green"))
par(mfrow = c(1,1))

# perform LDA using the training data
library(MASS)
lda.fit = lda(Group ~ GPA + GMAT, data = train)
lda.fit

# decision boundary on the plot
# Set up a dense grid and compute posterior prob on the grid
n.grid <- 50
x1.grid <- seq(f = min(train.x[, 1]), t = max(train.x[, 1]), l = n.grid)
x2.grid <- seq(f = min(train.x[, 2]), t = max(train.x[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(train.x)

pred.grid <- predict(lda.fit, grid)

prob1 <- matrix(pred.grid$posterior[, "1"], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(pred.grid$posterior[, "2"], nrow = n.grid, ncol = n.grid, byrow = F)

plot(train.x, col = train.y)

contour(x1.grid, x2.grid, prob1, levels = 0.5, add = T, col = "black")
contour(x1.grid, x2.grid, prob2, levels = 0.5, add = T, col = "red")

# confusion matrix of the training data
lda.pred.train = predict(lda.fit, train)
table(lda.pred.train$class, train.y)

# overall misclassification rate of the training data
mean(lda.pred.train$class != train.y)

# confusion matrix of the test data
lda.pred.test = predict(lda.fit, test)
table(lda.pred.test$class,test.y)

# overall misclassification rate of the test data
mean(lda.pred.test$class != test.y)

# perform QDA using the training data
qda.fit <- qda(Group ~ GPA + GMAT, data = train)
qda.fit

# decision boundary on the plot
# Set up a dense grid and compute posterior prob on the grid
n.grid <- 50
x1.grid <- seq(f = min(train.x[, 1]), t = max(train.x[, 1]), l = n.grid)
x2.grid <- seq(f = min(train.x[, 2]), t = max(train.x[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(train.x)

pred.grid <- predict(qda.fit, grid)

prob1 <- matrix(pred.grid$posterior[, "1"], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(pred.grid$posterior[, "2"], nrow = n.grid, ncol = n.grid, byrow = F)

plot(train.x, col = train.y)

contour(x1.grid, x2.grid, prob1, levels = 0.5, add = T, col = "black")
contour(x1.grid, x2.grid, prob2, levels = 0.5, add = T, col = "red")

# confusion matrix of the training data
qda.pred.train = predict(qda.fit, train)
table(qda.pred.train$class, train.y)

# overall misclassification rate of the training data
mean(qda.pred.train$class != train.y)

# confusion matrix of the test data
qda.pred.test = predict(qda.fit, test)
table(qda.pred.test$class,test.y)

# overall misclassification rate of the test data
mean(qda.pred.test$class != test.y)
