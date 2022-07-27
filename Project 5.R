## Load the data
library(ISLR)

## Remove missing data
Hitters_1 <- data.frame(Hitters)
Hitters_1 <- na.omit(Hitters)

## Use dummy representation for categorical variables
attach(Hitters_1)
table(League)
table(Division)
table(NewLeague)

Hitters_1$League <- ifelse(League == "A",1,0)
Hitters_1$Division <- ifelse(Division == "E",1,0)
Hitters_1$NewLeague <- ifelse(NewLeague == "A",1,0)

table(League) # 0:N, 1:A
table(Division) # 0:E, 1:W
table(NewLeague) # 0:A, 1:N

## Remove Salary to make this an unsupervised problem
Hitters <- Hitters_1[,-19]

## Check if standardizing is needed
apply(Hitters, 2, mean)
apply(Hitters, 2, sd)
# means and variance vary quite a bit so standardizing is needed

## Standardize the variables & Perform a PCA of the data
data <- subset(Hitters, select = -c(League, Division, NewLeague))
pca <- prcomp(data, center = T, scale = T)
summary(pca)
# The first three components explain 81.8% of the variation in the data.

## Create a scree plot that tells the number of PCs to keep
# Compute the proportion of variance explained (PVE)
pca.var <- pca$sdev^2
pve = pca.var/sum(pca.var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,0.5), type = 'b')
# After PC3, pve form a straight line

## Loading matrix
pca$rotation

## Plot the weight of the original variables in each PC
library(ggplot2)
rot <- data.frame(pca$rotation)
rot$feature <- rownames(rot)

# PC1
rot$feature <- factor(rot$feature, levels= rot$feature[order(rot$PC1, decreasing=T)])
ggplot(rot, aes(feature, PC1)) + 
  geom_bar(stat="identity", position="identity")

# PC2
rot$feature <- factor(rot$feature, levels= rot$feature[order(rot$PC2, decreasing=T)])
ggplot(rot, aes(feature, PC2)) + 
  geom_bar(stat="identity", position="identity")

# PC3
rot$feature <- factor(rot$feature, levels= rot$feature[order(rot$PC3, decreasing=T)])
ggplot(rot, aes(feature, PC3)) + 
  geom_bar(stat="identity", position="identity")

# PC1: CAtBat ~ CWalks + years load more heavily on the 1st pc compared to others
# -> measures of # of records during his career + number of years in the major league
# PC2: AtBat, Hits, Runs, RBI are primary measurements
# -> measure of # of times at bat & hits & runs batted in 1986
# PC3: has large negative associations with Assists, Errors
# -> measure of assists and errors in 1986

## head of score matrix
head(pca$x, 5)

cor(pca$x)[1:2,1:2]
biplot(pca, scale=0)
# PC1: CAtBat ~ CWalks + years 
# PC2: AtBat, Hits, Runs, RBI

## Standardize the variables
Hitters <- data.frame(sapply(Hitters, scale))

## Hierarchically cluster the players using complete linkage and Euclidean distance
dist <- dist(Hitters, method = "euclidean")
hc <- hclust(dist, method = "complete")
hc
hc$merge

## Plot a dendogram
plot(hc, main = "Complete Linkage", xlab = "", sub = "",
     cex = 0.7)

## Cut the dendogram at a height that results in two distinct clusters
twoclusters <- cutree(hc, k = 2)
rect.hclust(hc, k=2, border = "red")
table(twoclusters)

## Summarize the cluster-specific means of the variables
mean1 <- data.frame(colMeans(subset(Hitters_1[1:20], twoclusters == 1)))
mean2 <- data.frame(colMeans(subset(Hitters_1[1:20], twoclusters == 2)))
mean <- data.frame(cbind(mean1, mean2))
colnames(mean) <- c("cluster 1", "cluster 2")
mean

## summarize the mean salaries of the players in the two clusters
salary1 <- colMeans(subset(Hitters_1[19], twoclusters == 1))
salary2 <- colMeans(subset(Hitters_1[19], twoclusters == 2))
salarymean <- cbind(salary1, salary2)
colnames(salarymean) <- c("cluster 1","cluster 2")
salarymean

set.seed(123)
kmean <- kmeans(Hitters, 2, nstart = 10)
kmean

kmeanCluster <- kmean$cluster

salary1 <- colMeans(subset(Hitters_1[19], kmeanCluster == 1))
salary2 <- colMeans(subset(Hitters_1[19], kmeanCluster == 2))
salarymean <- cbind(salary1, salary2)
colnames(salarymean) <- c("cluster 1","cluster 2")
salarymean

Hitters <- Hitters_1[,-21]

## Fit a linear regression model with log(Salary) as response
lin.fit <- glm(log(Salary) ~ ., data = Hitters)
summary(lin.fit)

## Compute the test MSE of the model
library(boot)
lin.fit.err <- cv.glm(Hitters, lin.fit)
lin.fit.mse <- lin.fit.err$delta[1]

## Create training and test sets
library(pls)
y <- log(Hitters$Salary)
x <- model.matrix(log(Salary) ~ ., Hitters)[, -1]
set.seed(1)

## Choose M optimally via LOOCV to fit a PCR model
set.seed(1)
pcr.fit <- pcr(log(Salary) ~ ., data = Hitters, scale = TRUE, validation = "CV", segments = 10)

validationplot(pcr.fit, val.type = "MSEP")
MSEP(pcr.fit)
which.min(MSEP(pcr.fit)$val[1, 1,])
# We see that lowest cross-validation error occurs when M = 16

## PCR model with M = 16
pcr.fit <- pcr(log(Salary) ~ ., data = Hitters, scale = TRUE, validation = "CV", ncomp = 16, segments = 10)
summary(pcr.fit)

## Compute the test MSE for M = 16
pcr.pred <- predict(pcr.fit, ncomp = 16)
pcr.fit.mse <- mean((pcr.pred - y)^2)
pcr.fit.mse

## Choose M optimally via LOOCV to fit a PLS model
set.seed(1)
pls.fit <- plsr(log(Salary) ~ ., data = Hitters, scale = TRUE, validation = "CV", segments = 10)

validationplot(pls.fit, val.type = "MSEP")
MSEP(pls.fit)
which.min(MSEP(pls.fit)$val[1, 1,])
# We see that lowest cross-validation error occurs when M = 11

## Fit a PLS model with M = 11
pls.fit <- plsr(log(Salary) ~ ., data = Hitters, scale = TRUE, validation = "CV", ncomp = 11, segments = 10)
summary(pls.fit)

## Compute the test MSE for M = 11
pls.pred <- predict(pls.fit, ncomp = 11)
pls.fit.mse <- mean((pls.pred - y)^2)
pls.fit.mse

## Fit a ridge regression with penalty parameter chosen optimally via LOOCV
library(glmnet)
y <- log(Hitters$Salary)
x <- model.matrix(log(Salary) ~ ., Hitters)[, -1]
rid.fit <- glmnet(x, y, alpha = 0)
plot(rid.fit, xvar = "lambda")

## Compute the test MSE of the model
set.seed(1)
cv <- cv.glmnet(x, y, alpha = 0, grouped = FALSE)
plot(cv)

# find the best value of lambda
bestlam <- cv$lambda.min
bestlam

# get the test MSE for the best value of lambda
set.seed(1)
rid.fit.err <- sapply(1:100, FUN = function(i) {
  rid.pred <- predict(rid.fit, s = cv$lambda[i], newx =x)
  err <- mean((rid.pred[i] - y)^2)
  return(err)})
rid.fit.mse <- min(rid.fit.err)

## Compare the four models. Which model(s) would you recommend?
mse <- data.frame(cbind(lin.fit.mse, pcr.fit.mse, pls.fit.mse, rid.fit.mse))
colnames(mse) <- c("Linear", "PCR", "PLS", "Ridge")
rownames(mse) <- c("Test MSE")
mse