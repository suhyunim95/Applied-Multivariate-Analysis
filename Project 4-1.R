## Load the data
wine = read.csv("wine.csv")
wine <- data.frame(wine)

attach(wine)
train.x <- wine[,-6]
train.y <- wine[,6]

## Fit a linear regression model using all predictors
fit.a <- glm(Quality ~ ., data = wine)
summary(fit.a)
fit.a.est <- coef(fit.a)

## Compute the test error rate using LOOCV
library(boot)
err.a <- cv.glm(wine, fit.a)
err.a$delta[1]

## Use best-subset selection based on adjusted R^2 to find the best linear regression model
library(leaps)
fit.full <- regsubsets(Quality ~ ., wine, nvmax = 6)
fit.summary <- summary(fit.full)

# Plot the number of variables of the best linear regression with adjusted R squared
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
points(which.max(fit.summary$adjr2), fit.summary$adjr2[which.max(fit.summary$adjr2)], col = "red", cex = 2, pch = 20)

# find those 4 variables
plot(fit.full, scale = "adjr2")

## Compute the test error rate of the best model using LOOCV
fit.b <- glm(Quality ~ Clarity + Aroma + Flavor + Oakiness, data = wine)
err.b <- cv.glm(wine, fit.b)
err.b$delta[1]

# find each parameter estimates
fit.b.est <- coef(fit.b)

## Use forward stepwise selection
fit.fwd= regsubsets(Quality ~ ., data = wine, nvmax = 6, 
                    method = "forward")
fit.fwd.summary <- summary(fit.fwd)

## based on adjusted R2 to find the best linear regression model
plot(fit.fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
points(which.max(fit.fwd.summary$adjr2), fit.fwd.summary$adjr2[which.max(fit.fwd.summary$adjr2)], col = "red", cex = 2, pch = 20)
# Adjusted R^2 is the largest when the number of variables is 4.

## Compute the test error rate of the best model using LOOCV
fit.c <- glm(Quality ~ Clarity + Aroma + Flavor + Oakiness, data = wine)
err.c <- cv.glm(wine, fit.c)
err.c$delta[1]

# find each parameter estimates
fit.c.est <- coef(fit.c)

## Use backward stepwise selection
fit.bwd = regsubsets(Quality ~ ., data = wine, nvmax = 6, 
                     method = "backward")
bwdfit.summary <- summary(fit.bwd)

## based on adjusted R2 to find the best linear regression model
plot(bwdfit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
points(which.max(bwdfit.summary$adjr2), bwdfit.summary$adjr2[which.max(bwdfit.summary$adjr2)], col = "red", cex = 2, pch = 20)
# Adjusted R^2 is the largest when the number of variables is 4.

## Compute the test error rate of the best model using LOOCV
fit.d <- glm(Quality ~ Clarity + Aroma + Flavor + Oakiness, data = wine)
err.d <- cv.glm(wine, fit.d)
err.d$delta[1]

# find each parameter estimates
fit.d.est <- coef(fit.d)

## Use ridge regression with penalty parameter chosen optimally via LOOCV to fit a linear regression model
library(glmnet)

y <- wine$Quality
x <- model.matrix(Quality ~ ., wine)[, -1]

# Set up a grid of lambda values
grid <- 10^seq(10, -2, length = 100)

# Fit ridge regression for each lambda on the grid
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))

plot(ridge.mod, xvar = "lambda")

## Compute the test MSE of each lambda using LOOCV
set.seed(1)
cv <- cv.glmnet(x, y, alpha = 0, grouped = FALSE)
plot(cv)

# find the best value of lambda
bestlam <- cv$lambda.min
bestlam

# get the test MSE for the best value of lambda
err.e <- sapply(1:26, FUN = function(i) {
  ridge.pred <- predict(ridge.mod, s = cv$lambda[i], newx =x)
  err <- mean((ridge.pred[i] - y)^2)
  return(err)})
min(err.e)

# we refit our ridge regression model on the full data set, 
# using the value of  ??  chosen by cross-validation, 
# and examine the coefficient estimates.
fit.e.est <- predict(cv, type = "coefficients", s = bestlam)[1:7,]

## Use lasso with penalty parameter chosen optimally via LOOCV to fit a linear regression model
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.mod, xvar = "lambda")

## Compute the test error rate of the best model using LOOCV
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1, grouped = FALSE)
plot(cv.out)

# find the best value of lambda
bestlam <- cv.out$lambda.min
bestlam

# Test MSE for the best value of lambda
err.f <- sapply(1:26, FUN = function(i) {
  lasso.pred <- predict(lasso.mod, s = cv$lambda[i], newx =x)
  err <- mean((lasso.pred[i] - y)^2)
  return(err)})

min(err.f)

# find each parameter estimates of the ridge regression model using the best lambda
out <- glmnet(x, y, alpha = 1, lambda = grid)
fit.f.est <- predict(out, type = "coefficients", s = bestlam)[1:7, ]
# Lasso chooses the model with Aroma, Flavor, and Region.

## Make a tabular summary of the parameter estimates and test error rates from (a)-(f).
fit.b.est <- coef(fit.b)
b <- c <- d  <- c(4.2718302, 2.5715050, 0.6159385, 0, 1.1654980, -0.6246152 , 0)
par.est <- cbind(fit.a.est, b, d, d, fit.e.est, fit.f.est)
par.est <- data.frame(par.est)

err <- cbind(err.a, err.b, err.c, err.d, err.e, err.f)
err <- data.frame(err)

row.names(par.est[8,]) <- c("Test MSE")
par.est[8,] <- err

names(par.est) <- c("a","b","c","d","e","f")
par.est