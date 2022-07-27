## Load the data
diabetes = read.csv("diabetes.csv")
diabetes <- data.frame(diabetes)

attach(diabetes)
train.x <- diabetes[,-9]
train.y <- diabetes[,9]

# rename column names
colnames(diabetes)
names(diabetes) <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                     "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

## Fit a logistic regression model using all predictors
fit.a <- glm(Outcome ~ ., family = binomial, data = diabetes)
coef(fit.a)

## Compute its test error rate using 10-fold cross-validation
library(boot)
fit.a = glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness 
            + Insulin + BMI + DiabetesPedigreeFunction + Age, 
            data = diabetes, family = binomial)
err.a = cv.glm(diabetes, fit.a, K = 10)$delta[1]
err.a

## Use best-subset selection
# Based on AIC find the best logistic regression model
library(bestglm)
set.seed(125) 
fit.aic <- bestglm(Xy = diabetes, family = binomial, IC = "AIC", method = "exhaustive")
summary(fit.aic)

fit.aic$BestModels
fit.b <- fit.aic$BestModel
summary(fit.b)
formula(fit.b)
as.formula(fit.b$call)
fit.b.est <- coef(fit.b)

# Compute its test error rate using 10-fold cross-validation
library(caret)
diabetes$Outcome <- as.factor(diabetes$Outcome)
train_control <- trainControl(method = "cv", number = 10)
model <- train(Outcome ~ ., data = diabetes, method = "glm", family = "binomial", trControl = train_control)
err.b <- 1 - model$results[2]
err.b

## Use forward stepwise selection
fit.null = glm(Outcome ~ 1, data = diabetes, family = binomial)
fit.all = glm(Outcome ~ ., data = diabetes, family = binomial)
fit.c <- step(fit.null, scope = list(upper = fit.all, lower= fit.null), direction = "forward", 
              k = 2, trace = FALSE)
fit.c   

# Compute its test error rate using 10-fold cross-validation
err.c = rep(0,10)
for(i in 1:10){
  err.c[i] = cv.glm(diabetes, fit.c, K=10)$delta[1]
}
err.c

## Use backward stepwise selection
fit.d <- step(fit.all, direction = "backward", k = 2, trace = FALSE)

# Compute its test error rate using 10-fold cross-validation
err.d = rep(0,10)
for(i in 1:10){
  err.d[i] = cv.glm(diabetes, fit.d, K=10)$delta[1]
}
err.d

## Use ridge regression 
library(glmnet)
y <- diabetes$Outcome
x <- model.matrix(Outcome ~ ., diabetes)[, -1]

fit.e <- cv.glmnet(x, y, family = "binomial", nfolds=10, alpha = 0)
plot(fit.e)

# Find the best value of lambda
bestlam <- fit.e$lambda.min
bestlam

# Compute its test error rate using 10-fold cross-validation
ridge.pred <- ifelse(predict(fit.e, s = bestlam, newx = x) > 0.5, "1", "0")
cm.e <- table(ridge.pred, y)

err.e <- (cm.e[2,1] + cm.e[1,2]) / (cm.e[1,1] + cm.e[1,2] + cm.e[2,1] + cm.e[2,2])

## Use lasso
fit.f <- cv.glmnet(x, y, family = "binomial", nfolds=10, alpha = 1)
plot(fit.f)

# Find the best value of lambda
bestlam <- fit.f$lambda.min
bestlam

# Compute its test error rate using 10-fold cross-validation
lasso.pred <- ifelse(predict(fit.f, s = bestlam, newx = x) > 0.5, "1", "0")
cm.f <- table(lasso.pred, y)

err.f <- (cm.f[2,1] + cm.f[1,2]) / (cm.f[1,1] + cm.f[1,2] + cm.f[2,1] + cm.f[2,2])

## Make a tabular summary of the parameter estimates and test error rates from (a)-(f).
fit.a.est <- coef(fit.a)
fit.b.est <- coef(fit.b)
fit.c.est <- coef(fit.c)
fit.d.est <- coef(fit.d)
fit.e.est <- coef(fit.e)
fit.f.est <- coef(fit.f)
e <- c(-6.1065494968, 0.0866146751, 0.0230372668, -0.0043077182, 0.0005355078, -0.0001758245 , 0.0524168971, 0.6168612853, 0.0156865889)
f <- c(-6.03813451, 0.07919931, 0.02645360, 0, 0, 0, 0.04419372, 0.29288073, 0.00549285)

par.est <- cbind(fit.a.est, fit.b.est, fit.c.est, fit.d.est, e, f)
par.est <- data.frame(par.est)

err <- cbind(err.a, err.b, err.c, err.d, err.e, err.f)
err <- data.frame(err)

row.names(par.est[10,]) <- c("Test MSE")
par.est[10,] <- err

names(par.est) <- c("a","b","c","d","e","f")
par.est