# load the data
diabetes = read.csv("diabetes.csv")
diabetes <- data.frame(diabetes)
set.seed(1)

# split predictors and response
train.x <- diabetes[,-9]
train.y <- diabetes[,9]
train.y <- data.frame(train.y)

# rename column names
colnames(diabetes)
names(diabetes) <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness",
                     "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

# treat Outcome as qualitative predictor
attach(diabetes)
Outcome <- as.factor(Outcome)
table(Outcome)

# Explanatory data analysis
str(diabetes)
head(diabetes)
summary(diabetes)
pairs(diabetes)
par(mfrow=c(4,2))
plot1 <- boxplot(Pregnancies ~ Outcome, xlab = "Outcome", ylab = "Pregnancies", main = "Pregnancies vs. Outcome")
plot2 <- boxplot(Glucose ~ Outcome, xlab = "Outcome", ylab = "Glucose", main = "Glucose vs. Outcome")
plot3 <- boxplot(BloodPressure ~ Outcome, xlab = "Outcome", ylab = "BloodPressure", main = "BloodPressure vs. Outcome")
plot4 <- boxplot(SkinThickness ~ Outcome, xlab = "Outcome", ylab = "SkinThickness", main = "SkinThickness vs. Outcome")
plot5 <- boxplot(Insulin ~ Outcome, xlab = "Outcome", ylab = "Insulin", main = "Insulin vs. Outcome")
plot6 <- boxplot(BMI ~ Outcome, xlab = "Outcome", ylab = "BMI", main = "BMI vs. Outcome")
plot7 <- boxplot(DiabetesPedigreeFunction ~ Outcome, xlab = "Outcome", ylab = "DiabetesPedigreeFunction", main = "DPF vs. Ou
tcome")
plot8 <- boxplot(Age ~ Outcome, xlab = "Outcome", ylab = "Age", main = "Age vs. Outcome")

# build a logistic regression model
fit1 <- glm(Outcome ~ ., family = binomial, data = diabetes)
fit1
summary(fit1)

# remove SkinThickness
fit2 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age, family = binom
            ial, data = diabetes)
fit2
summary(fit2)
anova(fit2, fit1, test = "Chisq")

# The SkinThickness is not a significant predictor
# keep fit2

# remove Age, Insulin
fit3 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + BMI + DiabetesPedigreeFunction, family = binomial, data = diab
            etes)
fit3
summary(fit3)
anova(fit3, fit2, test = "Chisq")
# fit2 provides a significantly better fit than fit3
# removing Age and Insulin was not a good idea
# keep fit2

# let's remove Age only
fit4 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction, family = binomial, d
            ata = diabetes)
fit4
summary(fit4)
anova(fit4, fit2, test = "Chisq")

# since fit2 is still better than fit4, keep Age and remove Insulin
fit5 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure+ BMI + DiabetesPedigreeFunction + Age, family = binomial, data =
              diabetes)
fit5
summary(fit5)
anova(fit5, fit2, test = "Chisq")
# removing Insulin doesn't improve the model
# there is no more predictor that I want to remove, since their p-values are really small which means every predictor is significant
# therefore fit2 is the best

# fit2 in equation form
# probability = 1/(1 + exp(-[-8.0273146 + 0.1263707 * x1 + 0.0336810 * x2 - 0.0095806 * x3
# -0.0012123 * x4 + 0.0778743 * x5 + 0.8894946 * x6 + 0.0128944 * x7]))

# estimates of the regression coefficients
summary(fit2)$coefficients[,1]

# the standard errors of the estimates
summary(fit2)$coefficients[,2]

# 95% confidence intervals of the coefficients
confint.default(fit2)

# training error rates
fit2 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age, family = binom
            ial, data = diabetes)
fit2_pred = ifelse(predict(fit2, type = "response") > 0.5, "1", "0")
errorRate = function(actual, predict) {
  mean(actual != predict)
}
errorRate(actual = train.y, predict = fit2_pred)

# Estimated probabilities for test data
probability <- predict(fit1, train.x, type = "response")

# Predicted classes (using 0.5 cutoff)
pred.class <- ifelse(probability >= 0.5, "1", "0")

# error rate
mean(pred.class != train.y)

# Confusion matrix and (sensitivity, specificity)
table(pred.class, train.y)
c(1180/(1180 + 136), 388/(388 + 296))

error <- 0
for (i in 1:2000) {
  fit1 <- glm(Outcome ~ ., data = diabetes[-i,], family = binomial)
  pred.1 <- ifelse(predict(fit1, diabetes[i,], type = "response") > 0.5, "1", "0")
}
mean(pred.1 != train.y)

# load the libraries
library(caret)
train.control <- trainControl(method = "LOOCV")
model1 <- train(Outcome ~ ., data = diabetes, method = "glm", trControl = train.control)
model1$results

model2 <- train(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age,
                data = diabetes, method = "glm", trControl = train.control)
model2$results
rmse.model2.loocv <- model2$results[1,2]

# perform LDA using the training data
library(MASS)
lda.fit = lda(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age,
              data = diabetes, CV = TRUE)

# confusion matrix
table(lda.fit$class, train.y)

# overall misclassification rate (rmse)
rmse.lda <- sqrt(mean(lda.fit$class != train.y))
rmse.lda

# perform QDA using the training data
qda.fit = qda(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age,
              data = diabetes, CV = TRUE)

# confusion matrix
table(qda.fit$class, train.y)

# overall misclassification rate (rmse)
rmse.qda <- sqrt(mean(qda.fit$class != train.y))
rmse.qda

# Fit a KNN using the LOOCV estimate of test error rate
knnFit <- train(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age,
                data = diabetes,
                method = "knn",
                trControl = train.control,
                tuneLength = 20)
knnFit

rmse <- data.frame("Logistic regression" = rmse.fit2.loocv, "LDA" = rmse.lda, "QDA" = rmse.qda, "KNN" = knn.error[1,2])
rmse

# load the data
os = read.csv("oxygen_saturation.csv")
os <- data.frame(os)
set.seed(1)
attach(os)

# plots to see the extent of agreement between two methods
# plot a scatter plot of the data and superimpose the 45 degree line

plot1 <- plot(pos, osm, main = "pos vs. osm", pch = 19) +
  abline(coef = c(0,1), col = "blue")

# absolute values of differences in the measurements from the two methods
boxplot(abs(pos-osm), data = os, main = "abs difference b/w pos & osm")

thetahat <- quantile(abs(pos-osm))
thetahat

# manual code to compute bootstrap
# bootstrap estimates of bias
# bootstrap estimates of standard error
se <- function(data) {
  boots <- rep(NA, 1000)
  for (i in 1:1000) {
    boots[i] <- sd(sample(data, length(data), replace=TRUE))/sqrt(length(data))
  }
  
  boots
}
mean(se(thetahat))

# bootstrap estimates of 95% upper confidence bound
library(boot)
difference <- abs(pos-osm)
difference <- data.frame(difference)
quantile.fxn <- function(x, indices) {
  quantile(x[indices])
}
quantile.boot <- boot(data = difference[,1], statistic = quantile.fxn, R = 1000)
quantile.boot

# bootstrap estimates of 95% upper confidence bound
boot.ci(boot.out = quantile.boot, conf = 0.95, type = "norm")