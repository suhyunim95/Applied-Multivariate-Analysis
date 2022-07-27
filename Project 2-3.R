# load the data
db = read.csv("diabetes.csv")
db <- data.frame(db)

train.x <- db[,-9]
train.y <- db[,9]

# rename column names
colnames(db)
names(db) <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
               "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

# treat Outcome as qualitative predictor
attach(db)
Outcome <- as.factor(Outcome)

table(Outcome)

# Explanatory data analysis
str(db)
head(db)
summary(db)
pairs(db)

par(mfrow=c(4,2))
plot1 <- boxplot(Pregnancies ~ Outcome, xlab = "Outcome", ylab = "Pregnancies", main = "Pregnancies vs. Outcome")
plot2 <- boxplot(Glucose ~ Outcome, xlab = "Outcome", ylab = "Glucose", main = "Glucose vs. Outcome")
plot3 <- boxplot(BloodPressure ~ Outcome, xlab = "Outcome", ylab = "BloodPressure", main = "BloodPressure vs. Outcome")
plot4 <- boxplot(SkinThickness ~ Outcome, xlab = "Outcome", ylab = "SkinThickness", main = "SkinThickness vs. Outcome")
plot5 <- boxplot(Insulin ~ Outcome, xlab = "Outcome", ylab = "Insulin", main = "Insulin vs. Outcome")
plot6 <- boxplot(BMI ~ Outcome, xlab = "Outcome", ylab = "BMI", main = "BMI vs. Outcome")
plot7 <- boxplot(DiabetesPedigreeFunction ~ Outcome, xlab = "Outcome", ylab = "DiabetesPedigreeFunction", main = "DPF vs. Outcome")
plot8 <- boxplot(Age ~ Outcome, xlab = "Outcome", ylab = "Age", main = "Age vs. Outcome")
# -> Glucose, Age, Pregnancies might have relation with Outcome 

# correlation between all other predictors and Outcome
cor(db)[,9]

library(MASS)

# perform LDA using the the training data
lda.fit = lda(Outcome ~ ., data = db)
lda.fit

# confusion matrix of the training data
lda.pred = predict(lda.fit, train.x)
table(lda.pred$class, train.y)

# overall misclassification rate of the training data
mean(lda.pred$class != train.y)

# ROC curve
library(pROC)
roc.lda <- roc(train.y, lda.pred$posterior[, "1"], levels = c("0", "1"), direction = "<")
roc.lda
plot(roc.lda, legacy.axes = T)

# perform QDA using the training data
qda.fit = qda(Outcome ~ ., data = db)
qda.fit

# confusion matrix of the training data
qda.pred = predict(qda.fit, train.x)
table(qda.pred$class, train.y)

# overall misclassification rate of the training data
mean(qda.pred$class != train.y)

# ROC curve
roc.qda <- roc(train.y, qda.pred$posterior[, "1"], levels = c("0", "1"), direction = "<")
roc.qda
plot(roc.qda, legacy.axes = T, col = "blue")

# compare LDA & QDA
plot(roc.lda, legacy.axes = T)
plot(roc.qda, add = T, col = "blue")