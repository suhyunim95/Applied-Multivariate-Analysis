# load the data
wine = read.csv("wine.csv")
wine <- data.frame(wine)

# Explanatory data analysis
head(wine)
summary(wine)

# since the range of Quality is 7.90 ~ 16.10 as others' are between 1 ~ 7, Quality needs some transformation.
      # maybe log transformation? 
hist(Quality)
hist(sqrt(Quality))
pairs(wine) 
# aroma, body, flavor might have significant meaning as predictor for the response (Quality)

# treat Region as qualitative predictor
attach(wine)
Region <- as.factor(Region)
str(wine)
contrasts(Region)

fit1 <- lm(sqrt(Quality) ~ Clarity, data = wine)
fit1
summary(fit1)
fit2 <- lm(sqrt(Quality) ~ Aroma, data = wine)
fit2
summary(fit2)
fit3 <- lm(sqrt(Quality) ~ Body, data = wine)
fit3
summary(fit3)
fit4 <- lm(sqrt(Quality) ~ Flavor, data = wine)
fit4
summary(fit4)
fit5 <- lm(sqrt(Quality) ~ Oakiness, data = wine)
fit5
summary(fit5)
fit6 <- lm(sqrt(Quality) ~ Region, data = wine)
fit6
summary(fit6)

# scatter plot
attach(wine)
par(mfrow=c(2,2))
plot1 <- plot(Aroma, sqrt(Quality), main = "Aroma vs. Quality", pch = 19) + 
  abline(lm(sqrt(Quality) ~ Aroma, data = wine), col = "blue")
plot2 <- plot(Body, sqrt(Quality), main = "Body vs. Quality", pch = 19) +
  abline(lm(sqrt(Quality) ~ Body, data = wine), col = "blue")
plot3 <- plot(Flavor, sqrt(Quality), main = "Flavor vs. Quality", pch = 19) + 
  abline(lm(sqrt(Quality) ~ Flavor, data = wine), col = "blue")
plot4 <- plot(Region, sqrt(Quality), main = "Region vs. Quality", pch = 19)

cor(Aroma, sqrt(Quality))
cor(Body, sqrt(Quality))
cor(Flavor, sqrt(Quality))
cor(Region, sqrt(Quality))

# Fit a multiple regression model to predict Quality of wine
mlfit1 <- lm(sqrt(Quality) ~ Clarity + Aroma + Flavor + Body + Oakiness + factor(Region), data = wine)
mlfit1
summary(mlfit1)

# build a "reasonably good" multiple regression model
mlfit2 <- lm(sqrt(Quality) ~ Clarity + Aroma + Flavor + Oakiness + factor(Region), data = wine)
mlfit2
summary(mlfit2)
mlfit3 <- lm(sqrt(Quality) ~ Clarity + Aroma + Flavor + factor(Region), data = wine)
mlfit3
summary(mlfit3)
mlfit4 <- lm(sqrt(Quality) ~ Clarity + Flavor + factor(Region), data = wine)
mlfit4
summary(mlfit4)
mlfit5 <- lm(sqrt(Quality) ~ Flavor + factor(Region), data = wine)
mlfit5
summary(mlfit5)

# Explore interactions of Region with other predictors
mlfit6 <- lm(sqrt(Quality) ~ Flavor*factor(Region), data = wine)
mlfit6
summary(mlfit6)

mlfit7 <- lm(sqrt(Quality) ~ Clarity*factor(Region), data = wine)
mlfit7
summary(mlfit7)

mlfit8 <- lm(sqrt(Quality) ~ Aroma*factor(Region), data = wine)
mlfit8
summary(mlfit8)

mlfit9 <- lm(sqrt(Quality) ~ Body*factor(Region), data = wine)
mlfit9
summary(mlfit9)

mlfit10 <- lm(sqrt(Quality) ~ Oakiness*factor(Region), data = wine)
mlfit10
summary(mlfit10)

# journey to find the best model 
mlfit11 <- lm(sqrt(Quality) ~ Flavor + factor(Region) + Flavor:factor(Region), data = wine)
mlfit11
summary(mlfit11)

anova(mlfit5, mlfit11)

# final model(mlfit5) in equation form
sqrt(Quality) =  2.86261 + Flavor * 0.14366 + factor(Region)2 * (-0.26188) + 0.14780 * factor(Region)3

# use the final model to predict the quality of a wine from Region 1 
# with other predictors set as their sample means
mean(Flavor)
rt.pred =  2.86261 + mean(Flavor) * 0.14366 + 0 * (-0.26188) + 0.14780 * 0
pred = rt.pred * rt.pred
pred

# 95% prediction/confidence interval
predict(mlfit5, newdata = data.frame(Flavor = mean(Flavor), Region = 1), se.fit = T, interval = "prediction")
predict(mlfit5, newdata = data.frame(Flavor = mean(Flavor), Region = 1), se.fit = T, interval = "confidence")

