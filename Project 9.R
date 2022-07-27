#Import data
data <- read.table("C:/Users/pulmonary.dat")

#Change column names
colnames(data) <- c('V1:resting vol(L/min)', 'V2:resting vol(mL/kg/min)', 'V3:max vol(L/min)', 'V4:max vol(mL/kg/min)', 'gender')

######Include histograms of each variable in the diagonal

#Draw pairwise scatterplot matrix with different colors/symbols based on gender (blue:male, orange:female)
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white")
}

pairs(data[,1:4], col=ifelse(data$gender == "male", "#00AFBB", "#FC4E07"), 
      pch=ifelse(data$gender == "male", 15,17), cex = 1.5, diag.panel = panel.hist,
      labels = c('V1', 'V2', 'V3', 'V4'), main="Pairwise Scatterplot Matrix")

#Separate data for each gender
data.male <- data[which(data$gender == "male"),] 
data.female <- data[which(data$gender == "female"),] 

#Correlation matrix for male
cor.male <- cor(data.male[1:4])
cor.male
#male:
#V1&V2 are the strongest positive correlation (correlation: 0.78).
#All other correlations are positive and larger than female's correlation respectively except V1&V2.

#Correlation matrix for female
cor.female <- cor(data.female[1:4])
cor.female 
#female: 
#V1&V2 are strongly positive correlated (correlation: 0.94).
#It is stronger than male's V1&V2 (0.94 > 0.78).
#V2&V3 are negatively correlated but it is weak.
#All other variables are positively correlated but their relationships are not that strong except V1&V2.

#From the scatterplots, we can see that (V1,V2) are the most positively correlated pair.
#(V3,V4) are also positively correlated, but less than (V1,V2).
#(V1,V4) is the most uncorrelated pair.

#Find univariate and multivariate outliers based on quantiles from appropriate distributions.
#How many univariate and mutivariate extreme observations do we expect and
#how does it compare with the actual number of outliers in the dataset?

#Find 0.005 quantile of Normal Distribution 
qnorm(0.995)

##Find Univariate Outliers
#Find standardized values of each variable given by (x-mean(x))/sd(x)
std.V1<-(data$V1-mean(data$V1))/sd(data$V1)
std.V2<-(data$V2-mean(data$V2))/sd(data$V2)
std.V3<-(data$V3-mean(data$V3))/sd(data$V3)
std.V4<-(data$V4-mean(data$V4))/sd(data$V4)

#Check if any absolute standardized values are > qnorm(0.995)
std.V1[abs(std.V1)>qnorm(0.995)]
rownames(data)[abs(std.V1)>qnorm(0.995)]
std.V2[abs(std.V2)>qnorm(0.995)]
rownames(data)[abs(std.V2)>qnorm(0.995)]
std.V3[abs(std.V3)>qnorm(0.995)]
rownames(data)[abs(std.V3)>qnorm(0.995)]
std.V4[abs(std.V4)>qnorm(0.995)]
rownames(data)[abs(std.V4)>qnorm(0.995)]
#Univariate Outliers: 
#row 48

##Find the multivariate outliers
#Calculate Mahalanobis Distances
maha.dist<-mahalanobis(data[,1:4],center=colMeans(data[,1:4]),cov=cov(data[,1:4]))
#Sort the Distances
sort(maha.dist)
#Find 0.01 quantile of Chi-square Distribution with 4 d.f. 
qchisq(0.99,4)

#Check if any Mahalanobis distances are > qchisq(0.99,4)
maha.dist[maha.dist>qchisq(0.99,4)]
rownames(data)[maha.dist>qchisq(0.99,4)]
#multivariate Outliers: 
#row 48

#We expect 1 outlier (row 48).

#The actual number of outliers in the dataset
##Univariate
#QQ-plot of all variables V1~V4
library(car)
par(mar=c(2,2,3,3))
par(mfrow=c(2,2))

#V1
qqPlot(data$V1, main="V1:resting vol(L/min)", ylab="V1")
#V2
qqPlot(data$V2, main="V2:resting vol(mL/kg/min)", ylab="V2")
#V3
qqPlot(data$V3, main="V3:max vol(L/min)", ylab="V3")
#V4
qqPlot(data$V4, main="V4:max vol(mL/kg/min)", ylab="V4")
#Univariate Outliers:
#V1:row 31,48
#V2:row 31,48
#V3:row 22,23
#V4:row 19,22
#The actual number of outliers in the dataset is 5. (row 19,22,23,31,48)

#Detect multivariate outliers
#Function for calculating chi-square plot
dev.off()
chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  #
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n)/(n+1)
  #
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  #
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", main="Chi-square Plot", lwd=2,pch=1)
}

#Chi-square plot
chisplot(as.matrix(data[,1:4]))
#There is 1 outlier (row 48).

#I tried to detect multivariate outliers using the library "mvoutlier".
library(mvoutlier)
chi <- chisq.plot(data[,1:4])
chi$outliers
#multivariate outliers:
#row 48,23,34
#It was little ambiguous since it kept producing not-a-line graph.
#But I thought it was the best looking if I removed those three outliers.

#Assess univariate normality assumption for each gender separately
#QQ-plot of all variables V1~V4
library(car)
par(mar=c(2,2,3,3))
par(mfrow=c(2,2))

##male
#V1
qqPlot(data.male$V1, main="male, V1:resting vol(L/min)", ylab="V1")
#V2
qqPlot(data.male$V2, main="male, V2:resting vol(mL/kg/min)", ylab="V2")
#V3
qqPlot(data.male$V3, main="male, V3:max vol(L/min)", ylab="V3")
#V4
qqPlot(data.male$V4, main="male, V4:max vol(mL/kg/min)", ylab="V4")
#V2 and V4 are normally distributed while V1 and V3 are not.
#V2 is the most appropriate.

##female
#V1
qqPlot(data.female$V1, main="female, V1:resting vol(L/min)", ylab="V1")
#V2
qqPlot(data.female$V2, main="female, V2:resting vol(mL/kg/min)", ylab="V2")
#V3
qqPlot(data.female$V3, main="female, V3:max vol(L/min)", ylab="V3")
#V4
qqPlot(data.female$V4, main="female, V4:max vol(mL/kg/min)", ylab="V4")
#V1 and V2 are normally distributed while V3 and V4 are not.
#V1 is the most appropriate.

#The most appropriate variable for each gender after assessing normality
par(mfrow=c(1,2))
qqPlot(data.male$V2, main="male, V2:resting vol(mL/kg/min)", ylab="V2")
qqPlot(data.female$V1, main="female, V1:resting vol(L/min)", ylab="V1")

#original, sqrt, log, box-cox transformation respectively for each variable to find the best way
##male
par(mfrow=c(2,2))
#V1
qqPlot(data.male$V1, main="male, V1:resting vol(L/min)", ylab="V1")
qqPlot(sqrt(data.male$V1), main="male, sqrt(V1:resting vol(L/min))", ylab="V1")
qqPlot(log(data.male$V1), main="male, log(V1:resting vol(L/min))", ylab="V1")
powerTransform(data.male$V1)
qqPlot(bcPower(data.male$V1,0.8808799), main="male, box-cox(V1:resting vol(L/min))", ylab="V1")
#It has improved slightly, but log is the best

#V3
qqPlot(data.male$V3, main="male, V3:max vol(L/min)", ylab="V3")
qqPlot(sqrt(data.male$V3), main="male, sqrt(V3:max vol(L/min))", ylab="V3")
qqPlot(log(data.male$V3), main="male, log(V3:max vol(L/min))", ylab="V3")
powerTransform(data.male$V3)
qqPlot(bcPower(data.male$V3,-1.283871), main="male, box-cox(V3:max vol(L/min))", ylab="V3")
#box-cox makes it better

#V4
qqPlot(data.male$V4, main="male, V4:max vol(mL/kg/min)", ylab="V4")
qqPlot(sqrt(data.male$V4), main="male, sqrt(V4:max vol(mL/kg/min))", ylab="V4")
qqPlot(log(data.male$V4), main="male, log(V4:max vol(mL/kg/min))", ylab="V4")
powerTransform(data.male$V4)
qqPlot(bcPower(data.male$V4,2.278161), main="male, box-cox(V4:max vol(mL/kg/min))", ylab="V4")
#It has improved slightly, but sqrt is the best

#The best transformation for each variable of male
par(mfrow=c(1,3))
qqPlot(log(data.male$V1), main="male, log(V1:resting vol(L/min))", ylab="V1")
qqPlot(bcPower(data.male$V3,-1.283871), main="male, box-cox(V3:max vol(L/min))", ylab="V3")
qqPlot(sqrt(data.male$V4), main="male, sqrt(V4:max vol(mL/kg/min))", ylab="sqrt(V4)")

##female
par(mfrow=c(2,2))
#V2
qqPlot(data.female$V2, main="female, V2:resting vol(L/min)", ylab="V2")
qqPlot(sqrt(data.female$V2), main="female, sqrt(V2:resting vol(L/min))", ylab="V2")
qqPlot(log(data.female$V2), main="female, log(V2:resting vol(L/min))", ylab="V2")
powerTransform(data.female$V2)
qqPlot(bcPower(data.female$V2,0.407507), main="female, box-cox(V2:resting vol(L/min))", ylab="V1")
#log is the best

#V3
qqPlot(data.female$V3, main="female, V3:max vol(L/min)", ylab="V3")
qqPlot(sqrt(data.female$V3), main="female, sqrt(V3:max vol(L/min))", ylab="V3")
qqPlot(log(data.female$V3), main="female, log(V3:max vol(L/min))", ylab="V3")
powerTransform(data.female$V3)
qqPlot(bcPower(data.female$V3,-0.1728886), main="female, box-cox(V3:max vol(L/min))", ylab="V3")
#It has improved slightly, but box-cox makes it better than any other

#V4
qqPlot(data.female$V4, main="female, V4:max vol(mL/kg/min)", ylab="V4")
qqPlot(sqrt(data.female$V4), main="female, sqrt(V4:max vol(mL/kg/min))", ylab="V4")
qqPlot(log(data.female$V4), main="female, log(V4:max vol(mL/kg/min))", ylab="V4")
powerTransform(data.female$V4)
qqPlot(bcPower(data.female$V4,-0.1596472), main="female, box-cox(V4:max vol(mL/kg/min))", ylab="V4")
#It has  improved slightly, but box-cox is the best
#Both log and box-cox are almost the same, but I chose box-cox because it makes the blue shaded area slightly smaller.

#The best transformation for each variable of female
par(mfrow=c(1,3))
qqPlot(log(data.female$V2), main="female, log(V1:resting vol(L/min))", ylab="V1")
qqPlot(bcPower(data.female$V3,-0.1728886), main="female, box-cox(V3:max vol(L/min))", ylab="V3")
qqPlot(bcPower(data.female$V4,-0.1596472), main="female, box-cox(V4:max vol(mL/kg/min))", ylab="V4")

#Make a new dataframe with the best transformated version for each variable of male
f1 <- log(data.male$V1)
f2 <- data.male$V2
f3 <- bcPower(data.male$V3,-1.283871)
f4 <- sqrt(data.male$V4)
new.male <- data.frame(x=f1, y=f2, z=f3, w=f4)
colnames(new.male) <- c("f1:log(V1)","f2:V2","f3:bcPower(V3)","f4:sqrt(V4)")

#Test whether the mean vector of the four variables (in the order listed at the beginning) for males is equal to (f1(0.3), f2(7), f3(4.5), f4(35)).
mean <- colMeans(new.male)
mean
var.cov <- cov(new.male)
var.cov
var.inv <- solve(var.cov)
var.inv
mu <- matrix(c(log(0.3),7,bcPower(4.5,-1.283871),sqrt(35)),nrow=4) 
mu
as.matrix(mean)
T2 <- 25*t(as.matrix(mean)-mu)%*%var.inv%*%(as.matrix(mean)-mu)
T2
crit.value <- qf(0.01,4,21, lower.tail=F)
(24*4)*crit.value/21
#H0:mean(new.male)=mu(=(f1(0.3), f2(7), f3(4.5), f4(35)))vs. HA:not H0
#Since T2=26146.1 > 19.97173, we reject H0.
#We conclude that mean(new.male) is not equal to mu(=f1(0.3), f2(7), f3(4.5), f4(35)).

#find 95% simultaneous T2 confidence intervals for the mean of each variable (in transformed scale)
mean <- data.frame(mean)
crit.value <- qf(0.05,4,21, lower.tail=F)

#95% simultaneous T2 confidence intervals for log(V1)
mean[1,1]-sqrt((4*24/21)*crit.value)*sqrt(var.cov[1,1]/25)
mean[1,1]+sqrt((4*24/21)*crit.value)*sqrt(var.cov[1,1]/25)
#log(V1); (-1.106862,-0.7856224)

#95% simultaneous T2 confidence intervals for V2
mean[2,1]-sqrt((4*24/21)*crit.value)*sqrt(var.cov[2,2]/25)
mean[2,1]+sqrt((4*24/21)*crit.value)*sqrt(var.cov[2,2]/25)
#V2; (4.55875,6.10045)

#95% simultaneous T2 confidence intervals for bcPower(V3)
mean[3,1]-sqrt((4*24/21)*crit.value)*sqrt(var.cov[3,3]/25)
mean[3,1]+sqrt((4*24/21)*crit.value)*sqrt(var.cov[3,3]/25)
#bcPower(V3); (0.6029158,0.6502873)

#95% simultaneous T2 confidence intervals for sqrt(V4)
mean[4,1]-sqrt((4*24/21)*crit.value)*sqrt(var.cov[4,4]/25)
mean[4,1]+sqrt((4*24/21)*crit.value)*sqrt(var.cov[4,4]/25)
#sqrt(V4); (6.613543,7.405134)

#find 95% simultaneous T2 confidence intervals for the mean of an index given by 2*f1(resting volume (L/min)) + f3(maximum volume O2 (L/min)).
new <- data.frame(2*new.male$`f1:log(V1)`+new.male$`f3:bcPower(V3)`)
colnames(new) <- "2*f1+f3"
mean.new <- colMeans(new)
mean.new
var.cov.new <- cov(new)
var.cov.new
mean.new-sqrt((4*24/21)*crit.value)*sqrt(var.cov.new[1,1]/25)
mean.new+sqrt((4*24/21)*crit.value)*sqrt(var.cov.new[1,1]/25)
#2*f1(V1)+f3(V3); (-1.600436,-0.9313305)

#find 95% Bonferroni confidence intervals for the mean of each variable (in transformed scale) 
t.value <- qt(0.05/8,24,lower.tail=F) 

#95% Bonferroni confidence intervals for log(V1)
mean[1,1]-t.value*sqrt(var.cov[1,1]/25)
mean[1,1]+t.value*sqrt(var.cov[1,1]/25)
#log(V1); (-1.066609,-0.8258752)

#95% Bonferroni confidence intervals for V2
mean[2,1]-t.value*sqrt(var.cov[2,2]/25)
mean[2,1]+t.value*sqrt(var.cov[2,2]/25)
#V2; (4.751932,5.907268)

#95% Bonferroni confidence intervals for bcPower(V3)
mean[3,1]-t.value*sqrt(var.cov[3,3]/25)
mean[3,1]+t.value*sqrt(var.cov[3,3]/25)
#bcPower(V3); (0.6088517,0.6443514)

#95% Bonferroni confidence intervals for sqrt(V4)
mean[4,1]-t.value*sqrt(var.cov[4,4]/25)
mean[4,1]+t.value*sqrt(var.cov[4,4]/25)
#sqrt(V4); (6.712733,7.305944)

#find Bonferroni confidence intervals for the mean of an index given by 2*f1(resting volume (L/min)) + f3(maximum volume O2 (L/min)).
mean.new-t.value*sqrt(var.cov.new[1,1]/25)
mean.new+t.value*sqrt(var.cov.new[1,1]/25)
#2*f1(V1)+f3(V3); (-1.516594,-1.015172)

##95% simultaneous T2 confidence intervals
#log(V1); (-1.106862,-0.7856224)
#V2; (4.55875,6.10045)
#bcPower(V3); (0.6029158,0.6502873)
#sqrt(V4); (6.613543,7.405134)
#2*f1(V1)+f3(V3); (-1.600436,-0.9313305)

##95% Bonferroni confidence intervals
#log(V1); (-1.066609,-0.8258752)
#V2; (4.751932,5.907268)
#bcPower(V3); (0.6088517,0.6443514)
#sqrt(V4); (6.712733,7.305944)
#2*f1(V1)+f3(V3); (-1.516594,-1.015172)

#Simultaneous T2 confidence intervals for every variable are longer than the Bonferroni confidence intervals. 

#Draw a 95% confidence ellipse for the mean of V2 & sqrt(V4) (in transformed scale) 
mean
var.inv
crit.value <- 4*24/21*qf(0.05,4,21, lower.tail=F)

dev.off()
ellipse<-function(mu2,mu4)
{
  25*28.94897*(5.3296000-mu2)^2+25*81.50959*(7.0093381-mu4)^2-50*46.60197*(5.3296000-mu2)*(7.0093381-mu4)
}
mu2<-seq(5.2,5.5,0.0005)
mu4<-seq(6.75,7.2,0.0005)
z <- t(sapply(mu2,ellipse,mu4))
contour(mu2,mu4,z,levels=crit.value, drawlabels=F, axes=T, frame.plot=F, 
        xlab="mu2 (resting volume (mL/kg/min))", ylab="sqrt(mu4) (maximum volume O2 (mL/kg/min))", main = "A 95% confidence ellipse for (mu2,sqrt(mu4))")
points(5.3296000,7.0093381,pch=1)
segments(0,7.0093381,5.3296000,7.0093381,lty=2)
segments(5.3296000,0,5.3296000,7.0093381, lty=2)

#add the major and minor axes in the plot
var.cov
S <- matrix(c(1.144179,0.252329063,0.252329063,0.30164501),nrow=2)
S
eigen(S)
crit.value <- (4*24)/(25*21)*qf(0.05,4,21,lower.tail=F)
ev <- sqrt(eigen(S)$values)
lambda1 <- ev[1]
lambda2 <- ev[2]

#major axis
major <- lambda1*sqrt(crit.value) #0.7940107
#minor axis
minor <- lambda2*sqrt(crit.value) #0.3470021

#Draw a major axis & minor axis
arrows(5.3296000,7.0093381,5.3296000+minor*(0.9638153),7.0093381+major*(0.2665708))
arrows(5.3296000,7.0093381,5.3296000+major*(0.2665708),7.0093381+major*(-0.9638153))

#Use the ellipse to answer if (f2(5), f4(40)) is a plausible value for the mean vector? 
#(f2(5), f4(40)) = (5,6.324555)
points(5,6.324555,pch=1)
