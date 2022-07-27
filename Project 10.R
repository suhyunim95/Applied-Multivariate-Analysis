#Import the data
t6.8 <- read.table("~/T6-8.dat")

#Change the column names
colnames(t6.8) <- c('WordDiff','WordSame','ArabicDiff','ArabicSame')

attach(t6.8)

#Get the mean and S matrices for different parity
diff <- data.frame(WordDiff, ArabicDiff)
mean.diff <- colMeans(diff)
S.diff <- var(diff)

#Get the mean and S matrices for same parity
same <- data.frame(WordSame, ArabicSame)
mean.same <- colMeans(same)
S.same <- var(same)

#Get the mean and pooled S matrices for difference of diff and same
mean.D <- mean.diff-mean.same
solve(S.diff)
crit.value <- (2*31/30)*qf(.95,2,30)

#Construct a 95% confidence region (ellipsoid) for difference of means between each formats
ellipse <- function(mu1,mu2)
{
  32*0.00006*(91.95312-mu1)^2+0.00011*(114.37500-mu2)^2-0.00006*(91.95312-mu1)*(114.37500-mu2)
}
mu1<-seq(0,180,0.1)
mu2<-seq(-200,400,0.1)
z <- t(sapply(mu1,ellipse,mu2))
contour(mu1,mu2,z,levels=crit.value, drawlabels=F, axes=T, frame.plot=F, 
        xlab="mu1", ylab="mu2", main = "A 95% confidence ellipse for (mu1,mu2)")
points(0,0,pch=1)
#Since (0,0) is outside the confidence region, we reject H0.
#Therefore, the means for the two formats differ.

#Construct Bonferroni 95% simultaneous confidence intervals
tvalue <- qt(0.05/4,31,lower.tail=F)
mean.D[1]-tvalue*sqrt(S.diff[1,1]/32)
mean.D[1]+tvalue*sqrt(S.diff[1,1]/32)
mean.D[2]-tvalue*sqrt(S.diff[2,2]/32)
mean.D[2]+tvalue*sqrt(S.diff[2,2]/32)
#Word: (12.74949,171.1568)
#Arabic: (57.75582,170.9942)
#Both Word and Arabic doesn't contain 0, so (0,0) is not a plausible value.
#Therefore, we reject H0 and this implies that the means for the two formats differ.
#It has the same context with the conclusion derived from the confidence ellipse.

#Import the data
t11.5 <- read.table("~/T11-5.dat")

#Change the column names
colnames(t11.5) <- c('sepalLength','sepalWidth','petalLength','petalWidth','class')
#class 
#1:Iris Setosa
#2:Iris Versicolour
#3:Iris Virginica

#Consider class 2&3 only
t11.5 <- subset(t11.5, class==2 | class==3)

attach(t11.5)

#Make one scatterplot of sepal length vs sepal width 
min(t11.5$sepalLength)
max(t11.5$sepalLength)
min(t11.5$sepalWidth)
max(t11.5$sepalWidth)
plot(sepalLength, sepalWidth, xlim=c(4.9,7.9), ylim=c(2,3.8), pch=15:16, col=factor(class), main="sepal length vs sepal width")
legend("bottomright", xpd = TRUE, horiz = FALSE, legend = c("Iris Versicolour", "Iris Virginica"), pch=15:16, col=1:2)
#Generally, versicolour has a shorter and narrower sepal compared to virginica.

#Conduct a hypothesis test at 5% level on whether the two types differ in terms of the two means jointly.
#Data preprocessing
versicolour <- subset(t11.5, class==2)
versicolour <- data.frame(versicolour$sepalLength, versicolour$sepalWidth)
colnames(versicolour) <- c("sepalLength","sepalWidth")
virginica <- subset(t11.5, class==3)
virginica <- data.frame(virginica$sepalLength, virginica$sepalWidth)
colnames(virginica) <- c("sepalLength","sepalWidth")
n1 <- 50
n2 <- 50
p <- 2

#Get the mean matrices for two types
x1.bar <- colMeans(versicolour)
x2.bar <- colMeans(virginica)

#Get the variance-covariance matrices for two types
s1 <- var(versicolour)
s2 <- var(virginica)

#Assume the variance-covariance matrices for the two types are the same
Sp <- ((n1-1)/(n1+n2-2))*s1+((n2-1)/(n1+n2-2))*s2

#Calculate the test statistic
T2 <- (x1.bar-x2.bar)%*%solve((1/50+1/50)*Sp)%*%(x1.bar-x2.bar)
T2
crit.value <- ((n1+n2-2)*p/(n1+n2-p-1))*qf(.95,p,n1+n2-p-1)
crit.value
pvalue <- 1-pf(T2,p,n1+n2-p-1)
pvalue
#H0:Two types are not different in terms of the two means jointly  vs. HA:Two types differ in terms of the two means jointly 
#Test statistic = 31.97952
#crit.value = 6.244089
#p-value = 2.151457e-11
#Since T2 = 31.97952 > 6.244089 and p-value is small enough, we reject H0.
#We conclude that two types differ in terms of the two means jointly.

#Read the data again
t11.5 <- read.table("~/T11-5.dat")

colnames(t11.5) <- c('sepalLength','sepalWidth','petalLength','petalWidth','class')

attach(t11.5)

#Re-make the scatterplot with the whole dataset
min(t11.5$sepalLength)
max(t11.5$sepalLength)
min(t11.5$sepalWidth)
max(t11.5$sepalWidth)
plot(sepalLength, sepalWidth, xlim=c(4.3,7.9), ylim=c(2,4.4), pch=class, col=factor(class), main="sepal length vs sepal width")
legend("topright", xpd = TRUE, horiz = FALSE, legend = c("Iris Setosa", "Iris Versicolour", "Iris Virginica"), pch=1:3, col=1:3)
#Setosa has shorter sepal length than the other two types.
#Versicolour and Virginica have similar Sepal length and width, but mostly Virginica is longer than Versicolour.

#Make the scatterplot of petal length vs petal width of the three types
min(t11.5$petalLength)
max(t11.5$petalLength)
min(t11.5$petalWidth)
max(t11.5$petalWidth)
plot(petalLength, petalWidth, xlim=c(1,6.9), ylim=c(0.1,2.5), pch=class, col=factor(class), main="petal length vs petal width")
legend("bottomright", xpd = TRUE, horiz = FALSE, legend = c("Iris Setosa", "Iris Versicolour", "Iris Virginica"), pch=1:3, col=1:3)
#When it comes to Petal length and width, 3 types of iris are separated well.
#Setosa is the type which has short petal length and width.
#Versicolour has longer petal length and width than Setosa, but not than Virginica.
#Verginica is the type which has long petal length and width.

#Conduct MANOVA assuming equality of the variance-covariance matrices
iris.manova <- manova(cbind(sepalLength,sepalWidth,petalLength,petalWidth) ~ as.factor(class))
summary(iris.manova, test="Wilks")
crit.value <- pf(.95,4,2*(150-3-1))
#H0:Three types are not different in terms of the three means jointly  vs. HA:Three types differ in terms of the three means jointly 
#Test statistic = 199.15
#critical value = 0.5645977
#p-value = 2.2e-16
#Since p-value is small enough, we reject H0.
#We conclude that three types differ in terms of the three means jointly.

#Obtain 95% simultaneous CI for differences in mean components.
#Data preprocessing
setosa <- subset(t11.5[,-5], class==1)
versicolour <- subset(t11.5[,-5], class==2)
virginica <- subset(t11.5[,-5], class==3)
colnames(setosa) <- c("sepalLength","sepalWidth","petalLength","petalWidth")
colnames(versicolour) <- c("sepalLength","sepalWidth","petalLength","petalWidth")
colnames(virginica) <- c("sepalLength","sepalWidth","petalLength","petalWidth")

g=3
p=4

x1.bar <- colMeans(setosa)
x2.bar <- colMeans(versicolour)
x3.bar <- colMeans(virginica)
s1 <- var(setosa)
s2 <- var(versicolour)
s3 <- var(virginica)
W <- summary(iris.manova, test="Wilks")$SS$Residuals
tvalue <- qt(.95/(p*g*(g-1)),147)
  
#CI for variables 1~4 between groups 1 and 2
CI.sepalLength.grp12<-c(x1.bar[1]-x2.bar[1]-tvalue*sqrt(W[1,1]/147*(1/50+1/50)), 
                 x1.bar[1]-x2.bar[1]+tvalue*sqrt(W[1,1]/147*(1/50+1/50)))
CI.sepalWidth.grp12<-c(x1.bar[2]-x2.bar[2]-tvalue*sqrt(W[2,2]/147*(1/50+1/50)), 
                 x1.bar[2]-x2.bar[2]+tvalue*sqrt(W[2,2]/147*(1/50+1/50)))
CI.petalLength.grp12<-c(x1.bar[3]-x2.bar[3]-tvalue*sqrt(W[3,3]/147*(1/50+1/50)), 
                 x1.bar[3]-x2.bar[3]+tvalue*sqrt(W[3,3]/147*(1/50+1/50)))
CI.petalWidth.grp12<-c(x1.bar[4]-x2.bar[4]-tvalue*sqrt(W[4,4]/147*(1/50+1/50)), 
                 x1.bar[4]-x2.bar[4]+tvalue*sqrt(W[4,4]/147*(1/50+1/50)))

CI.sepalLength.grp12
CI.sepalWidth.grp12
CI.petalLength.grp12
CI.petalWidth.grp12

#CI for variables 1~4 between groups 2 and 3
CI.sepalLength.grp23<-c(x2.bar[1]-x3.bar[1]-tvalue*sqrt(W[1,1]/147*(1/50+1/50)), 
                        x2.bar[1]-x3.bar[1]+tvalue*sqrt(W[1,1]/147*(1/50+1/50)))
CI.sepalWidth.grp23<-c(x2.bar[2]-x3.bar[2]-tvalue*sqrt(W[2,2]/147*(1/50+1/50)), 
                       x2.bar[2]-x3.bar[2]+tvalue*sqrt(W[2,2]/147*(1/50+1/50)))
CI.petalLength.grp23<-c(x2.bar[3]-x3.bar[3]-tvalue*sqrt(W[3,3]/147*(1/50+1/50)), 
                        x2.bar[3]-x3.bar[3]+tvalue*sqrt(W[3,3]/147*(1/50+1/50)))
CI.petalWidth.grp23<-c(x2.bar[4]-x3.bar[4]-tvalue*sqrt(W[4,4]/147*(1/50+1/50)), 
                       x2.bar[4]-x3.bar[4]+tvalue*sqrt(W[4,4]/147*(1/50+1/50)))

CI.sepalLength.grp23
CI.sepalWidth.grp23
CI.petalLength.grp23
CI.petalWidth.grp23

#CI for variables 1~4 between groups 1 and 3
CI.sepalLength.grp13<-c(x1.bar[1]-x3.bar[1]-tvalue*sqrt(W[1,1]/147*(1/50+1/50)), 
                        x1.bar[1]-x3.bar[1]+tvalue*sqrt(W[1,1]/147*(1/50+1/50)))
CI.sepalWidth.grp13<-c(x1.bar[2]-x3.bar[2]-tvalue*sqrt(W[2,2]/147*(1/50+1/50)), 
                       x1.bar[2]-x3.bar[2]+tvalue*sqrt(W[2,2]/147*(1/50+1/50)))
CI.petalLength.grp13<-c(x1.bar[3]-x3.bar[3]-tvalue*sqrt(W[3,3]/147*(1/50+1/50)), 
                        x1.bar[3]-x3.bar[3]+tvalue*sqrt(W[3,3]/147*(1/50+1/50)))
CI.petalWidth.grp13<-c(x1.bar[4]-x3.bar[4]-tvalue*sqrt(W[4,4]/147*(1/50+1/50)), 
                       x1.bar[4]-x3.bar[4]+tvalue*sqrt(W[4,4]/147*(1/50+1/50)))

CI.sepalLength.grp13
CI.sepalWidth.grp13
CI.petalLength.grp13
CI.petalWidth.grp13

#Import the data
personality <- read.csv("~/personality.csv")

#Remove obs column
personality <- personality[,-1]

#Perform a principal components analysis of these variables
pc <- princomp(personality, cor=FALSE)
summary(pc, loadings=FALSE) 
#4~6 components are needed to explain 83% of the total variation.

#Report those PCs
pc$loadings[,1:4]

#calculate the correlation between these PCs and each variable.
e <- as.matrix(pc$loadings[,1:4])
sqrt.lambda <- as.matrix(pc$sdev)
s2 <- as.matrix(diag(var(personality)))

r = matrix(data=NA, nrow=15, ncol=4)

for(i in 1:4){
  for(k in 1:15){
    r[k,i] = e[k,i]*sqrt.lambda[i]/sqrt(s2[k])
  }
}

colnames(r) <- c("PC1","PC2","PC3","PC4")
rownames(r) <- colnames(personality)
r

#Make a screeplot
screeplot(pc, type="lines", main="Scree Plot")
#3-4 PCs are adequate.

#Make scatterplot(s) of the selected PC scores
pairs(pc$scores, verInd = 1:4, horInd = 1:4, main="scatterplot of the PC scores")
