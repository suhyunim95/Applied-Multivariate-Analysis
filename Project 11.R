#Import the data
ability.cor <- read.table("abilitycor.DAT")
colnames(ability.cor) <- c("GeneralIntelligence", "PictureCompletion", "BlockDesign", "Mazes", "ReadingComprehension", "Vocabulary")
rownames(ability.cor) <- c("GeneralIntelligence", "PictureCompletion", "BlockDesign", "Mazes", "ReadingComprehension", "Vocabulary")
ability.cor <- as.matrix(ability.cor)

#Unrotated factor analysis with m=1,2,3
ability.fa <- lapply(1:3,function(nf) factanal(covmat=ability.cor, factors=nf, rotation="none", n.obs=112))

#Varimax rotation for m=1,2,3
ability.fa.rot <- lapply(1:3,function(nf) factanal(covmat=ability.cor, factors=nf, rotation="varimax", n.obs=112))

#Test of whether the number of factors is sufficient
for (i in 1:3) {
  print(as.numeric(ability.fa[[i]]$PVAL))
}

#Test of whether the number of factors is sufficient
for (i in 1:3) {
  print(as.numeric(ability.fa.rot[[i]]$PVAL))
}

#Hypotheses: 
#H0: The model is not sufficient. vs. HA: The model is sufficient.
#Test statistic: 
#m=1: 75.18
#m=2: 6.11
#m=3: n/a
#p-value: 
#m=1: 1.456385e-12
#m=2: 0.1913266
#m=3: 0

#m=1 or m=2 would be good.
#But the p-value for m=2 is relatively large.
#We need to look at the residual matrix as well.

#m=1
pred1 <- ability.fa[[1]]$loadings%*%t(ability.fa[[1]]$loadings)+
  diag(ability.fa[[1]]$uniquenesses) 
round(ability.cor-pred1,digits=3)

#m=2
pred2 <- ability.fa[[2]]$loadings %*% t(ability.fa[[2]]$loadings)+
  diag(ability.fa[[2]]$uniquenesses)
round(ability.cor-pred2,digits=3)

#The off-diagonals of the residual matrix for m=2 are smaller than the one for m=1.
#Therefore, m=2 model is better.

#L (factor loading matrix)
ability.fa.rot[[2]]$loadings 

#Specific variance matrix
ability.fa.rot[[2]]$uniquenes

#R=LL'+Psi
pred2.rot <- ability.fa.rot[[2]]$loadings %*% t(ability.fa.rot[[2]]$loadings)+
  diag(ability.fa.rot[[2]]$uniquenesses)
round(pred2.rot,digits=3)

#Model assumptions for this analysis:
#The relationship between observed variables X and the underlying factors F is linear.
#F and epsilon are independent.
#Mean(F)=0, cov(F)=I.
#Mean(epsilon)=0, cov(epsilon)=Psi, where Psi is a diagonal matrix.
#Common factor F and specific factors epsilon are normally distributed.
#R=LL'+Psi => The m-factor model holds for X and vice versa.

t11.6 <- read.table("T11-6.DAT")
colnames(t11.6) <- c("GPA","GMAT","class")
#1:Admit, 2:Do not Admit, 3:Borderline

#Perform LDA
library(MASS)
dis <- lda(class~GPA+GMAT, data=t11.6, prior=c(1/3,1/3,1/3))
dis
dis$scaling
#96% of between-groups variance explains by the first discriminant.
#d1=-5.017202736*GPA-0.008503148*GMAT
#d2=1.85401003*GPA-0.01448967*GMAT

#Classify a new applicant with GPA=3 and GMAT=450
new <- predict(dis, newdata=data.frame(GPA=3,GMAT=450))
new
#A new applicant is classified as class 3: Borderline.

#Calculate APER
dis.pred <- predict(dis)
dis.pred$class
table(dis.pred$class, t11.6$class)
mean(dis.pred$class != t11.6$class)
#APER = 0.08235294

#Calculate AER
dis.cv <- lda(class~., data=t11.6, prior=c(1/3,1/3,1/3), CV=TRUE)
dis.cv$class
table(dis.cv$class, t11.6$class)
mean(dis.cv$class != t11.6$class)
#AER = 0.1058824

#Make a scatterplot of the first two discriminant scores by labeling different decision types with different symbols and colors.
ld <- predict(dis)$x
eqscplot(ld[1:31,1],ld[1:31,2], xlab="First linear discriminant", ylab="Second linear discriminant", xlim=range(ld[,1]), ylim=range(ld[,2]),
         cex=0.8, col=2, main="LDA Classification")
points(ld[32:59,1],ld[32:59,2],pch=2,cex=0.8,col=3)
points(ld[60:85,1],ld[60:85,2],pch=19,cex=0.8,col=4)
legend(-4,10,legend=c("Admit", "Do not Admit", "Borderline"),pch=c(1,2,19), col=c(2,3,4), bty="n")

#Develop a classification rule using logistic regression

#Repeat
library(nnet)
logreg <- multinom(as.factor(class)~GPA+GMAT, data=t11.6)
summary(logreg)
fitted(logreg)

#Classify a new applicant with GPA=3 and GMAT=450
new <- predict(logreg, newdata=data.frame(GPA=3,GMAT=450))
new
#A new applicant is classified as class 3: Borderline.

#Calculate APER
mean(predict(logreg) != t11.6$class)
#APER = 0.03529412

#Calculate AER
newpred <- numeric(length(t11.6$class))
for (i in 1:length(t11.6$class))
{
  newdat <- t11.6[-i,]
  newfit <- multinom(as.factor(class)~., data=newdat)
  newpred[i] <- predict(newfit, newdat=data.frame(t11.6[i,-3]))
}
table(t11.6$class, newpred)
mean(t11.6$class != newpred)
#AER = 0.05882353

#Compare the results from the linear discriminant function and logistic regression 
#by making an appropriate plot that shows classifications based on both rules
pred <- predict(logreg)
preddata <- data.frame(cbind(t11.6[,-3],pred))
plot(preddata$GPA, preddata$GMAT, pch=19, cex=1, col=factor(preddata$pred), 
     xlab="GPA", ylab="GMAT", main="Logistic Regression Classification")
legend("topleft",legend=c("Admit", "Do not Admit", "Borderline"), pch=19, col=c(2,1,3), bty="n")

planet <- source("planet.dat")$value

#Make a scatterplot matrix of variables
pairs(planet, lower.panel=NULL)

#Perform hierarchical clustering with single, complete, and average linkage
par(mfrow=c(1,3))
plot(hclust(dist(planet),method="single"), xlab = "single", ylab="Distance", main="(a) Single linkage")
plot(hclust(dist(planet),method="complete"), xlab = "Complete", ylab="Distance", main="(b) Complete linkage")
plot(hclust(dist(planet),method="average"), xlab = "average", ylab="Distance", main="(c) Average linkage")
abline(h=1500)
cluster <- cutree(hclust(dist(planet),method="complete"),h=1500)
max(cluster)
#k=4 is the optimal number of clusters.

#Mean vectors for each cluster
cluster.mean.4 <- lapply(1:4,function(nc) {colMeans(planet[cluster==nc,])})
cluster.mean.4

#Standardizing the variables by dividing each variable by its range
rge <- apply(planet,2,max)-apply(planet,2,min)
planet.std <- sweep(planet,2,rge,FUN="/")

#Find sum of within-groups ss for #clusters = 1 to 20
n <- length(planet.std[,1])
wss <- numeric(10)
for(i in 1:10) {
  W<-sum(kmeans(planet.std,i)$withinss)
  wss[i]<-W
}

#Plot the wss vs number of clusters
par(mfrow=c(1,1))
plot(1:10, wss, type="l", xlab="Number of groups", ylab="Within groups sum of squares", lwd=2, main="within-group sum of squares vs. k")
#k=3 is the optimal number of clusters since there is an elbow in the plot.

#Mean vectors for each cluster
cluster.mean.3 <- lapply(1:3,function(nc) {colMeans(planet[cluster==nc,])})
cluster.mean.3

#Perform model-based clustering
library(mclust)
mb = Mclust(planet)
summary(mb)
mb$G
#k=3 is the optimal number of clusters.
mb$modelName 
#The optimal selected model is VVI.

#Make the pairwise scatterplots showing the clusters, density plot, and BIC plot.
par(mfrow=c(1,1))
plot(mb, what=c("classification"))
plot(mb, "density")
plot(mb, "BIC")

#Use the package fpc to further compare the results
library("fpc")
cs = cluster.stats(dist(planet), mb$classification)
cs$within.cluster.ss #within cluster sum of squares
cs[c("within.cluster.ss","avg.silwidth")] #average silhouette width - ranges from 0 to 1; value closer to 1 suggests the data are better clustered.
cs$cluster.number
#Mean vectors for each cluster
cluster.mean.3 <- lapply(1:3,function(nc) {colMeans(planet[cluster==nc,])})
cluster.mean.3