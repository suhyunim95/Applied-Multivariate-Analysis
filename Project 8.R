#load data
data <- read.csv("AB Climate Means.csv")
factor(data$BIOME)

#load library
library(ggplot2)
library(ggpubr)
library(gridExtra)

#make side-by-side boxplots for each BIOME without legends
g.mat = ggplot(data, aes(x=BIOME, y=MAT)) + geom_boxplot(aes(color=BIOME))
g.mwmt = ggplot(data, aes(x=BIOME, y=MWMT)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none")
g.mcmt = ggplot(data, aes(x=BIOME, y=MCMT)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none")
g.map = ggplot(data, aes(x=BIOME, y=MAP)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none")
g.msp = ggplot(data, aes(x=BIOME, y=MSP)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none") 
g.ahm = ggplot(data, aes(x=BIOME, y=AHM)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none") 
g.shm = ggplot(data, aes(x=BIOME, y=SHM)) + geom_boxplot(aes(color=BIOME)) + theme(legend.position = "none") 

#extract a shared legend and combine all the plots with legend in one grid
g.legend <- get_legend(g.mat)
grid.arrange(g.mat + theme(legend.position = "none"), g.mwmt ,g.mcmt, g.map, g.msp, g.ahm, g.shm, g.legend, 
             nrow = 2, ncol = 4, top="7 Variables for Each BIOME")

#sample mean vector
variables <- data[,c(3:5,7:10)]
colMeans(variables)

#covariance matrix
cov(variables)

#correlation matrix
cor(variables)

#create a correlation heatmap
library(ggcorrplot)
ggcorrplot(cor(variables))

par(oma = c(0, 1, 0, 1), mar = c(0, 1, 0, 0))
plot(variables, pch=16, col=factor(data$BIOME))
legend("top", xpd = TRUE, horiz = TRUE, inset=c(1,-0.13), legend = c("Boreal", "Grassland", "Montane", "Parkland"), pch=16, col=1:4)

#reset plot setting
dev.off()

#scatterplot of AHM vs. MWMT with labeling the outliers with their ecosystem
#Add circles to the same plot based on the value of MAP variable 
#(larger circles for larger MAP values)
i <- ggplot(data, aes(x=AHM, y=MWMT)) + geom_point() +
        labs(title="Scatterplot", x="AHM", y="MWMT") +
        geom_smooth(aes(x=AHM, y=MWMT), method=lm) +
        geom_point(aes(size=MAP), shape = 1, color="white") + 
        annotate("text", x=data$AHM[1], y=data$MWMT[1], label=data$ECOSYS[1]) +
        annotate("text", x=data$AHM[2], y=data$MWMT[2], label=data$ECOSYS[2]) +
        annotate("text", x=data$AHM[4], y=data$MWMT[4], label=data$ECOSYS[4]) + 
        annotate("text", x=data$AHM[9], y=data$MWMT[9], label=data$ECOSYS[9]) +
        annotate("text", x=data$AHM[10], y=data$MWMT[10], label=data$ECOSYS[10]) +
        annotate("text", x=data$AHM[13], y=data$MWMT[13], label=data$ECOSYS[13]) +
        annotate("text", x=data$AHM[16], y=data$MWMT[16], label=data$ECOSYS[16]) + 
        annotate("text", x=data$AHM[17], y=data$MWMT[17], label=data$ECOSYS[17]) +
        annotate("text", x=data$AHM[19], y=data$MWMT[19], label=data$ECOSYS[19]) +
        annotate("text", x=data$AHM[21], y=data$MWMT[21], label=data$ECOSYS[21]) 
i

#boxplot of AHM and label outliers with their ecosystem
ii <- ggplot(data, aes(x=factor(0), y=AHM)) + geom_boxplot() + 
        theme(axis.title.x=element_blank()) +
        annotate("text", x=0.935, y=42.8, label="DMG")
ii

#boxplot of AHM and label outliers with their ecosystem
iii <- ggplot(data, aes(x=factor(0), y=MWMT)) + geom_boxplot() + 
        theme(axis.title.x=element_blank()) +
        annotate("text", x=0.955, y=8.6, label="A")
iii

iv <- ggplot(data, aes(x=MWMT)) + geom_histogram() + scale_x_continuous(breaks = 1:20)
iv

#make a new dataframe with variables whose BIOME are Boreal and Grassland
sub.b <- subset(data, BIOME == "Boreal")
sub.g <- subset(data, BIOME == "Grassland")
sub.bg <- data.frame(rbind(sub.b, sub.g))
v <- ggplot(sub.bg, aes(x=MWMT, fill = BIOME)) + geom_histogram(position = "dodge") +
        scale_x_continuous(breaks=1:20, lim=c(8,19))
v

#Divide the plotting area into five parts in such a manner 
#that none of the plots get squeezed/distorted. 
#Make efficient use of the plotting area in order to minimize white space. 
ggarrange(i, ggarrange(ii, iii, nrow=2, ncol=1), ggarrange(iv, v, nrow=1, ncol=2))
graphs <- list(i,ii,iii,vi,v)
grid.arrange(i,ii,iii,iv,v,
             layout_matrix = rbind(c(1, 2),c(1, 3),c(4, 5),c(4, 5) ))

#Make a 3D scatterplot of MWMT, MAT, and MAP with different colors&symbols of BIOME
library(scatterplot3d)
shapes = c(15, 16, 17, 18) 
shapes <- shapes[factor(data$BIOME)] 
colors <- c("#999999", "#E69F00", "#56B4E9", "#99CC00")
colors <- colors[factor(data$BIOME)]

#Add vertical lines connecting the points to the floor
scatterplot3d(data$MWMT, data$MAT, data$MAP, xlab="MWMT", ylab="MAT", zlab="MAP", 
              color=colors, pch = shapes, type="h")
#Add legend for different BIOME
legend("topright", legend = c("Boreal", "Grassland", "Montane", "Parkland"),
       col = c("#999999", "#E69F00", "#56B4E9", "#99CC00"), pch = c(15, 16, 17, 18),
       inset = -0.005, xpd = TRUE)

shapes = c(15, 16, 17, 18) 
shapes <- shapes[factor(data$BIOME)] 
colors <- c("#999999", "#E69F00", "#56B4E9", "#99CC00")
colors <- colors[factor(data$BIOME)]

#Add vertical lines connecting the points to the floor
scatterplot3d(data$MCMT, data$AHM, data$MSP, xlab="MCMT", ylab="AHM", zlab="MSP", 
              color=colors, pch = shapes, type="h")
#Add legend for different BIOME
legend("topright", legend = c("Boreal", "Grassland", "Montane", "Parkland"),
       col = c("#999999", "#E69F00", "#56B4E9", "#99CC00"), pch = c(15, 16, 17, 18),
       inset = -0.005, xpd = TRUE)

#Make a side-by-side boxplot of TD for all categories of BIOME. 
boxplot(data$TD ~ data$BIOME, col="#E69F00", main="TD for each BIOME", xlab="BIOME", ylab="TD")

#Superimpose the mean TD for each biome in each boxplot
means <- by(data$TD, data$BIOME, mean)   
points(1:4, means, pch = 1, cex = 1)
text(1:4, means, labels = round(means, digits=2), 
     pos = 3, cex = 1, col = "black")
