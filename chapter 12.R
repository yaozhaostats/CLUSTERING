# Example 12.3 (Clustering using single linkage)

library(stats)
d <- matrix(0,5,5)

# replicate matrix D
d[lower.tri(d)] <- c(9,3,6,11,7,5,10,9,2,8)
d <- d+t(d)
d <- as.dist(d)
hc.s <- hclust(d, "single")

# this is difficult to interpret. I escape it.
hc.s$merge

# notice that four circled numbers (distances) in the textbook: 2,3,5,6 
hc.s$height
plot(hc.s)

# A negative value (-1) will cause the labels to hang down from 0
plot(hc.s,hang=-1)

# Another way to create the figure 12.3
library(factoextra)
fviz_dend(hc.s)

# Another beautiful way to create the figure 12.3
par( mar=c(5,5,5,5) ) # Outputs can be different depending on operating system and displays. You can play around with the values in the first line. Try e.g. c(5,5,8,12). These numbers set the margins for bottom, left, top and right in this order.
plot( hc.s,hang=-1, axes=F, labels=F, sub="", xlab="Objects", ylab="Distance" )
axis( 1, 1:length(hc.s$order), labels=hc.s$order, col=NA, pos=0.2 )
axis( 2, 0:max(hc.s$height), las=1 )
points( 1:5, rep(0,5), pch=19 )

# Example 12.5 (Clustering using complete linkage)
hc.c <- hclust(d, "complete")
hc.c$merge
hc.c$height

# Average linkage
hc.a <- hclust(d, "average")
hc.a$merge
hc.a$height

# Example 12.9 (Average linkage clustering of public utilities)
utility <- read.table("http://users.stat.umn.edu/~sandy/courses/8053/Data/Wichern_data/T12-4.DAT", header=FALSE)
rownames(utility) <- as.character(utility[,9])

#clustering observations
dist.eu <- dist(scale(utility[,1:8]), "euclidean") # Table 12.6
# the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". 

hc.eu.c <- hclust(dist.eu, "average") # you can also specify "ward" to replace “average”
hc.eu.c$merge
hc.eu.c$height
plot(hc.eu.c,hang=-1, main="Public utility companies",
     labels=as.character(utility[,9])) 
# Notice that the figure and clustering are different from Figure 12.10 in the textbook.

plot(as.dendrogram(hc.eu.c), horiz=TRUE) # another way to show the clustering

# Example 12.7 (Clustering variables using complete linkage)

#clustering variables
cormtx <- cor(utility[,1:8]) # Table 12.5
dist.cor <- sqrt(2*(1-cormtx)) # This is the key step. The previous correlation can be negaive, so it cannot be used as distance. The correlation is [-1,1], thus we use 1- to make it [0,2]. 0 means the very similar, 2 means very dissimilar, which can be used as distance.
# The reason we can transform this is in the textbook: a new assignement of distances that have the same relative orderings will not change the configuration of the complete linkage cluster.

hc.c <- hclust(as.dist(dist.cor), "complete")
plot(hc.c, hang=-1, main="cluster for variables") # Figure 12.8

# Example 12.12 (K-means clustering of public utilities)
utility.km <- kmeans(utility[,1:8], 4)
utility.km # Notice that if you don't set a seed first, then you will get different results in each run.

# Simulated data
# This is not in the textbook.

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean=.5, sd=.2), ncol=2))
colnames(x) <- c("x", "y")
par(mfrow=c(1,2)) # combine two figures in one screen
plot(x, col=c(rep(1,50),rep(2,50),rep(3,50)), main="true clusters")
(cl <- kmeans(x, 3))
plot(x, col = cl$cluster, main="clusters from k-means")
points(cl$centers, col = 1:3, pch = 8, cex=2)

# Example 12.13 (A model based clustering of the iris data)

library(mclust)
irisBIC <- mclustBIC(iris[,-5], modelNames = "VII") # in the textbook page 705, three forms of Sigma_k is EII, VII, not sure about the third type.
irisBIC # Notice that there are mistakes in the textbook. The software doesn't choose K=3. And when K=7, BIC is -698.5.

# Let's use K=3 from the textbook:
mc <- Mclust(iris[,-5], modelNames = "VII", G = 3)
mc$parameters[2] # the same as estimated centers in the textbook
mc$parameters$variance$scale # estimated variance-covariance scale factors 
mc$parameters$pro # the estimated mixing proportions
mc$BIC 

# start page 706 part in the textbook:
mc <- Mclust(iris[,-5])
mc # R returns VEV,2. VEV is corresponding to the "unconstrained covariances", 2 means 2 clusters.
# Notice that in the textbook, we say 2 clusters. In R, it says 2 mixture components.

mc$parameters$pro  # the estimated mixing probabilities
mc$parameters[2] # the same as estimated group centers in the textbook
mc$parameters$variance$sigma # the two estimated covariance matrices are different

# Now we replicate Figure 12.13. 
# Notice that this figure has 2 clusters (squares and triangles) in each subfigure. So the textbook has a mistake here. The textbook used 2 clusters.
library(tidyverse)
pairs(mc$data, pch = mc$classification)
# If you want to change the shapes, you can map the classification assignment to the desired shape: https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/points.


# Example 12.16

univ <- read.table("http://users.stat.umn.edu/~sandy/courses/8053/Data/Wichern_data/T12-9.DAT", row.names=1)
univ.dist <- dist(scale(univ))
univ.cmd <- cmdscale(univ.dist, 2) # cmdscale: Classical (Metric) Multidimensional Scaling

plot(univ.cmd, type="n", xlab="Dimension 1", ylab="Dimension 2",
     main="cmdscale(university)")
text(univ.cmd, labels=rownames(univ.cmd), cex=.5) # different from textbook



library(MASS)
univ.sm <- sammon(univ.dist) # Sammon's Non-Linear Mapping
plot(univ.sm$points, type="n", xlab="Dimension 1", ylab="Dimension 2",
     main="sammon(university)")
text(univ.sm$points, labels=rownames(univ), cex=.5)
