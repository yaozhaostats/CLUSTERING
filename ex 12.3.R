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

hc.eu.c <- hclust(dist.eu, "average")
hc.eu.c$merge
hc.eu.c$height
plot(hc.eu.c,hang=-1, main="Public utility companies",
     labels=as.character(utility[,9]))
