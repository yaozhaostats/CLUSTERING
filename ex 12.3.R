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