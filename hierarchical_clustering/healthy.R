healthy <- read.csv("./healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy)
healthyVector <- as.vector(healthyMatrix)
str(healthyMatrix)

image(healthyMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

k = 7
set.seed(1)
kmc <- kmeans(healthyVector, centers = k, iter.max = 1000)

str(kmc)

healthyCluster <- kmc$cluster

dim(healthyCluster) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyCluster, axes = FALSE, col = rainbow(k))
