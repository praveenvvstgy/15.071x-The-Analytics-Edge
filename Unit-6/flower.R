flower = read.csv("flower.csv", header = FALSE)
str(flower)

flowerMatrix = as.matrix(flower)
str(flowerMatrix)

flowerVector = as.vector(flowerMatrix)

str(flowerVector)

distances = dist(flowerVector, method = "euclidean")

clusterIntensity = hclust(distances, method = "ward.D")
plot(clusterIntensity)

rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters = cutree(clusterIntensity, k = 3)

flowerClusters
tapply(flowerVector, flowerClusters, mean)

dim(flowerClusters) = c(50, 50)
image(flowerClusters, axes = FALSE)

image(flowerMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))
