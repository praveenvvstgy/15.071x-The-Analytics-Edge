healthy = read.csv("healthy.csv", header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

image(healthyMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

healthyVector = as.vector(healthyMatrix)
str(healthyVector)

n = 365636
n * (n - 1) / 
# distance = dist(healthyVector, method = "euclidean")
# large amtrix required, Hierarchical clustering not possible
	
k = 5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyClusters = KMC$cluster
dim(healthyClusters) = dim(healthyMatrix)
image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor = read.csv("tumor.csv")
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = dim(tumorMatrix)
image(tumorClusters, axes = FALSE, col = rainbow(k))
