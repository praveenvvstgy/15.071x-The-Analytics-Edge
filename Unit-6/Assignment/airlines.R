airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

str(airlinesNorm)
summary(airlinesNorm)

distance = dist(airlinesNorm, method = "euclidean")
airlinesCluster = hclust(distance, method = "ward.D")
plot(airlinesCluster)

clusterGroups = cutree(airlinesCluster, k = 5)
table(clusterGroups)

lapply(split(airlines, clusterGroups), colMeans)

k = 5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMC)
sum(KMC$size > 1000)

lapply(split(airlines, KMC$cluster), colMeans)
