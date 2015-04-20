dailykos = read.csv("dailykos.csv")

distance = dist(dailykos, method = "euclidean")
dailykosClusters = hclust(distance, method = "ward.D")
plot(dailykosClusters)

clusterGroups = cutree(dailykosClusters, k = 7)
cluster1 = subset(dailykos, clusterGroups == 1)
cluster2 = subset(dailykos, clusterGroups == 2)
cluster3 = subset(dailykos, clusterGroups == 3)
cluster4 = subset(dailykos, clusterGroups == 4)
cluster5 = subset(dailykos, clusterGroups == 5)
cluster6 = subset(dailykos, clusterGroups == 6)
cluster7 = subset(dailykos, clusterGroups == 7)

str(cluster3)
tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))


set.seed(1000)
k = 7
KMC = kmeans(dailykos, centers = k)
str(KMC)

kClusters = KMC$cluster

tail(sort(colMeans(subset(dailykos, kClusters == 1))))
tail(sort(colMeans(subset(dailykos, kClusters == 2))))
tail(sort(colMeans(subset(dailykos, kClusters == 3))))

table(kClusters == 2, clusterGroups)
table(kClusters == 3, clusterGroups)
table(kClusters == 7, clusterGroups)
table(kClusters == 6, clusterGroups)
