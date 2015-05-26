hubway = read.csv("HubwayTrips.csv")

mean(hubway$Duration)

mean(subset(hubway, Weekday == 1)$Duration)
mean(subset(hubway, Weekday == 0)$Duration)

table(hubway$Morning)
table(hubway$Afternoon)
table(hubway$Evening)

table(hubway$Male)
136505 / (136505 + 48685)

library(caret)
preproc = preProcess(hubway)
hubwaynorm  = predict(preproc, hubway)

max(hubwaynorm$Duration)
max(hubwaynorm$Age)

k = 10

set.seed(5000)

KMC = kmeans(hubwaynorm, centers = k)
str(KMC)
min(KMC$size)
max(KMC$size)

cluster1 = subset(hubway, KMC$cluster == 1)
cluster2 = subset(hubway, KMC$cluster == 2)
cluster3 = subset(hubway, KMC$cluster == 3)
cluster4 = subset(hubway, KMC$cluster == 4)
cluster5 = subset(hubway, KMC$cluster == 5)
cluster6 = subset(hubway, KMC$cluster == 6)
cluster7 = subset(hubway, KMC$cluster == 7)
cluster8 = subset(hubway, KMC$cluster == 8)
cluster9 = subset(hubway, KMC$cluster == 9)
cluster10 = subset(hubway, KMC$cluster == 10)

KMC$centers


k = 20

set.seed(8000)

KMC = kmeans(hubwaynorm, centers = k)
str(KMC)

min(KMC$size)
max(KMC$size)

KMC$centers
