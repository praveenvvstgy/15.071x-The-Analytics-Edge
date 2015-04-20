stocks = read.csv("StocksCluster.csv")
table(stocks$PositiveDec)
6324/ (6324 + 5256)

sort(cor(stocks))

colSums(stocks)

library(caTools)
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
predModel = predict(StocksModel, type = "response")
predModel


table(stocksTrain$PositiveDec, predModel >= 0.5)
(990 + 3640) / (990 + 3640 + 787 + 2689)

testPred = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, testPred >= 0.5)
(417 + 1553) / (417 + 1553 + 344 + 1160)

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

summary(normTrain)

k = 3
set.seed(144)
km = kmeans(normTrain, centers = k)
str(km)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
sum(clusterTest == 2)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)

(30 + 774) / (30 + 774 + 23 + 471)
(388 + 757) / (388 + 757 + 309 + 626)
(49 + 13) / (49 + 13 + 21 + 13)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions >= 0.5)
(467 + 1544) / (467 + 1544 + 1110 + 353)
