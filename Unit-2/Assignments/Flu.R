fluTrain = read.csv("FluTrain.csv")
sort(tapply(fluTrain$ILI, fluTrain$Week, sum))
sort(tapply(fluTrain$Queries, fluTrain$Week, sum))
subset(fluTrain, Queries == max(Queries))

hist(fluTrain$ILI)

plot(log(fluTrain$ILI), fluTrain$Queries)

fluTrend1 = lm(log(ILI) ~ Queries, data = fluTrain)
summary(fluTrend1)
corvalue = cor(fluTrain$ILI, fluTrain$Queries)
corvalue^2

fluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(fluTrend1, newdata=fluTest))

which(fluTest$Week == "2012-03-11 - 2012-03-17")
(fluTest$ILI[11] - 2.187378)/fluTest$ILI[11]

SSE = sum((fluTest$ILI - PredTest1) ^ 2)
RMSE = sqrt(SSE/nrow(fluTest))

library(zoo)

ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)

fluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

plot(log(fluTrain$ILILag2), fluTrain$ILI)

fluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
summary(fluTrend2)

ILILag2Test = lag(zoo(fluTest$ILI), -2, na.pad = TRUE)
fluTest$ILILag2 = coredata(ILILag2Test)
summary(fluTest)

tail(fluTrain$ILI)
fluTest$ILILag2[1] = fluTrain$ILI[nrow(fluTrain) - 1]
fluTest$ILILag2[2] = fluTrain$ILI[nrow(fluTrain)]

fluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
PredTest2 = exp(predict(fluTrend2, newdata = fluTest))
RMSE = sqrt(mean((fluTest$ILI - PredTest2) ^ 2))
SSE = sum((fluTest$ILI - PredTest2)^2)
