pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

table(pisaTrain$readingScore, pisaTrain$male)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals ^ 2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

29.542707 * 9 - 29.542707 * 11

predTest = predict(lmScore, newdata = pisaTest)
predTest
range(predTest)
353.2231 - 637.6914

SSE = sum((pisaTest$readingScore - predTest) ^ 2)
RMSE = sqrt(SSE/nrow(pisaTest))

mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore)) ^ 2)
1 - SSE/SST
