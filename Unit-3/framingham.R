framingham = read.csv("framingham.csv")
str(framingham)

library(caTools)

set.seed(1000)

split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

framinghamTrain = subset(framingham, split == TRUE)
framinghamTest = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., family = binomial, data = framinghamTrain)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = "response", newdata = framinghamTest)
table(framinghamTest$TenYearCHD, predictTest > 0.5)

library(ROCR)

ROCRPred = prediction(predictTest, framinghamTest$TenYearCHD)
as.numeric(performance(ROCRPred, "auc")@y.values)

