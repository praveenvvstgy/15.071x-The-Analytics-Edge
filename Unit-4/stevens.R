stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 5)
prp(StevensTree)

predictCart = predict(StevensTree, type = "class", newdata = test)
table(test$Reverse, predictCart)
(41 + 71) / (41 + 71 + 22 + 36)

library(ROCR)
predictROC = predict(StevensTree, newdata = test)
head(predictROC)

pred = prediction(predictROC[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
auc = as.numeric(performance(pred, "auc")@y.values)
auc
