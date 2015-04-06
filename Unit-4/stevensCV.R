stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp = 0.19)
predictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, predictCV)
(59 + 64) / (59 + 64 + 29 + 18)

prp(StevensTreeCV)
