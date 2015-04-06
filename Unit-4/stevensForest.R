stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)

install.packages("randomForest")
library(randomForest)

train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

set.seed(200)
StevesForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)

predictForest = predict(StevesForest, newdata = test)
table(test$Reverse, predictForest)
(44 + 76) / ( 44 + 76 + 17 + 33)
