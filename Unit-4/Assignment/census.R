census = read.csv("census.csv")

library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

logmodel = glm(over50k ~ ., data = train, family = binomial)
summary(logmodel)

predlog = predict(logmodel, newdata = test, type = "response")
table(predlog >= 0.5, test$over50k)
(9051 + 1888) / (9051 + 1888 + 662 + 1190)

table(test$over50k)
9713 / (9713 + 3078)

library(ROCR)
ROCRpred = prediction(predlog, test$over50k)
performance(ROCRpred, "auc")@y.values
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperflibrary(rpart)

library(rpart.plot)

tree = rpart(over50k ~ ., data = train, method = "class")
prp(tree)

predtree = predict(tree, newdata = test, type = "class")
table(predtree, test$over50k)
(9243 + 1596) / (1596 + 9243 + 470 + 1482)
predtree = predict(tree, newdata = test)
ROCRpred = prediction(predtree[,2], test$over50k)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
performance(ROCRpred, "auc")@y.values
plot(ROCRperf)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000),]

set.seed(1)
library(randomForest)
forest = randomForest(over50k ~ ., data = trainSmall)
predforest = predict(forest, newdata = test)
table(predforest, test$over50k)
(9586 + 1093) / (9586 + 1093 + 1985 + 127)

vu = varUsed(forest, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

varImpPlot(forest)


set.seed(2)
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number = 10)
grid = expand.grid(.cp = seq(0.002, 0.1, 0.002))
train(over50k ~ ., data = train, method="rpart", trControl = numFolds, tuneGrid = grid)

tree = rpart(over50k ~ ., data = train, method = "class", cp = 0.002)
prp(tree)

predtree = predict(tree, newdata = test, type = "class")
table(predtree, test$over50k)
(9178 + 1838) / (9243 + 1596 + 470 + 1482)
nrow(tree$splits)
