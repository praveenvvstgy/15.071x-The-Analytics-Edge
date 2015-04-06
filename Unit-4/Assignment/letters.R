letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
table(test$isB)
1175 / (1175 + 383)

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . -letter, data = train, method = "class")
predCartb = predict(CARTb, newdata = test, type = "class")
table(predCartb, test$isB)
(1114 + 351) / ( 1114 + 351 + 61 + 32)

library(randomForest)
Forestb = randomForest(isB ~ . -letter, data = train)
predictForest = predict(Forestb, newdata = test)
table(predictForest, test$isB)
(1162 + 373) / (1162 + 373 + 23)


letters$letter = as.factor(letters$letter)
set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

table(test$letter)
401 / (395 + 383 + 401 + 379)

CART = rpart(letter ~ . -isB, data = train, method = "class")
predCART = predict(CART, newdata = test, type = "class")
table(predCART, test$letter)
(350 + 314 + 366 + 338) / nrow(test)

set.seed(1000)
Forest = randomForest(letter ~ . -isB, data = train)
predictForest = predict(Forest, newdata = test)
table(predictForest, test$letter)
(395 + 374 + 397 + 362) / nrow(test)
