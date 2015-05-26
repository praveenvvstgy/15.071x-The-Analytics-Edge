eBay = read.csv("ebay.csv", stringsAsFactors = FALSE)
table(eBay$sold)
799 / (799 + 2997)

summary(eBay)

table(eBay$size)

eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)

set.seed(144)

library(caTools)

spl = sample.split(eBay$sold, 0.7)

training = subset(eBay, spl == TRUE)
testing = subset(eBay, spl == FALSE)

logModel = glm(sold ~ biddable + startprice + condition + heel + style + color + material, data = training, family = binomial)
summary(logModel)

logPredTest = predict(logModel, newdata = testing, type = "response")
sum((testing$sold == 1) != (logPredTest >= 0.5))

table(testing$sold, logPredTest >= 0.5)
(58 + 877) / (58 + 877 + 22 + 182)

table(testing$sold)

library(ROCR)

ROCRPred = prediction(logPredTest, testing$sold)
auc = performance(ROCRPred, "auc")@y.values
auc

ROCRPerf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize = TRUE)

library(caret)
set.seed(144)
library(e1071)

numFolds = trainControl( method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.001, 0.05, 0.001))
train(sold ~ biddable + startprice + condition + heel + style + color + material, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

library(rpart)
library(rpart.plot)

# Lines missing due to crash


library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(eBay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

spdtm = removeSparseTerms(dtm, 0.9)

descriptionText = as.data.frame(as.matrix(spdtm))

which.max(colSums(descriptionText))

names(descriptionText) = paste0("D", names(descriptionText))

descriptionText$sold = eBay$sold
descriptionText$biddable = eBay$biddable
descriptionText$startprice = eBay$startprice
descriptionText$condition = eBay$condition
descriptionText$heel = eBay$heel
descriptionText$style = eBay$style
descriptionText$color = eBay$color
descriptionText$material = eBay$material

trainText = subset(descriptionText, spl == TRUE)
testText = subset(descriptionText, spl == FALSE)

glmText = glm(sold ~ ., data = trainText, family = "binomial")
summary(glmText)

library(ROCR)

textPred = predict(glmText, type = "response")

textROCRPred = prediction(textPred, trainText$sold)
performance(textROCRPred, "auc")@y.values

textTestPred = predict(glmText, type = "response", newdata = testText)
textROCRTestPred = prediction(textTestPred, testText$sold)
performance(textROCRTestPred, "auc")@y.values
