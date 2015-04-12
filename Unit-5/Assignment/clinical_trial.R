trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
summary(trials)

max(nchar(trials$abstract))
sum(nchar(trials$abstract) == 0)

min(nchar(trials$title))
which(nchar(trials$title) == 28)
trials$title[1258]

library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)

corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)
str(dtmAbstract)
max(colSums(dtmTitle))
max(colSums(dtmAbstract))
which(colSums(dtmAbstract) == 8381)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)

library(caTools)
set.seed(144)

spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(dtm$trial)
1043 / (1043 + 817)

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)
predCARTTrain = predict(trialCART)
predCARTTrain.pred = predCARTTrain[,2]
max(predCARTTrain.pred)
table(train$trial, predCARTTrain.pred >= 0.5)
(631 + 441) / (631 + 441 + 99 + 131)
441 / (441 + 131)
631 / (99 + 631)

predCARTTest = predict(trialCART, newdata = test)
predCARTTest.pred = predCARTTest[,2]
table(test$trial, predCARTTest.pred >= 0.5)
(261 + 162) / (261 + 162 + 52 + 83)

library(ROCR)
predROCR = prediction(predCARTTest.pred, test$trial)
auc = as.numeric(performance(predROCR, "auc")@y.values)
auc
