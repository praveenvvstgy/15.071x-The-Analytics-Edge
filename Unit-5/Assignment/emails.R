emails = read.csv("emails.csv", stringsAsFactors = FALSE)
sum(emails$spam)
str(emails)

max(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam

rev(sort(colSums(subset(emailsSparse, spam == 0))))

sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = binomial)

library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data = train, method = "class")

library(randomForest)

set.seed(123)
spamRF = randomForest(spam ~ ., data = train)

predLog = predict(spamLog)
sum(predLog <  0.00001)
sum(predLog > 0.99999)
sum(predLog <=  0.00001 & predLog >= 0.99999)

summary(spamLog)

prp(spamCART)

table(train$spam, predLog >= 0.5)
(3052 + 954) / (3052 + 954 + 4)

library(ROCR)

logPredROCR = prediction(predLog, train$spam)
auc = as.numeric(performance(logPredROCR, "auc")@y.values)
auc

predCART = predict(spamCART)
predCART.pred = predCART[,2]
table(train$spam, predCART.pred >= 0.5)
(2885 + 894) / (2885 + 894 + 67 + 167)

predROCR = predict(spamCART)
predCARTROCR = prediction(predROCR[,2], train$spam)
as.numeric(performance(predCARTROCR, "auc")@y.values)

predRF = predict(spamRF)
table(train$spam, predRF)
(3013 + 914) / (3013 + 914 + 44 + 39)


predRF = predict(spamRF, type = "prob")
predRFROCR = prediction(predRF[,2], train$spam)
as.numeric(performance(predRFROCR, "auc")@y.values)


predLog = predict(spamLog, newdata = test)
table(test$spam, predLog >= 0.5)
(1258 + 376) / (1258 + 376 + 34 + 50)

logPredROCR = prediction(predLog, test$spam)
auc = as.numeric(performance(logPredROCR, "auc")@y.values)
auc


predCART = predict(spamCART, newdata = test)
predCART.pred = predCART[,2]
table(test$spam, predCART.pred >= 0.5)
(1228 + 386) / (1228 + 386 + 24 + 80)

predROCR = predict(spamCART, newdata = test)
table(test$spam, predROCR[,2] >= 0.5)
(1228 + 386) / (1228 + 386 + 24 + 80)
predROCR = prediction(predCART.pred, test$spam)
as.numeric(performance(predROCR, "auc")@y.values)

predRF = predict(spamRF, newdata = test, type = "prob")
table(test$spam, predRF[,2] >= 0.5)
(1290 + 386) / (1290 + 386 + 24 + 18)
predRFROCR = prediction(predRF[,2], test$spam)
as.numeric(performance(predRFROCR, "auc")@y.values)
