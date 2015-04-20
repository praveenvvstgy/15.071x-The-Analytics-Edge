NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)


dtmHeadline = DocumentTermMatrix(CorpusHeadline)
dtmHeadline = removeSparseTerms(dtmHeadline, 0.99)
Headline = as.data.frame(as.matrix(dtmHeadline))
colnames(Headline) = make.names(colnames(Headline))

news = cbind(Headline)

news$NewsDesk = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
news$SectionName = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
news$SubsectionName = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
news$WordCount = c(NewsTrain$WordCount, NewsTest$WordCount)
news$Weekday = c(strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S"), strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S"))$wday
news$Month = c(strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S"), strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S"))$mon
news$UniqueID = c(NewsTrain$UniqueID, NewsTest$UniqueID)

Train = head(news, nrow(NewsTrain))
NewsTest = tail(news, nrow(NewsTest))
Train$Popular = NewsTrain$Popular
NewsTrain = Train


library(caTools)
spl = sample.split(NewsTrain$Popular, SplitRatio = 0.7)
train = subset(NewsTrain, spl == TRUE)
test = subset(NewsTrain, spl == FALSE)

table(test$Popular)
1632 / (1632 + 328)

logModel = bayesglm(Popular ~ get + morn + read + today + word + NewsDesk + SectionName + SubsectionName + WordCount + Weekday, data = train, family = "binomial")
summary(logModel)
predictLog = predict(logModel, newdata = test, type = "response")
table(test$Popular, predictLog >= 0.5)
(1568 + 212) / (1568 + 212 + 64 + 116)

library(ROCR)

predROCR = prediction(predictLog, test$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
auc = as.numeric(performance(predROCR, "auc")@y.values)
auc

library(arm)
logModel = bayesglm(Popular ~ get + morn + read + today + word + NewsDesk + SectionName + SubsectionName + WordCount + Weekday, data = NewsTrain, family = "binomial")
predictLog = predict(logModel, newdata = NewsTest, type = "response")
summary(logModel)
sum(predictLog >= 0.5)
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predictLog)
write.csv(MySubmission, "final4.csv", row.names=FALSE)
