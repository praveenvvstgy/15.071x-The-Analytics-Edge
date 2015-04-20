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
dtmHeadline
dtmHeadline = removeSparseTerms(dtmHeadline, 0.99)
dtmHeadline
Headline = as.data.frame(as.matrix(dtmHeadline))
colnames(Headline) = make.names(colnames(Headline))

news = Headline

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

library(caret)
library(e1071)

numFolds = trainControl(method = "cv", number = 15)
cpGrid = expand.grid(.cp = seq(0.001, 0.05, 0.001))
train(Popular ~ . -UniqueID, data = NewsTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
CART = rpart(Popular ~ . - UniqueID, data = NewsTrain, method = "class", cp = 0.002)
prp(CART)
predictCART = predict(CART, newdata = NewsTest)
predictCART

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predictCART[,2])
write.csv(MySubmission, "SubmissionMinCART.csv", row.names=FALSE)
