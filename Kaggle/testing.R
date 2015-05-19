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
colnames(Headline) = paste('H', colnames(Headline))

CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))

CorpusSnippet = tm_map(CorpusSnippet, tolower)

CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)


dtmSnippet = DocumentTermMatrix(CorpusSnippet)
dtmSnippet = removeSparseTerms(dtmSnippet, 0.99)
Snippet = as.data.frame(as.matrix(dtmSnippet))
colnames(Snippet) = make.names(colnames(Snippet))
colnames(Snippet) = paste("S", colnames(Snippet))

news = cbind(Headline, Snippet)

news$NewsDesk = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
news$SectionName = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
news$SubsectionName = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
news$WordCount = c(NewsTrain$WordCount, NewsTest$WordCount)
news$Weekday = c(strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S"), strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S"))$wday
news$Month = c(strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S"), strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S"))$mon
news$ampm = c(strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S"), strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S"))$hour > 12
news$UniqueID = c(NewsTrain$UniqueID, NewsTest$UniqueID)

Train = head(news, nrow(NewsTrain))
NewsTest = tail(news, nrow(NewsTest))
Train$Popular = NewsTrain$Popular
NewsTrain = Train

library(arm)
logModel = bayesglm(Popular ~ . - UniqueID, data = NewsTrain, family = "binomial")
predictLog = predict(logModel, newdata = NewsTest, type = "response")
summary(logModel)

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predictLog)
write.csv(MySubmission, "final9.csv", row.names=FALSE)

NewsTest$Popular = MySubmission$Probability1 >= 0.5

NewsTrain = rbind(NewsTrain, NewsTest)
