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

CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtmHeadline = DocumentTermMatrix(CorpusHeadline)
dtmHeadline = removeSparseTerms(dtmHeadline, 0.99)
Headline = as.data.frame(as.matrix(dtmHeadline))
colnames(Headline) = make.names(colnames(Headline))
colnames(Headline) = paste("H", colnames(Headline))

dtmSnippet = DocumentTermMatrix(CorpusSnippet)
dtmSnippet = removeSparseTerms(dtmSnippet, 0.99)
Snippet = as.data.frame(as.matrix(dtmSnippet))
colnames(Snippet) = make.names(colnames(Snippet))
colnames(Snippet) = paste("S", colnames(Snippet))

dtmAbstract = DocumentTermMatrix(CorpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.99)
Abstract = as.data.frame(as.matrix(dtmAbstract))
colnames(Abstract) = make.names(colnames(Abstract))
colnames(Abstract) = paste("A", colnames(Abstract))

str(Headline)
str(Snippet)
news = cbind(Headline, Snippet, Abstract)

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

library(rpart)
library(rpart.plot)

table(NewsTrain$Popular)
5439 / (5439 + 1093)
CART = rpart(Popular ~ . - UniqueID, data = NewsTrain, method = "class")
prp(CART)

predCART = predict(CART, newdata = NewsTest)
predCART
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predCART[,2])
write.csv(MySubmission, "SubmissionCART.csv", row.names=FALSE)
