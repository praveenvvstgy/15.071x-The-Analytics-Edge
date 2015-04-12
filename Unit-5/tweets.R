tweets = read.csv("tweets.csv", stringsAsFactor = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

stopwords("english")
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005, 505:515])

findFreqTerms(frequencies, lowfreq = 100)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetSparse = as.data.frame(as.matrix(sparse))
colnames(tweetSparse) = make.names(colnames(tweetSparse))
tweetSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetSparse$Negative, SplitRatio = 0.7)
train = subset(tweetSparse, split == TRUE)
test = subset(tweetSparse, split == FALSE)

library(rpart)
library(rpart.plot)

CART = rpart(Negative ~ ., data = train, method = "class")
prp(CART)

predictCART = predict(CART, newdata= test, type = "class")
table(test$Negative, predictCART)
(294 + 18) / (294 + 18 + 37 + 6)

table(test$Negative)
library(randomForest)
set.seed(123)
RF= randomForest(Negative ~ ., data = train)

predictRF = predict(RF, newdata = test)
table(test$Negative, predictRF)
(292 + 23) / (292 + 23 + 32 + 8)

tweetLM = glm(Negative ~ ., data = train, family = binomial)
predictLM = predict(tweetLM, newdata = test, type = "response")
table(test$Negative, predictLM >= 0.5)
(253 + 28) / (253 + 28 + 27 + 47)
