wiki = read.csv("wiki.csv", stringsAsFactor = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, .997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("A", colnames(wordsRemoved))

str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
table(wikiWords$Vandal)
2061 / (2061 + 1815)

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data = train, method = "class")
prp(wikiCART)

predCART = predict(wikiCART, newdata = test)
predCART.prob = predCART[,2]
table(test$Vandal, predCART.prob >= 0.5)
(618 + 12) / (618 + 12 + 533)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
sum(wikiWords2$HTTP)

train2 = subset(wikiWords2, spl == TRUE)
test2 = subset(wikiWords2, spl == FALSE)

wikiCART2 = rpart(Vandal ~ ., data = train2, method = "class")
prp(wikiCART2)

predCART2 = predict(wikiCART2, newdata = test2)
predCART2.prob = predCART2[,2]
table(test$Vandal, predCART2.prob >= 0.5)
(609 + 57) / (609 + 57 + 9 + 488)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

sum(wikiWords2$NumWordsAdded) / nrow(wikiWords2)

train3 = subset(wikiWords2, spl == TRUE)
test3 = subset(wikiWords2, spl == FALSE)

wikiCART3 = rpart(Vandal ~ ., data = train3, method = "class")
prp(wikiCART3)

predCART3 = predict(wikiCART3, newdata = test3)
predCART3.prob = predCART3[,2]
table(test$Vandal, predCART3.prob >= 0.5)
(514 + 248) / (514 + 248 + 297 + 104)


wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

train4 = subset(wikiWords3, spl == TRUE)
test4 = subset(wikiWords3, spl == FALSE)

wikiCART4 = rpart(Vandal ~ ., data = train4, method = "class")
prp(wikiCART4)

predCART4 = predict(wikiCART4, newdata = test4)
predCART4.prob = predCART4[,2]
table(test$Vandal, predCART4.prob >= 0.5)
(595 + 241) / (595 + 241 + 23 + 304)
