emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)

emails$email[1]

strwrap(emails$email[2])

table(emails$responsive)

library(tm)

corpus = Corpus(VectorSource(emails$email))

corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)

corpus[[1]]

dtm = DocumentTermMatrix(corpus)
dtm

dtm = removeSparseTerms(dtm, 0.97)
dtm

labeledterms = as.data.frame(as.matrix(dtm))
labeledterms$responsive = emails$responsive

str(labeledterms)

library(caTools)
set.seed(144)
spl = sample.split(labeledterms$responsive, SplitRatio = 0.7)
train = subset(labeledterms, spl == TRUE)
test = subset(labeledterms, spl == FALSE)

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data = train, method = "class")
prp(emailCART)

predCART = predict(emailCART, newdata = test)
pred.prob = predCART[,2]
table(test$responsive, pred.prob >= 0.5)
(195 + 25) / (195 + 25 + 17 + 20)

table(test$responsive)
(215) / (215 + 42)

library(ROCR)

predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)

auc = as.numeric(performance(predROCR, "auc")@y.values)
auc
