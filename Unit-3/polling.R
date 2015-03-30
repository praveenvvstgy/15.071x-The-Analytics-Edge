polling = read.csv("PollingData.csv")
str(polling)

table(polling$Year)

library(mice)

simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]

set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

summary(polling)

train = subset(polling, Year == 2004  | Year == 2008)
test = subset(polling, Year == 2012)
table(train$Republican)
table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

cor(train[c("Rasmussen", "PropR", "SurveyUSA", "Republican", "DiffCount")])

mod1 = glm(Republican ~ PropR, family = binomial, data = train)
summary(mod1)

pred1 = predict(mod1, type="response")
table(train$Republican, pred1 > 0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, family = binomial, data = train)
summary(mod2)
pred2 = predict(mod2, type="response")
table(train$Republican, pred2 > 0.5)

table(test$Republican, sign(test$Rasmussen))
testpred = predict(mod2, type="response", newdata=test)
table(test$Republican, testpred > 0.5)
