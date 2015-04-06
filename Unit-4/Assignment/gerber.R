gerber = read.csv("gerber.csv")
str(gerber)

sum(gerber$voting == 1)/nrow(gerber)

tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)

logmodel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(logmodel)

logpred = predict(logmodel, type = "response")
table(gerber$voting, logpred >= 0.3)
(51966 + 134513) / (51966 + 134513 + 100875 + 56730)

table(gerber$voting, logpred >= 0.5)
235388 / (235388 + 108696)

library(ROCR)
predROCR = prediction(logpred, gerber$voting)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR)
auc = as.numeric(performance(predROCR, "auc")@y.values)
auc

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)


CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodelcoly = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodelcoly, digits = 6)

CARTmodelcsx = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodelcsx, digits = 6)

logmodelcs = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(logmodelcs)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logmodelcs, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
