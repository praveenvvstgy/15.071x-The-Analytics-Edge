loans = read.csv("loans.csv")
table(loans$not.fully.paid)

1533 / (8045 + 1533)

str(loans)
summary(loans)

no.na = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
mean(no.na$int.rate)
mean(loans$int.rate, na.rm = TRUE)

library(mice)
set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

loan.imputed.manual = complete(mice(loans[vars.for.imputation]))
loan.imputed.auto = read.csv("loans_imputed.csv")
if(loan.imputed.auto == loan.imputed.manual) {
	"true"
}

library(caTools)
set.seed(144)
split = sample.split(loan.imputed.auto$not.fully.paid, SplitRatio = 0.7)
train = subset(loan.imputed.auto, split == TRUE)
test = subset(loan.imputed.auto, split == FALSE)

mod1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(mod1)

logitA = (700 * -9.317e-03)
logitB = (710 * -9.317e-03)
logitA - logitB
exp(logitA - logitB)

predicted.risk = predict(mod1, type="response", newdata = test)
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk >= 0.5)

(2400 + 3) / (2400 + 3 + 457 + 13)

table(test$not.fully.paid)

2413 / (2413 + 460)

library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

mod2 = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
testPred = predict(mod2, newdata = test, type = "response")
summary(testPred)
table(test$not.fully.paid, testPred >= 0.5)

ROCRPred2 = prediction(testPred, test$not.fully.paid)
auc2 = as.numeric(performance(ROCRPred2, "auc")@y.values)
auc2

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

highint = subset(test, int.rate >= 0.15)
mean(highint$profit)
table(highint$not.fully.paid)
110/(110+327)

cutoff = sort(highint$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highint, predicted.risk <= cutoff)
sum(selectedLoans$profit)
sum(selectedLoans$not.fully.paid)
