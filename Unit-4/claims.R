claims = read.csv("ClaimsData.csv")
str(claims)

table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)

train = subset(claims, spl == TRUE)
test = subset(claims, spl == FALSE)

mean(train$age)
table(train$diabetes)/nrow(train)

table(test$bucket2009, test$bucket2008)
(110138 + 10721 +  2774 + 1539 + 104)/nrow(test)

PenaltyMatrix = matrix(c(0, 1, 2, 3, 4, 2, 0, 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 0, 3, 8, 6, 4, 2, 0), byrow = TRUE, nrow = 5)
PenaltyMatrix

sum(as.matrix(table(test$bucket2009, test$bucket2008))*PenaltyMatrix)/nrow(test)

nrow(subset(test, bucket2009 == 1))/nrow(test)

test$tmp = 1
sum(as.matrix(table(test$bucket2009, test$tmp)) * PenaltyMatrix[,1])/nrow(test)
str(test)
claimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = train, method = "class", cp = 0.00005)
prp(claimsTree)

predictTest = predict(claimsTree, newdata = test, type = "class")
table(test$bucket2009, predictTest)
(114141 + 16102 + 118 + 201 + 0) / nrow(test)

sum(as.matrix(table(test$bucket2009, predictTest))*PenaltyMatrix)/nrow(test)

claimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = train, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix))
predictTest = predict(claimsTree, newdata = test, type = "class")
table(test$bucket2009, predictTest)
(94310 + 19017 + 4645 + 640 + 0) / nrow(test)
sum(as.matrix(table(test$bucket2009, predictTest))*PenaltyMatrix)/nrow(test)

