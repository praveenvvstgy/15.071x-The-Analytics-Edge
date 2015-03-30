parole = read.csv("parole.csv")
sum(parole$violator)
summary(parole)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

mod1 = glm(violator ~ ., data = train, family = binomial)
summary(mod1)

y = (-4.2411574) + (0.3869904) + (0.8867192) + (-0.0001756 * 50) + (-0.1238867 * 3) + (0.0802954 * 12) + (0.6837143)
py = 1 / (1 + exp(-y))

py/(1 - py)

predictTest = predict(mod1, type="response", newdata = test)
summary(predictTest)

table(test$violator, predictTest >= 0.5)

12 / (12 + 11)
167 / (167 + 12)
(167 + 12) / (167 + 12 + 12 + 11)

table(test$violator)
179/ (179 + 23)

library(ROCR)

ROCRpred = prediction(predictTest, test$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

