quality = read.csv("quality.csv")

install.packages("caTools")
library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

QualityModel = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityModel)

predictTrain = predict(QualityModel, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityModel2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityModel2)

predictTest= predict(QualityModel, type = "response", newdata = qualityTest)

install.packages("ROCR")
library(ROCR)
ROCRPredTest = prediction(predictTest, qualityTest$PoorCare)
ROCRTestPerf = performance(ROCRPredTest, "tpr", "fpr")
plot(ROCRTestPerf, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))

auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)
