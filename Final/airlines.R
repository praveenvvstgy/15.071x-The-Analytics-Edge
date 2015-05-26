Airlines = read.csv("AirlineDelay.csv")

set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]


linearModel = lm(TotalDelay ~ ., data = AirlinesTrain)
summary(linearModel)

cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)

cor(AirlinesTrain$OriginAvgWind, AirlinesTrain$OriginWindGust)


linearPredTest = predict(linearModel, newdata = AirlinesTest)
linearSSE = sum((AirlinesTest$TotalDelay - linearPredTest)^2)
linearSSE

linearSST = sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)
linearSST

linearRSq = 1 - linearSSE / linearSST
linearRSq


Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
table(Airlines$DelayClass)

Airlines$TotalDelay = NULL

set.seed(15071)
library(caTools)

spl = sample.split(Airlines$DelayClass, SplitRatio = 0.7)
AirlinesTrain = subset(Airlines, spl == TRUE)
AirlinesTest = subset(Airlines, spl == FALSE)

library(rpart)
library(rpart.plot)

CARTModel = rpart(DelayClass ~ ., data = AirlinesTrain)
prp(CARTModel)

CARTPredTrain = predict(CARTModel, type = "class")
table(AirlinesTrain$DelayClass, CARTPredTrain)
(361 + 3094) / (361 + 3094 + 804 + 314 + 1806 + 188)

table(AirlinesTrain$DelayClass)
3282 / (3282 + 1118 + 2167)

CARTPredTest = predict(CARTModel, newdata = AirlinesTest, type = "class")
table(AirlinesTest$DelayClass, CARTPredTest)
(1301 + 153) / (1301 + 153 + 141 + 338 + 776 + 105)
