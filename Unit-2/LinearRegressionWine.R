wine = read.csv("wine.csv")
str(wine)
summary(wine)

model1 = lm(Price ~ AGST, data = wine)
summary(model1)

SSE = sum(model1$residuals ^ 2)

model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)

SSE = sum(model2$residuals ^ 2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

SSE = sum(model3$residuals ^ 2)


model4 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)

cor(wine$HarvestRain, wine$WinterRain)

cor(wine)
