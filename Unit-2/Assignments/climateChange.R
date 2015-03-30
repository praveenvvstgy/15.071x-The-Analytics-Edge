climate = read.csv("climate_change.csv")
climate_train = subset(climate, Year <= 2006)
climate_test = subset(climate, Year > 2006)

climateReg1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train) 

summary(climateReg1)

cor(climate$CFC.11, climate$N2O)

cor(climate)

climateReg2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climate_train)
summary(climateReg2)

step(climateReg1)

climateReg3 = lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train)
summary(climateReg3)

tempPredictions = predict(climateReg3, newdata = climate_test)
SSE = sum((climate_test$Temp - tempPredictions) ^ 2)
SST = sum((climate_test$Temp - mean(climate_train$Temp)) ^ 2)
1 - SSE/SST
