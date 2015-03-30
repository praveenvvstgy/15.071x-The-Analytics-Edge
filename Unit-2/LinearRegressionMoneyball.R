baseball = read.csv("baseball.csv")

plot(baseball$RD, baseball$W)
baseball$RD = baseball$RS - baseball$RA
moneyball = subset(baseball, Year < 2002)

summary(moneyball)
WinReg = lm(W ~ RD, data = moneyball)
summary(WinReg)

predict(WinReg, newdata = data.frame("RD" = 713-614))

RSReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RSReg)

-804.63 + 2737.77 * 0.311 + 1584.91 * 0.405

RAReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RAReg)

-837.38 + 2913.60 * 0.297 + 1514.29 * 0.370

qq = read.csv("QuickQuestion.csv")
summary(qq)

qqPredict = predict(RSReg, newdata = qq)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)
