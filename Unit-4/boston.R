boston = read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS ==1], boston$LAT[boston$CHAS ==1], col="blue", pch=19)
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col="red", pch=19)
summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col="green", pch=19)

plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)

latlonglm  = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonglm)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)

latlonglm$fitted.values
points(boston$LON[latlonglm$fitted.values > 21.2], boston$LAT[latlonglm$fitted.values > 21.2], col="blue", pch="$")


library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues > 21.2], boston$LAT[fittedvalues > 21.2], col="blue", pch="$")

latlontree = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
prp(latlontree)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues > 21.2], boston$LAT[fittedvalues > 21.2], col="blue", pch="$")


library(caTools)
spl = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, spl == TRUE)
test = subset(boston, spl == FALSE)
lnreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + AGE + RM + DIS + RAD + TAX + PTRATIO, data = train)
summary(lnreg)
linreg.pred = predict(lnreg, newdata = test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + AGE + RM + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
tree.pred = predict(tree, newdata = test)
tree.sse = sum((tree.pred - test$MEDV)^2)


library(caret)
library(e1071)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)

tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + AGE + RM + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred - test$MEDV) ^ 2)
