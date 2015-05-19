who = read.csv("WHO.csv")
str(who)

plot(who$GNI, who$FertilityRate)

library(ggplot2)

scatterplot = ggplot(who, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()

scatterplot + geom_point(col = "darkred", shape = 8, size = 3)

fertilityGNIPlot = scatterplot + geom_point(col = "darkred", shape = 8, size = 3) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIPlot)
dev.off()

ggplot(who, aes(x = GNI, y = FertilityRate, col = Region)) + geom_point()

ggplot(who, aes(x = GNI, y = FertilityRate, col = LifeExpectancy)) + geom_point()


ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()

model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)

ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = lm, level = 0.99)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = lm, se = FALSE)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = lm, level = 0.99, col = "orange")


ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = FertilityRate, y = Under15, col = Region)) + geom_point() + scale_color_brewer(palette="Dark2")

colors()
