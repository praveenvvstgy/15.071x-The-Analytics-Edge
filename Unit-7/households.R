households = read.csv("households.csv")
str(households)

library(ggplot2)
library(reshape2)

households[,1:2]

head(melt(households, id = "Year"), n = 30)

ggplot(melt(households, id = "Year"), aes(x = Year, y = value, color = variable)) + geom_line(size = 2) + geom_point(size = 5) + ylab("Percentage of households")
