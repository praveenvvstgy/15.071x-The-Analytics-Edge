intl = read.csv("intl.csv")
str(intl)
library(ggplot2)

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl))

intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat="identity", fill = "darkblue") + geom_text(aes(label = PercentOfIntl), vjust = -0.4) + ylab("Percentage of International Students") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
