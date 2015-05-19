murders = read.csv("murders.csv")

str(murders)

statesMap = map_data("state")
str(statesMap)


library(ggplot2)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", col = "black")

murders$region = tolower(murders$State)

murderMap = merge(statesMap, murders, by = "region")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(col = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(col = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

murderMap$murderRate = murderMap$Murders / murderMap$Population*100000

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = murderRate)) + geom_polygon(col = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = murderRate)) + geom_polygon(col = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0, 10))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(col = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
