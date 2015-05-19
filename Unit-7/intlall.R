intall = read.csv("intlall.csv", stringsAsFactors = FALSE)

str(intall)

intall[is.na(intall)] = 0

str(intall)

worldMap = map_data("world")

str(worldMap)

mit_map = merge(worldMap, intall, by.x = "region", by.y = "Citizenship")
str(mit_map)

ggplot(mit_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", col = "black") + coord_map("mercator")

mit_map = mit_map[order(mit_map$group, mit_map$order),]

ggplot(mit_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", col = "black") + coord_map("mercator")

table(intall$Citizenship)

intall$Citizenship[intall$Citizenship == "China (People's Republic Of)"] = "China"

table(intall$Citizenship)

mit_map = merge(worldMap, intall, by.x = "region", by.y = "Citizenship")

mit_map = mit_map[order(mit_map$group, mit_map$order),]

ggplot(mit_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), col = "black") + coord_map("mercator")

ggplot(mit_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), col = "black") + coord_map("ortho", orientation = c(20, 30, 0))

ggplot(mit_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), col = "black") + coord_map("ortho", orientation = c(20, 30, 0))
