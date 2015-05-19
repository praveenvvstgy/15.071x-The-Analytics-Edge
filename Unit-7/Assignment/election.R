library(maps)
library(ggmap)
library(ggplot2)

statesMap = map_data("state")
str(statesMap)

table(statesMap$group)
