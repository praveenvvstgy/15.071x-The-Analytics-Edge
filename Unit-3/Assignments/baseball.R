baseball = read.csv("baseball.csv")

nrow(table(baseball$Year))

baseball = subset(baseball, Playoffs == 1)

table(baseball$Year, baseball$Playoffs)

PlayOffTable = table(baseball$Year)
PlayOffTable
names(PlayOffTable)
PlayOffTable[c("1990", "2001")]
PlayOffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayOffTable[as.character(baseball$Year)]
