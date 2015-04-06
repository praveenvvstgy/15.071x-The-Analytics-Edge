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

table(baseball$NumCompetitors == 8)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
sum(baseball$WorldSeries == 0)

mod1 = glm(WorldSeries ~ Year, data = baseball, family = binomial)
summary(mod1)

mod2 = glm(WorldSeries ~ RS, data = baseball, family = binomial)
summary(mod2)

mod3 = glm(WorldSeries ~ RA, data = baseball, family = binomial)
summary(mod3)

mod4 = glm(WorldSeries ~ W, data = baseball, family = binomial)
summary(mod4)

mod5 = glm(WorldSeries ~ OBP, data = baseball, family = binomial)
summary(mod5)

mod6 = glm(WorldSeries ~ SLG, data = baseball, family = binomial)
summary(mod6)

mod7 = glm(WorldSeries ~ BA, data = baseball, family = binomial)
summary(mod7)

mod8 = glm(WorldSeries ~ RankSeason, data = baseball, family = binomial)
summary(mod8)

mod9 = glm(WorldSeries ~ OOBP, data = baseball, family = binomial)
summary(mod9)

mod10 = glm(WorldSeries ~ OSLG, data = baseball, family = binomial)
summary(mod10)

mod11 = glm(WorldSeries ~ NumCompetitors, data = baseball, family = binomial)
summary(mod11)

mod12 = glm(WorldSeries ~ League, data = baseball, family = binomial)
summary(mod12)

mod13 = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(mod13)

mod14 = glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
summary(mod14)

mod15 = glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial)
summary(mod15)

mod16 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
summary(mod16)

mod17 = glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial)
summary(mod17)

mod18 = glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = binomial)
summary(mod18)

mod19 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(mod19)

cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])
