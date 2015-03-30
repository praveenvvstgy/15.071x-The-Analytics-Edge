songs = read.csv("songs.csv")
str(songs)
table(songs$year)
table(songs$artistname == "Michael Jackson")
MichaelJacksonTop10 = subset(songs, artistname == "Michael Jackson" & Top10 == 1)

table(songs$timesignature)

max(songs$tempo)
subset(songs, tempo == 244.307)

train = subset(songs, year <= 2009)
test = subset(songs, year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[, !(names(train) %in% nonvars)]
test = test[, !(names(test) %in% nonvars)]

mod1 = glm(Top10 ~ ., data = train, family = binomial)
summary(mod1)

cor(train$energy, train$loudness)

mod2 = glm(Top10 ~ . - loudness, data = train, family = binomial)
summary(mod2)

mod3 = glm(Top10 ~ . - energy, data = train, family = binomial)
summary(mod3)

predictTest = predict(mod3, type = "response", newdata = test)
table(test$Top10, predictTest >= 0.45)
(309 + 19) / (309 + 19 + 5 + 40)

table(test$Top10)
314 / (314 + 59)

19 / (19 + 40)

309 / (309 + 5)
