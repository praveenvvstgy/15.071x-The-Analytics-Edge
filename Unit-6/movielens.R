movies = read.table("movielens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL

movies = unique(movies)
str(movies)

sum(movies$Comedy)
sum(movies$Western)
sum(movies$Romance & movies$Drama)

distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title == "Toy Story (1995)")

clusterGroups[1]

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

clusterGroups2 = cutree(clusterMovies, k = 2)
tapply(movies$Action, clusterGroups2, mean)
tapply(movies$Adventure, clusterGroups2, mean)
tapply(movies$Animation, clusterGroups2, mean)
tapply(movies$Childrens, clusterGroups2, mean)
tapply(movies$Comedy, clusterGroups2, mean)
tapply(movies$Crime, clusterGroups2, mean)
tapply(movies$Documentary, clusterGroups2, mean)
tapply(movies$Drama, clusterGroups2, mean)

clusterGroups

colMeans(subset(movies[4:21], clusterGroups2 == 1))

spl = split(movies[4:21], clusterGroups2)
lapply(spl, colMeans)
