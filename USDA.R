# Read the data
USDA = read.csv("USDA.csv")

# Structure
str(USDA)

# Summary
summary(USDA)

# Which food has so much Sodium?
USDA$Description[which.max(USDA$Sodium)]

# High Sodium Foods
HighSodium = subset(USDA, Sodium > 10000)
HighSodium$Description

# How much Sodium in Caviar

USDA$Sodium[match("CAVIAR", USDA$Description)]

# Plot protein and fat
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main="Plot of Protein and Fat Conten", col="red")

# Histogram of Vitamin
hist(USDA$VitaminC, xlim = c(0,100), breaks=2000, xlab="Vitamin C (mg)", main = "Histogram of Vitamin C")

# Boxplot of sugar content
boxplot(USDA$Sugar, ylab="Sugar", main="Boxplot of sugar content")

# Adding new variables
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighCarbohydrate = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))

# High Sodium and High Carbohydrate
table(USDA$HighSodium, USDA$HighCarbohydrate)

# Average amount of iron sorted by high and low protein
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

# Relation between Vitamin C and Carbohydrate content
tapply(USDA$VitaminC, USDA$HighCarbohydrate, summary, na.rm = TRUE)
