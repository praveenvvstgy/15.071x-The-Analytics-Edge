# AN ANALYTICAL DETECTIVE
mvt = read.csv("mvtWeek1.csv")

# How many rows of data (observations) are in this dataset?
# How many variables are in this dataset?
str(mvt)

# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)

# What is the minimum value of the variable "Beat"?
min(mvt$Beat)

# How many observations have value TRUE in the Arrest variable 
# (this is the number of crimes for which an arrest was made)?
nrow(subset(mvt, Arrest == TRUE))

# How many observations have a LocationDescription value of ALLEY?
nrow(subset(mvt, LocationDescription == "ALLEY"))

# In what format are the entries in the variable Date?
mvt$Date[1]

# What is the month and year of the median date in our dataset?
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
# May 2006

# In which month did the fewest motor vehicle thefts occur?
mvt$Date = DateConvert
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
table(mvt$Month)

# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)

# Which month has the largest number of motor vehicle thefts
# for which an arrest was made?
table(mvt$Month, mvt$Arrest)

# Plot
# In general, does it look like crime increases or decreases from 2002 - 2012?
# In general, does it look like crime increases or decreases from 2005 - 2008?
# In general, does it look like crime increases or decreases from 2009 - 2011?
jpeg('crimedate.jpg')
hist(mvt$Date, breaks = 100)
dev.off()

# Boxplot

# Does it look like there were more crimes for which arrests 
# were made in the first half of the time period or the second half 
# of the time period? 
jpeg('arrest.jpg')
boxplot(mvt$Date ~ mvt$Arrest)
dev.off()

# For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Year, mvt$Arrest)

# Which locations are the top five locations for motor vehicle thefts,
# excluding the "Other" category?
rev(sort(table(mvt$LocationDescription)))

# How many observations are in Top5?
Top5 = subset(mvt, LocationDescription == "STREET" 
			  | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"
			  | LocationDescription == "ALLEY"
			  | LocationDescription == "GAS STATION"
			  | LocationDescription == "DRIVEWAY - RESIDENTIAL")
nrow(Top5)

# One of the locations has a much higher arrest rate than 
# the other locations. Which is it?
Top5$LocationDescription = factor(Top5$LocationDescription)

table(Top5$Arrest, Top5$LocationDescription)

# On which day of the week do the most motor vehicle thefts at 
# gas stations happen?

# On which day of the week do the fewest motor vehicle thefts 
# in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)
