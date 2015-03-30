# Stock Dynamics

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

# Convert Date
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# How many observations are there in each data set?
str(IBM)

# What is the earliest year in our datasets?
# What is the latest year in our datasets?
summary(Boeing)

# What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)

# What is the minimum stock price of General Electric (GE) over 
# this time period?
min(GE$StockPrice)

# What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)

# What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)

# What is the standard deviation of the stock price of Procter & Gamble
# over this time period?
sd(ProcterGamble$StockPrice)

# Around what year did Coca-Cola has its highest stock price
# in this time period?

# Around what year did Coca-Cola has its lowest stock price in this time period?
jpeg('cococolaplot.jpg')
plot(CocaCola$Date, CocaCola$StockPrice, col = "red")

# In March of 2000, the technology bubble burst, and a stock
# market crash occurred. According to this plot, which company's stock 
# dropped more?
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v = as.Date(c("2000-03-01")), lwd = 2)

# Around 1983, the stock for one of these companies (Coca-Cola or Procter 
# and Gamble) was going up, while the other was going down. Which one was 
# going up?
abline(v = as.Date(c("1983-01-01")), lwd = 2)
dev.off()

# VISUALIZING STOCK DYNAMICS 1995-2005
jpeg('stock1995-2005.jpg')
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")

lines(GE$Date[301:432], GE$StockPrice[301:432], col="pink")

lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="gray")

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="orange")

abline(v = as.Date(c("2000-01-01")))
abline(v = as.Date(c("1997-09-01")))
abline(v = as.Date(c("1997-10-01")))

dev.off()

# MONTHLY TRENDS

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

tapply(GE$StockPrice, months(GE$Date), mean)
mean(GE$StockPrice)

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
mean(ProcterGamble$StockPrice)

tapply(Boeing$StockPrice, months(Boeing$Date), mean)
mean(Boeing$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)
