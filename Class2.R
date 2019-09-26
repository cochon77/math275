FlightDelays <- read.csv("https://sites.google.com/site/ChiharaHesterberg/data2/FlightDelays.csv")
View(FlightDelays)
str(FlightDelays)
names(FlightDelays)
head(FlightDelays)
dim(FlightDelays)
table(FlightDelays$Carrier)
barplot(table(FlightDelays$Carrier))
table(FlightDelays$Carrier, FlightDelays$Delayed30)
with(FlightDelays, table(Carrier, Delayed30))
hist(FlightDelays$Delay)
with(FlightDelays, hist(Delay))
delay <- subset(FlightDelays, select = Delay, drop = T) ## vectorize
View(subset(FlightDelays, select = Delay)) ## dataframe without drop = T
mean(delay)
median(delay)
mean(delay, trim = 0.25) ## trim 25% by each sides
range(delay)
max(delay)
min(delay)
var(delay)
sd(delay)
quantile(delay)
tapply(FlightDelays$Delay, FlightDelays$Carrier, mean)
with(FlightDelays, tapply(Delay, Carrier, mean))
tapply(FlightDelays$Delay, FlightDelays$Carrier, median)
boxplot(delay)
boxplot(Delay ~ DepartTime, data = FlightDelays)
qqnorm(delay)
qqline(delay)

# US Census Data

states09 <- read.csv("http://www.math.carleton.edu/Stats215/RLabManual/states09.csv")
str(states09)
hist(states09$TheftsMV) ## motor vehicle thefts per 100000 population
qqnorm(states09$TheftsMV)
qqline(states09$TheftsMV)

# ECDF
x <- c(3,4,4,8,19,19,19,12,15)
plot.ecdf(x)
abline(h = 0.5, col = "red", lty = 2)

# Beerwings
Beerwings <- read.csv("https://sites.google.com/site/ChiharaHesterberg/data2/Beerwings.csv")
beerM <- subset(Beerwings, select = Beer, subset = Gender == "M", drop = T) ## subset Beer variable of male
beerF <- subset(Beerwings, select = Beer, subset = Gender == "F", drop = T) ## subset Beer variable of female
plot.ecdf(beerM, xlab = "ounces")
plot.ecdf(beerF, col = "blue", pch = 2, add = TRUE)
abline(v = 25, lty = 2)
legend("topleft",legend = c("Males","Females"), col = c("black","blue"),pch = c(19,2))

