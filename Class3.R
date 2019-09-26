library(resampledata)
str(Beerwings)
boxplot(Hotwings ~ Gender, data = Beerwings)
tapply(Beerwings$Hotwings, Beerwings$Gender, mean)
observed <- 14.5333 - 9.333
hotwings <- subset(Beerwings, select = Hotwings, drop=TRUE)
N <- 10^5-1
result <- numeric(N)
head(result)
for(i in 1:N){
  index <- sample(30, size = 15, replace=FALSE) # get sample from 1:30 with size 15
  result[i] <- mean(hotwings[index]) - mean(hotwings[-index]) 
  # get mean of selected/unselected hotwings and subtract
}
hist(result, xlab = "xbarM-xbarF", main = "Permutation distribution for hot wings")
abline(v = observed, col = "red", lty=5)
(sum(result >= observed)+1)/(N+1)
