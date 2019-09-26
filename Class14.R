w <- numeric(10^4)
n <- 25
for (i in 1:10^4){
  x <- rnorm(n, 30,7)
  xbar <- mean(x)
  s <- sd(x)
  w[i] <- (xbar-30)/(s/sqrt(n))
}

hist(w, main = "Distribution of statistic", prob = TRUE, ylim=c(0, 0.4))
curve(dnorm(x), add = TRUE, col = "red")

qqnorm(w)
qqline(w)

##Population Assumption

n <- 20
q <- qt(0.975, n-1)

trueMean <- 10
tooLow <- 0
tooHigh <- 0

for (i in 1:10^5){
  x <- rchisq(n,10)
  xbar <- mean(x)
  s <- sd(x)
  L <- xbar - q*s/sqrt(n)
  U <- xbar + q*s/sqrt(n)
  if (U < trueMean) tooLow <- tooLow + 1
  if (L > trueMean) tooHigh <- tooHigh + 1
}

tooLow/10^5
tooHigh/10^5

##Camparison with t-dist

n <- 20
tstat <- numeric(10^4)
for(i in 1:10^4){
  x <- rchisq(n,10)
  tstat[i] <- (mean(x)-10)/(sd(x)/sqrt(n))
}

hist(tstat, main = "dist of tstat")

tsample <- rt(10^4, n-1)
plot(sort(tsample), sort(tstat))
abline(0,1)
