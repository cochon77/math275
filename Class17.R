Movies2011 <- read.csv("http://math.carleton.edu/Chihara/Stats275/Movies2011.csv")
gross <- Movies2011$DomesticGross
hist(gross, xlab = "Revenue (Millions)", main = "Movies 2011")
xbar <- mean(gross)
n <- length(gross)
SE <- sd(gross)/sqrt(n)

N <- 10^4
Tstar <- numeric(N)

for (i in 1:N){
  grossBoot <- sample(gross, n, replace = TRUE)
  SEstar <- sd(grossBoot)/sqrt(n)
  Tstar[i] <- (mean(grossBoot)-xbar)/SEstar
}
Q <- quantile(Tstar, c(0.975, 0.025))

xbar - Q*SE

qqnorm(Tstar)

hist(Tstar, prob = TRUE, main = "T*")
abline(v = Q, col="red")

curve(dt(x,n-1), add = TRUE, col = "blue")
abline(v = qt(c(0.025,0.975),n-1), col = "green")

xbar - qt(c(0.975, 0.025),n-1)*SE

## Difference in Means

library(resampledata)
delayFri <- subset(FlightDelays, select = Delay, subset = Day == "Fri", drop = T)
delayMon <- subset(FlightDelays, select = Delay, subset = Day == "Mon", drop = T)
thetahat <- mean(delayFri) - mean(delayMon)

nx <- length(delayFri)
ny <- length(delayMon)

SE <- sqrt(var(delayFri)/nx + var(delayMon)/ny)

N <- 10^4
Tstar <- numeric(N)

for(i in 1:N){
  bootx <- sample(delayFri, nx, replace = TRUE)
  booty <- sample(delayMon, ny, replace = TRUE)
  SEstar <- sqrt(var(bootx)/nx + var(booty)/ny)
  Tstar[i] <- (mean(bootx)-mean(booty)-thetahat)/SEstar
}

quantile(Tstar, c(0.025, 0.975))

hist(Tstar, prob = TRUE, main = "T*")
abline(v = Q, col="red")

curve(dt(x,nx+ny-2), add = TRUE, col = "blue")
abline(v = qt(c(0.025,0.975),nx+ny-2), col = "green")

thetahat - quantile(Tstar, c(0.975, 0.025))*SE

