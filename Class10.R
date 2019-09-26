N <- 10^4
Xbar <-  numeric(N)
Max2 <- numeric(N)

n <- 25

for(i in 1:N){
  x <- runif(n,0,15)
  Xbar[i] <- 2*mean(x)
  Max2[i] <- (n+1)/n * max(x)
}

Range <- range(Xbar)

out <- par(mfrow = c(2,1))

hist(Xbar, xlim = Range, main = "MOM")

hist(Max2, xlim = Range, main = "MLE")

par(out)

mean(Xbar)
sd(Xbar)

mean(Max2)
sd(Max2)
