library(resampledata)
time.ILEC <- subset(Verizon, select = Time, subset = Group == "ILEC", drop = T)
time.CLEC <- subset(Verizon, select = Time, subset = Group == "CLEC", drop = T)
observed <-  mean(time.CLEC)/mean(time.ILEC)
observed
time.boot <- numeric(10^5)
nI <- length(time.ILEC)
nC <- length(time.CLEC)

for(i in 1:10^5){
  Iresamp <- sample(time.ILEC, nI, replace = TRUE)
  Cresamp <- sample(time.CLEC, nC, replace = TRUE)
  time.boot[i] <- mean(Cresamp)/mean(Iresamp)
}

mean(time.boot)
sd(time.boot)
hist(time.boot, main = "bootstrap dist of ratio of means", xlab = "xbarC*/xbarI*")
abline(v = observed, lty = 2)
quantile(time.boot, c(0.025,0.975))
