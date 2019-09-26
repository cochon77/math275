LocalSnacks <- read.csv("http://math.carleton.edu/Chihara/Stats275/LocalSnacks.csv")
View(LocalSnacks)
dim(LocalSnacks)
colMeans(LocalSnacks[,2:3])
Diff <- LocalSnacks$FamilyFare - LocalSnacks$Target
observed <- mean(Diff)
N <- 10^5 -1
result <- numeric(N)
for(i in 1:N){
  Sign <- sample(c(-1,1), size = 15, replace = TRUE)
  Diff2 <- Sign*Diff
  result[i] <- mean(Diff2)
}
hist(result,xlab = "FF-T", main = "Permutation distribution for matched pairs")
abline(v = observed, col = "red", lty = 5)
2*(sum(result >=observed)+1)/(N+1)

##Sampling Distribution

N <- 10^5-1
Xbar <- numeric(N)

for(i in 1:N){
  x <-rpois(25,15)
  Xbar[i] <- mean(x)
}

hist(Xbar, main = "Sampling dist of means")
mean(Xbar)
sd(Xbar)
