Bushmeat <- read.csv("http://math.carleton.edu/Chihara/Stats275/Bushmeat.csv")
head(Bushmeat)

library(ggplot2)
ggplot(Bushmeat, aes(x = Fish, y = Change)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

bush.lm <- lm(Change ~ Fish, data = Bushmeat, na.action = na.exclude)  
summary(bush.lm)

coef(bush.lm)[2]-qt(c(0.975,0.025), 27)*0.1341

head(resid(bush.lm))
Bushmeat$Residuals <- resid(bush.lm)

ggplot(Bushmeat, aes(x = Fish, y = Residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red")


N <- 10^4
n <- nrow(Bushmeat)

result <- numeric(N)

for (i in 1:N){
  index <- sample(n, n, replace = TRUE)
  BushResample <- Bushmeat[index,]
  boot.lm <- lm(Change ~ Fish, data = BushResample)
  result[i] <- coef(boot.lm)[2]
}

quantile(result, c(0.025, 0.975))

result2 <- numeric(N)

for (i in 1:N){
  index <- sample(n, n, replace = TRUE)
  BushResample <- Bushmeat[index,]
  boot.lm <- lm(Change ~ Fish, data = BushResample)
  result2[i] <- -coef(boot.lm)[1]/coef(boot.lm)[2]
}

quantile(result2, c(0.025, 0.975))

ggplot() + geom_histogram(aes(result2))
