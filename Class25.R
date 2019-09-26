## Import Data

draft <- read.csv("http://math.carleton.edu/Chihara/Stats275/draft70.csv")
draft[258,]

## Plot

library(ggplot2)

ggplot(draft, aes(x = Birthday, y = Rank)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

## Cor

cor(draft$Birthday, draft$Rank)

N <- 10^4
cor.boot <- numeric(N)
n <- nrow(draft)

for (i in 1:N){
  index <- sample(n, replace = TRUE)
  draftBoot <- draft[index,]
  cor.boot[i] <- cor(draftBoot$Birthday, draftBoot$Rank)
}

temp <- data.frame(cor.boot)

ggplot(temp, aes(x = cor.boot))+
  geom_histogram()

quantile(cor.boot, c(0.025,0.975))

## lm

draft.lm <- lm(Rank ~ Birthday, data = draft)

summary(draft.lm)

ggplot(draft, aes(x = Birthday, y = Rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Rank v. Month

ggplot(draft, aes(x = Month, y = Rank)) + 
  geom_boxplot()

tapply(draft$Rank, draft$Month, mean)

table(draft$Month, draft$Rank2)

out <- table(draft$Month, draft$Rank2)
chisq.test(out)

## Rank v. Year

ggplot(draft, aes(x = Year, y = Rank)) + 
  geom_boxplot()

tapply(draft$Rank, draft$Year, mean)

perm.result <- numeric(N)
observed <- tapply(draft$Rank, draft$Year, mean)[1]-tapply(draft$Rank, draft$Year, mean)[2]

firstHalf <- subset(draft, select = Rank, subset = Year == "FirstHalf", drop = T)
secondHalf <- subset(draft, select = Rank, subset = Year == "SecondHalf", drop = T)

for (i in 1:N){
  index <- sample(n, size = length(firstHalf), replace = FALSE)
  perm.result[i] <- mean(draft$Rank[index]) - mean(draft$Rank[-index])
}

hist(perm.result, xlab = "xbarF-xbarS", main = "Permutation distribution")
abline(v = observed, col = "red", lty=5)

(sum(perm.result >= observed)+1)/(N+1)
