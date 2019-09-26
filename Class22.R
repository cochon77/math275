theta <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
prior <- c(0.01,0.01,0.09,0.2,0.23,0.29,0.15,0.01,0.01)

likelihood <- theta^2*(1-theta)^5
likelihood
priorLike <- prior*likelihood
priorLike
probData <- sum(priorLike)
posterior <- priorLike/probData

sum(theta*prior)
sum(theta*posterior)

prior2 <- posterior
likelihood2 <- theta^4*(1-theta)^5
priorLike2 <- prior2*likelihood2
probData2 <- sum(priorLike2)
posterior2 <- priorLike2/probData2
posterior2

sum(theta*posterior2)

likelihood3 <- theta^6*(1-theta)^10
priorLike3 <- prior*likelihood3
probData3 <- sum(priorLike3)
posterior3 <- priorLike3/probData3
posterior3

plot(theta, prior, type = "b", ylim = c(0,1), ylab = "Probability")
lines(theta, posterior, type = "b", lty = 2, pch = 2)
lines(theta, posterior3, type = "b", lty = 3, pch = 3)
legend("topleft", legend = c("prior","posterior","posterior3"), lty = 1:3)

