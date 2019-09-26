plot(x = c(22,28), y = c(1,100), type = "n", xlab = "", ylab = "")
abline(v = 25, col = "red")
counter <- 0


for (i in 1:1000){
  x <- rnorm(30, 25, 4)
  L <- mean(x) + qnorm(0.025)*4/sqrt(30)
  U <- mean(x) + qnorm(0.975)*4/sqrt(30)
  if (L < 25 && 25 < U)
    counter <- counter + 1
  if (i<=100)
    segments(L,i,U,i)
}

counter/1000
