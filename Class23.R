counter1 <- 0
counter2 <- 0 
extracounts <- 0

N<-10^5
n1 <- 25
n2 <- 25

for(i in 1:N){
  x <- rnorm(n1)
  pvalue <- t.test(x, mu = 0)$p.value
  if (pvalue < 0.05) #reject null (false positive)
    counter1 <- counter1 + 1 
  else #do not reject, draw another sample
  {
    extracounts <- extracounts + 1 #keep track of extracounts
    y <- rnorm(n2) #draw another sample from N(0,1)
    w <- c(x,y) #combine samples
    pvalue2 <- t.test(w, mu = 0)$p.value 
    if(pvalue2 < 0.05){ #reject null (false positive)
      counter2 <- counter2 + 1 #keep track of false positive
    }
  }
}

counter1/N #false positive rate from the first sample
counter1/N + counter2/extracounts # total proportion of false positive
