#This function shuffles a sequence of integer numbers from 1:n.
#The function only requires le largest number in the sequence as an argument
shuffle <- function(n){
      x <- 1:n
      x.1 <- numeric(n)
      for(i in 1:(n-1)){
            x.1[i] <- sample(x, 1)
            x <- x[-which(x == x.1[i])]
      }
      x.1[n] <- x
      return(x.1)
}

