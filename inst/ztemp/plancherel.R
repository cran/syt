library(syt)

connectedParts <- function(lambda0){
  lambda <- lambda0
  lambda[1L] <- lambda[1L]+1L
  out <- list(lambda)
  for(i in seq_along(lambda0)[-1L]){
    if(lambda0[i]<lambda0[i-1L]){
      lambda <- lambda0
      lambda[i] <- lambda[i]+1L
      out <- c(out, list(lambda))
    }
  }
  c(out, list(c(lambda0,1L)))
}


p0 <- 1L
path <- list(p0)
p1_set <- connectedParts(p0)
p1 <- sample(p1_set, 1L, prob=c(0.5,0.5))
path <- c(path, p1)

n <- 4
p0 <- 1L
path <- list(p0)
for(i in seq_len(n-1L)){
  set <- connectedParts(path[[i]])
  f_nu <- count_sytx(path[[i]])
  probs <- sapply(set, function(p) count_sytx(p))/(i+1)/f_nu
  path <- c(path, sample(set, 1L, prob=probs))
}
path


