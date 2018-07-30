library(syt)

# syt2path
syt <- list(c(1,2,4), 3, 5)

M <- syt2matrix(syt)

whichRow <- function(M,x){
  i <- 1L
  idx <- match(x, M[i,])
  while(is.na(idx)){
    i <- i+1L
    idx <- match(x, M[i,])
  }
  i
}

N <- sum(lengths(syt))
path <- vector("list", N)
path[[1]] <- c(1L,integer(length(syt)-1L))

for(k in seq_len(N)[-1L]){
  part <- path[[k-1L]]
  i <- whichRow(M,k)
  part[i] <- part[i]+1L
  path[[k]] <- part
}
lapply(path, removezeros)

# converse: path2syt
path <- list(1, 2, c(2,1), c(3,1), c(3,1,1))
path <- lapply(path, checkPartition)
check1 <- all(diff(lengths(path)) %in% c(0,1))
N <- length(path)
l <- length(path[[N]])
syt <- vector("list", l)
path2 <- replicate(N, integer(l))
for(i in 1L:N){
  path2[,i][seq_along(path[[i]])] <- path[[i]]
}
# check path
diffs <- diff(t(path2))
check2 <- all(apply(diffs, 1, function(x){
  sum(x) == 1 && all(x %in% c(0,1))
}))
syt[[1L]] <- 1L
for(i in seq_len(N-1L)){
  idx <- match(1,diffs[i,])
  syt[[idx]] <- c(syt[[idx]], i+1L)
}
syt
