#' @importFrom utils head
#' @noRd
removezeros <- function(x){ # e.g c(3,1,0,0) -> c(3,1)
  i <- match(0L, x)
  if(!is.na(i)) head(x, i-1L) else x
}
removeTrailingZeros <- function(x) {
  n <- length(x)
  while(n > 0L && x[n] == 0) {
    n <- n - 1L
  }
  head(x, n)
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1 && floor(x) == x
}

isIntegerVector <- function(x) {
  is.numeric(x) && !anyNA(x) && all(x == as.integer(x))
}

arePositiveIntegers <- function(x){
  all(x > 0 & floor(x) == x)
}

areNonnegativeIntegers <- function(x){
  all(x >= 0 & floor(x) == x)
}

isPartition <- function(x){
  is.numeric(x) && areNonnegativeIntegers(x) && all(diff(x) <= 0)
}

checkPartition <- function(x){
  if(isPartition(x)){
    return(as.integer(removezeros(x)))
  }else{
    stop("`lambda` is not a partition.", call. = FALSE)
  }
}

isStrictlyIncreasing <- function(x) {
  all(diff(x) > 0)
}

isWeaklyIncreasing <- function(x) {
  all(diff(x) >= 0)
}

checkSYTrows <- function(syt){
  all(vapply(syt, isStrictlyIncreasing, logical(1L)))
}

checkSSYTrows <- function(ssyt){
  all(vapply(ssyt, isWeaklyIncreasing, logical(1L)))
}

checkSYT <- function(syt){
  if(!isSYT(syt)){
    stop("Not a standard Young tableau.", call. = FALSE)
  }else{
    return(invisible())
  }
}

.rg <- function(start, end) {
  #seq_len(end - start + 1L) + (start - 1L)
  if(start <= end) start:end else integer(0L)
}

# the converse of toString
fromString <- function(string) {
  as.integer(strsplit(string, ",", fixed = TRUE)[[1L]])
}

partitionAsString <- function(lambda) {
  sprintf("[%s]", toString(lambda))
}

fromPartitionAsString <- function(string) {
  string <- gsub("(\\[|\\])", "", string)
  as.integer(strsplit(string, ",", fixed = TRUE)[[1L]])
}

# lambda and mu are clean
.isDominatedBy <- function(mu, lambda) {
  n <- sum(lambda)
  lambda <- c(lambda, rep(0L, n - length(lambda)))
  dominated <- TRUE
  i <- 1L
  ellMu <- length(mu)
  partialSum_mu <- partialSum_lambda <- 0L
  while(dominated && i <= ellMu) {
    partialSum_mu <- partialSum_mu + mu[i]
    partialSum_lambda <- partialSum_lambda + lambda[i]
    dominated <- partialSum_mu <= partialSum_lambda
    i <- i + 1L
  }
  dominated
}

# #' @importFrom partitions parts
# #' @noRd
# .dominatedPartitions <- function(lambda) {
#   Filter(
#     function(mu) .isDominatedBy(mu, lambda),
#     apply(parts(sum(lambda)), 2L, removeTrailingZeros, simplify = FALSE)
#   )
# }

.dominatedPartitions <- function(lambda) {
  n <- length(lambda)
  if(n == 0L) {
    return(list(integer(0L)))
  }
  go <- function(h, w, dds, e) {
    if(w == 0L) {
      list(integer(0L))
    } else {
      arange <- seq_len(min(h, dds[1L] - e))
      do.call(c, lapply(arange, function(a) {
        L <- go(a, w-a, dds[-1L], e+a)
        lapply(L, function(as) {
          c(a, as)
        })
      }))
    }
  }
  weight <- sum(lambda)
  dsums <- c(cumsum(lambda), rep(weight, weight - n))
  go(lambda[1L], weight, dsums, 0L)
}

#' @importFrom partitions parts
#' @noRd
listOfPartitions <- function(n) {
  if(n == 0L) {
    list(integer(0L))
  } else {
    apply(parts(n), 2L, removeTrailingZeros, simplify = FALSE)
  }
}