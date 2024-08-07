test_that("count_ssytx", {
  N <- count_ssytx(c(4, 3, 3, 2), 5)
  expect_equal(N, 450)
})

test_that("all_ssytx", {
  ssytx <- all_ssytx(c(2, 1), 3)
  expect_length(ssytx, 8L)
  #
  lambda <- c(2, 2, 1)
  n <- 4
  ssytx <- all_ssytx(lambda, n)
  expect_length(ssytx, count_ssytx(lambda, n))
})

test_that("isSSYT", {
  ssytx <- all_ssytx(c(2, 1), 4)
  expect_true(all(vapply(ssytx, isSSYT, logical(1L))))
})

test_that("Comparison all_ssytx with ssytx_withGivenShapeAndWeight", {
  lambda <- c(4, 2, 1); n <- 5
  ssytx <- all_ssytx(lambda, n)
  ssytxMatrix <- orderedMatrix(do.call(
    rbind,
    lapply(ssytx, function(ssyt) {
      do.call(c, ssyt)
    })
  ))
  compos <- partitions::compositions(sum(lambda), n)
  ssytx2 <- do.call(
    c, 
    apply(compos, 2L, function(compo) {
      ssytx_withGivenShapeAndWeight(lambda, compo)
    }, simplify = FALSE)
  )
  ssytxMatrix2 <- orderedMatrix(do.call(
    rbind,
    lapply(ssytx2, function(ssyt) {
      do.call(c, ssyt)
    })
  ))
  expect_true(
    all(ssytxMatrix == ssytxMatrix2)
  )
})