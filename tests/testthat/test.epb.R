context("epb")

test_that("epb", {
  # Need to expand these a LOT
  set.seed(134)
  x1 <- rnorm(10)
  x2 <- rnorm(10)
  y <- x1+x2+rnorm(10)
  d <- data.frame(y,x1,x2)
  rm(y,x1,x2)
  treatment <- rep(0:1, each=5)

  e <- epb(y ~ . , treatment, d)

  expect_equal(length(e), 3)
  expect_true(is.numeric(e))
  # Regardless of input, bounds should capture estimate
  expect_true(e[1] > e[2])
  expect_true(e[1] < e[3])

  # using profile.likelihood should slightly alter results
  e2 <- epb(y ~ . , treatment, d, profile.likelihood=TRUE)

  expect_false(isTRUE(all.equal(e,e2)))
})
