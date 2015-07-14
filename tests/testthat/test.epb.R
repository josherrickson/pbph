context("epb")

test_that("epb class", {
  x <- 1:5
  y <- 1:5
  e <- epbm(1,c(1,2), lm(y~x), lm(x~y))
  expect_true(length(e) == 4)
  expect_true(all(sapply(e,class) == rep(c("numeric", "lm"), each=2)))

  expect_error(epb("a", c(1,2), lm(y~x), lm(x~y)))
})

test_that("epb", {
  # Need to expand these a LOT

  # Testing valid CI's (have to set seed because this data is
  # terribly fit, so its going to be mostly Inf)
  set.seed(134)
  x1 <- rnorm(10)
  x2 <- rnorm(10)
  y <- x1+x2+rnorm(10)
  d <- data.frame(y,x1,x2)
  rm(y,x1,x2)
  treatment <- rep(0:1, each=5)

  e <- epb(y ~ . , treatment, d)

  expect_equal(length(e), 4)
  expect_true(is(e, "epbm"))
  # Regardless of input, bounds should capture estimate
  expect_true(all(is.finite(e$bounds)))
  expect_true(e$est > e$bounds[1])
  expect_true(e$est < e$bounds[2])

  # Inf CI
  set.seed(1352)
  x1 <- rnorm(10)
  x2 <- rnorm(10)
  y <- x1+x2+rnorm(10)
  d <- data.frame(y,x1,x2)
  rm(y,x1,x2)
  treatment <- rep(0:1, each=5)

  e <- epb(y ~ . , treatment, d)

  expect_equal(length(e), 4)
  expect_true(is(e, "epbm"))
  expect_true(all(!is.finite(e$bounds)))

  # using profile.likelihood should slightly alter results
  e2 <- epb(y ~ . , treatment, d, profile.likelihood=TRUE)

  expect_false(e$est == e2$est)
})
