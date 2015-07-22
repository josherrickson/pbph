context("epb")

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

  mod1 <- lm(y ~ ., data=d, subset=treatment==0)

  e <- epb(mod1, treatment, d)

  expect_true(is(e, "summary.lm"))
  # Regardless of input, bounds should capture estimate
  expect_true(all(is.finite(e$coefficients[2,5:6])))
  expect_true(e$coefficients[2,1] > e$coefficients[2,5])
  expect_true(e$coefficients[2,1] < e$coefficients[2,6])

  # Inf CI
  set.seed(1352)
  x1 <- rnorm(10)
  x2 <- rnorm(10)
  y <- x1+x2+rnorm(10)
  d <- data.frame(y,x1,x2)
  rm(y,x1,x2)
  treatment <- rep(0:1, each=5)

  mod1 <- lm(y ~ ., data=d, subset=treatment==0)

  e <- epb(mod1, treatment, d)

  expect_true(is(e, "summary.lm"))
  expect_true(all(!is.finite(e$coefficients[2,5:6])))

})
