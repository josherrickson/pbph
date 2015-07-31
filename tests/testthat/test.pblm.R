context("pblm")

test_that("pblm", {
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

  e <- pblm(mod1, treatment, d)
  se <- summary(e)

  expect_true(is(e, "pblm"))
  expect_true(is(e, "lm"))
  expect_true(is(se, "summary.lm"))

  # Check NA's
  expect_true(all(is.na(se$coefficients[2, 3:4])))

})
