context("confint.pblm")

test_that("confint.pblm", {

  # Force finite CI
  set.seed(32432)
  n <- 100
  d <- data.frame(abc = rnorm(n),
                  x = rnorm(n),
                  z = rnorm(n))
  t <- rep(0:1, each = n/2)
  d$abc <- d$x + t + d$x*t*.5 + rnorm(n)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  ci <- confint(e)
  ci

  expect_equal(dim(ci), c(2,2))
  expect_equal(rownames(ci), c("treatment", "pred"))

  # Ci's should have lower and upper correct
  expect_true(all(apply(ci, 1, function(x) diff(x) > 0)))

  expect_equal(ci[1,], confint.lm(e)[1,])
  expect_false(isTRUE(all.equal(ci[2,], confint.lm(e)[2,])))

  expect_true(e$coef[1] > ci[1,1] & e$coef[1] < ci[1,2])
  expect_true(e$coef[2] > ci[2,1] & e$coef[2] < ci[2,2])

  # Force infinite CI
  set.seed(4)
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  ci <- confint(e)

  expect_true(all(!is.finite(ci[2,])))

})

test_that("CI arguments", {

  # Force finite CI
  set.seed(32432)
  n <- 100
  d <- data.frame(abc = rnorm(n),
                  x = rnorm(n),
                  z = rnorm(n))
  t <- rep(0:1, each = n/2)
  d$abc <- d$x + t + d$x*t*.5 + rnorm(n)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  ci <- confint(e)
  ci

  ci1 <- confint(e, parm = "treatment")
  expect_equal(rownames(ci1), "treatment")
  expect_equal(ci1[1,], ci[1,], check.attributes = FALSE)

  ci2 <- confint(e, parm = "pred")
  expect_equal(rownames(ci2), "pred")
  expect_equal(ci2[1,], ci[2,], check.attributes = FALSE)

  ci3 <- confint(e, parm = c("pred", "pred", "treatment"))
  expect_equal(rownames(ci3), c("pred", "pred", "treatment"))

  # Confidence level should shrink
  ci4 <- confint(e, level = .5)
  expect_true(all((ci4 - ci)[,1] > 0))
  expect_true(all((ci4 - ci)[,2] < 0))
})

test_that("wald-style CI's", {
  set.seed(8)
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  ci <- confint(e)
  ci2 <- confint(e, wald.style = TRUE)

  expect_false(identical(ci[2,],ci2[2,]))

})
