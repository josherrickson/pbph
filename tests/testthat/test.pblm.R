context("pblm")

test_that("making pblm object", {
  s <- lm(1~1)
  s <- as(s, "pblm")
  expect_is(s, "pblm")
  expect_is(s, "lm")
  # These tests are likely tautologies, but assigning `s` to class
  # `pblm` could error.
})

test_that("pblm input/output", {
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  expect_is(e, "pblm")
  expect_equal(e, as(e, "lm"), check.attributes = FALSE)

  expect_true(length(coef(e)) == 2)
  expect_equal(names(coef(e)), c("treatment", "pred"))

  # These are tested explicitly in makemod2, but checking that they
  # don't get overwritten when converting to pblm.

  expect_true(e$call$formula[[2]][[2]] == "abc_t")
  expect_true(all(!isTRUE(grepl("0", as.character(e$call$formula[[3]])))))
  expect_true(all(!isTRUE(grepl("y", colnames(model.frame(e))))))

  # Checking extra pieces in pblm
  expect_true(!is.null(e$epb))
  expect_equal(names(e$epb), c("mod1", "pred", "treatment", "data", "cluster"))
  expect_equal(e$epb$mod1, mod1)
  expect_equal(e$epb$treatment, t)
  expect_equal(e$epb$data, d)
  expect_true(length(e$epb$pred) == nrow(d))

  expect_error(pblm(mod1, c(0,0,0,0,0,1,1,1,1,2), d),
               "must be indicator")
  expect_error(pblm(lm(abc ~ ., data = d), t, d),
               "only on control")
  expect_error(pblm(mod1, t[1:9], d),
               "same length")
})

test_that("corrVar and vcov", {
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  cv <- corrVar(e$coef[2],e)

  expect_equal(dim(cv), c(2,2))
  expect_equal(colnames(cv), rownames(cv))
  expect_equal(colnames(cv), c("treatment", "pred"))

  expect_true(all(diag(cv) > 0))
  expect_equal(cv[1,2], cv[2, 1])

  expect_identical(cv, vcov(e))

  expect_true(all(vcov(e) - vcov(as(e, "lm")) != 0))
})

test_that("createBreadAndMeat", {
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  bnm <- createBreadAndMeat(e)

  expect_is(bnm, "list")
  expect_equal(length(bnm), 4)
  expect_identical(names(bnm), c("b11", "b22", "m11", "m22"))
  expect_true(all(c(3,3,2,2,3,3,2,2) == sapply(bnm, dim)))

})

test_that("Hypothesis Test", {

  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  ht <- hypothesisTest(e)

  expect_is(ht, "numeric")
  expect_equal(length(ht), 1)
})

test_that("summary.pblm", {
  d <- data.frame(abc = rnorm(10),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  s <- summary(e)

  expect_is(s, "summary.pblm")

  expect_equal(e$coef, s$coef[,1])
  expect_true(all.equal(s$coef[2,3], hypothesisTest(e),
                        check.attributes = FALSE))

  expect_true(all.equal(s$coef[2,4],
                        2*pt(abs(hypothesisTest(e)),2, lower.tail = FALSE),
                        check.attributes = FALSE))
  expect_equal(s$cov.unscaled, vcov(e))

  expect_identical(summary.lm(e), summary(as(e, "lm")))
  slm <- summary.lm(e)

  expect_equal(slm$coef[,1], s$coef[,1])
  expect_true(all(!slm$coef[,-1] == s$coef[,-1]))
})
