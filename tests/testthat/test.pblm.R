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

  expect_true(e$call$formula[[2]] == "abc - pred")
  expect_true(all(!isTRUE(grepl("0", as.character(e$call$formula[[3]])))))
  expect_true(all(!isTRUE(grepl("y", colnames(model.frame(e))))))

  # Checking extra pieces in pblm
  expect_true(!is.null(e$epb))
  expect_equal(names(e$epb), c("mod1", "data", "cluster"))
  expect_equal(e$epb$mod1, mod1)
  expect_equal(names(e$epb$data), c(names(d), "treatment", "pred"))
  expect_equal(e$epb$data$treatment, t)
  expect_equal(e$epb$data[,1:3], d)
  expect_true(length(e$epb$data$pred) == nrow(d))

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
                  z = rnorm(10),
                  c = c(1,1,1,2,2,3,3,4,4,4))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)

  e <- pblm(mod1, t, d)

  bnm <- createBreadAndMeat(e)

  expect_is(bnm, "list")
  expect_equal(length(bnm), 4)
  expect_identical(names(bnm), c("b11", "b22", "m11", "m22"))
  expect_true(all(c(3,3,2,2,3,3,2,2) == sapply(bnm, dim)))

  e2 <- pblm(mod1, t, d, cluster = c)
  expect_identical(e$coef, e2$coef)

  bnm.nocluster <- createBreadAndMeat(e2)
  expect_identical(bnm, bnm.nocluster)

  bnm.cluster <- createBreadAndMeat(e2, cluster = d$c)
  expect_identical(bnm$b11, bnm.cluster$b11)
  expect_identical(bnm$b22, bnm.cluster$b22)
  expect_true(!isTRUE(all.equal(bnm$m11, bnm.cluster$m11)))
  expect_true(!isTRUE(all.equal(bnm$m22, bnm.cluster$m22)))
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

test_that("testinverse", {
  data(eottest)
  mod1 <- lm(test ~ gpa + male, data = eottest, subset = (afterschool == 0))
  mod2 <- pblm(mod1, eottest$afterschool, eottest)

  t <- testinverse(mod2)
  expect_is(t, "numeric")
  expect_equal(length(t), 2)
  expect_true(t[1] < t[2])
  expect_equal(attr(t, "type"), "finite")

  t2 <- testinverse(mod2, .9)
  expect_true(t[1] < t2[1] & t[2] > t2[2])

  t3 <- testinverse(mod2, .99)
  expect_true(t[1] > t3[1] & t[2] < t3[2])


  # No relationship, infinite CI
  set.seed(1)
  eottest$test <- rnorm(nrow(eottest))
  mod1 <- lm(test ~ gpa + male, data = eottest, subset = (afterschool == 0))
  mod2 <- pblm(mod1, eottest$afterschool, eottest)

  t <- testinverse(mod2)
  expect_is(t, "numeric")
  expect_equal(length(t), 2)
  expect_true(all(!is.finite(t)))
  expect_equal(attr(t, "type"), "infinite")

})
