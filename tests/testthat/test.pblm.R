context("pblm")

test_that("making pblm object", {
  s <- lm(1~1)
  s <- as(s, "pblm")
  expect_true(is(s, "pblm"))
  expect_true(is(s, "lm"))
  # These tests are likely tautologies, but assigning `s` to class
  # `pblm` could error.
})

test_that("pblm input/output", {
  d <- data.frame(abc=rnorm(10),
                  x=rnorm(10),
                  z=rnorm(10))
  t <- rep(0:1, each=5)

  mod1 <- lm(abc ~ x + z, data=d, subset=t==0)

  e <- pblm(mod1, t, d)

  expect_true(is(e, "pblm"))
  expect_equal(e, as(e, "lm"), check.attributes=FALSE)

  expect_true(length(coef(e)) == 2)
  expect_equal(names(coef(e)), c("treatment", "pred"))

  # These are tested explicitly in makemod2, but checking that they
  # don't get overwritten when converting to pblm.

  expect_true(e$call$formula[[2]][[2]] == "abc_t")
  expect_true(all(!isTRUE(grepl("0", as.character(e$call$formula[[3]])))))
  expect_true(all(!isTRUE(grepl("y", colnames(model.frame(e))))))

  # Checking extra pieces in pblm
  expect_true(!is.null(e$epb))
  expect_equal(names(e$epb), c("mod1", "pred", "treatment", "data"))
  expect_equal(e$epb$mod1, mod1)
  expect_equal(e$epb$treatment, t)
  expect_equal(e$epb$data, d)
  expect_true(length(e$epb$pred) == nrow(d))

  expect_error(pblm(mod1, c(0,0,0,0,0,1,1,1,1,2), d),
               "must be indicator")
  expect_error(pblm(lm(abc ~ ., data=d), t, d),
               "only on control")
  expect_error(pblm(mod1, t[1:9], d),
               "same length")
})

test_that("vcov.pblm", {
  d <- data.frame(abc=rnorm(10),
                  x=rnorm(10),
                  z=rnorm(10))
  t <- rep(0:1, each=5)

  mod1 <- lm(abc ~ x + z, data=d, subset=t==0)

  e <- pblm(mod1, t, d)

  v <- vcov(e)

  expect_equal(dim(v), c(2,2))
  expect_equal(colnames(v), rownames(v))
  expect_equal(colnames(v), c("treatment", "pred"))

  expect_true(all(diag(v) > 0))
  expect_equal(v[1,2], v[2, 1])

  expect_true(all(vcov(e) - vcov(as(e, "lm")) != 0))
})
