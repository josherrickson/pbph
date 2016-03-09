context("First stage glm")

test_that("logistic", {
  set.seed(1)
  d <- data.frame(abc = sample(0:1, 10, TRUE),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)
  suppressWarnings(mod1g <- glm(abc ~ x + z, data = d, subset = t == 0, family = "binomial"))

  e <- pblm(mod1, t, d)
  eg <- pblm(mod1g, t, d)

  expect_is(eg, "pblm")
  expect_equal(eg, as(eg, "lm"), check.attributes = FALSE)

  expect_true(length(coef(eg)) == 2)
  expect_equal(names(coef(eg)), c("treatment", "pred"))

  expect_true(e$coef[2] != eg$coef[2])

  expect_identical(e$epb$mod1, mod1)
  expect_identical(eg$epb$mod1, mod1g)

  sg <- summary(eg)
  sg.lm <- summary.lm(eg)

  expect_is(sg, "summary.pblm")

  expect_identical(rownames(sg$coef), rownames(sg.lm$coef))
  expect_identical(colnames(sg$coef), colnames(sg.lm$coef))
  expect_false(isTRUE(all.equal(sg$coef, sg.lm$coef)))

  # since s.e. should increase with correction, the default P value
  # should incease as well.
  expect_true(sg$coef["pred","Pr(>|t|)"] > summary(e)$coef["pred","Pr(>|t|)"])


})

test_that("poisson", {
  set.seed(1)
  d <- data.frame(abc = rpois(10, 3),
                  x = rnorm(10),
                  z = rnorm(10))
  t <- rep(0:1, each = 5)

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)
  suppressWarnings(mod1p <- glm(abc ~ x + z, data = d, subset = t == 0, family = "poisson"))

  e <- pblm(mod1, t, d)
  ep <- pblm(mod1p, t, d)

  expect_is(ep, "pblm")
  expect_equal(ep, as(ep, "lm"), check.attributes = FALSE)

  expect_true(length(coef(ep)) == 2)
  expect_equal(names(coef(ep)), c("treatment", "pred"))

  expect_true(e$coef[2] != ep$coef[2])

  expect_identical(e$epb$mod1, mod1)
  expect_identical(ep$epb$mod1, mod1p)


  sp <- summary(ep)
  sp.lm <- summary.lm(ep)

  expect_is(sp, "summary.pblm")

  expect_identical(rownames(sp$coef), rownames(sp.lm$coef))
  expect_identical(colnames(sp$coef), colnames(sp.lm$coef))
  expect_false(isTRUE(all.equal(sp$coef, sp.lm$coef)))


})
