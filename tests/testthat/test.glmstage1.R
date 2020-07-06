context("First stage glm")


test_that("logistic", {
  set.seed(2)
  d <- data.frame(abc = sample(0:1, 10, TRUE),
                  x = c(2,5,4,2,1,2,4,3,4,1),
                  z = c(2,2,1,3,6,2,2,4,0,9),
                  t = rep(0:1, each = 5))

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)
  mod1g <- glm(abc ~ x + z, data = d, subset = t == 0,
               family = "binomial")

  e <- pbph(mod1, t, d)
  eg <- pbph(mod1g, t, d)

  expect_is(eg, "pbph")
  expect_equal(eg, as(eg, "lm"), check.attributes = FALSE)

  expect_true(length(coef(eg)) == 2)
  expect_equal(names(coef(eg)), c("treatment", "pred"))

  expect_true(e$coef[2] != eg$coef[2])

  expect_identical(e$pbph$mod1, mod1)
  expect_identical(eg$pbph$mod1, mod1g)

  sg <- summary(eg)
  sg.lm <- summary.lm(eg)

  expect_is(sg, "summary.pbph")

  expect_identical(rownames(sg$coef), rownames(sg.lm$coef))
  expect_identical(colnames(sg$coef), colnames(sg.lm$coef))
  expect_false(isTRUE(all.equal(sg$coef, sg.lm$coef)))

})

test_that("bread and meat with glm", {
  set.seed(2)
  d <- data.frame(abc = sample(0:1, 10, TRUE),
                  x = c(2,5,4,2,1,2,4,3,4,1),
                  z = c(2,2,1,3,6,2,2,4,0,9),
                  t = rep(0:1, each = 5))

  mod1 <- glm(abc ~ x + z, data = d, subset = t == 0,
              family = "binomial")

  e <- pbph(mod1, t, d)

  b11 <- bread11(mod1)

  expect_true(all.equal(dim(b11), c(3,3)))
  expect_equal(b11, t(b11))
  expect_equal(b11, bread22(mod1))

  })


test_that("poisson", {
  set.seed(1)
  d <- data.frame(abc = rpois(10, 3),
                  x = rnorm(10),
                  z = rnorm(10),
                  t <- rep(0:1, each = 5))

  mod1 <- lm(abc ~ x + z, data = d, subset = t == 0)
  suppressWarnings(mod1p <- glm(abc ~ x + z, data = d, subset = t == 0, family = "poisson"))

  e <- pbph(mod1, t, d)
  ep <- pbph(mod1p, t, d)

  expect_is(ep, "pbph")
  expect_equal(ep, as(ep, "lm"), check.attributes = FALSE)

  expect_true(length(coef(ep)) == 2)
  expect_equal(names(coef(ep)), c("treatment", "pred"))

  expect_true(e$coef[2] != ep$coef[2])

  expect_identical(e$pbph$mod1, mod1)
  expect_identical(ep$pbph$mod1, mod1p)


  sp <- summary(ep)
  sp.lm <- summary.lm(ep)

  expect_is(sp, "summary.pbph")

  expect_identical(rownames(sp$coef), rownames(sp.lm$coef))
  expect_identical(colnames(sp$coef), colnames(sp.lm$coef))
  expect_false(isTRUE(all.equal(sp$coef, sp.lm$coef)))


})
