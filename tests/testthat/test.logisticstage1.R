context("First stage logistic")

test_that("pblm input/output", {
  set.seed(1)
  d <- data.frame(abc=sample(0:1, 10, TRUE),
                  x=rnorm(10),
                  z=rnorm(10))
  t <- rep(0:1, each=5)

  mod1 <- lm(abc ~ x + z, data=d, subset=t==0)
  suppressWarnings(mod1g <- glm(abc ~ x + z, data=d, subset=t==0, family="binomial"))

  e <- pblm(mod1, t, d)
  eg <- pblm(mod1g, t, d)

  expect_is(eg, "pblm")
  expect_equal(eg, as(eg, "lm"), check.attributes=FALSE)

  expect_true(length(coef(eg)) == 2)
  expect_equal(names(coef(eg)), c("treatment", "pred"))

  expect_true(e$coef[2] != eg$coef[2])

  expect_identical(e$epb$mod1, mod1)
  expect_identical(eg$epb$mod1, mod1g)

  sg <- summary(eg)
  sg.lm <- summary.lm(eg)

  # since s.e. should increase with correction, the default P value
  # should incease as well.
  expect_true(sg$coef["pred","Pr(>|t|)"] > sg.lm$coef["pred","Pr(>|t|)"])


})
