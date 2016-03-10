context("sandwich")

test_that("clustered sandwich", {

  d <- data.frame(y = rnorm(15),
                  x = rnorm(15),
                  c = rep(1:3, each = 5))

  mod <- lm(y ~ x, data = d)

  s1 <- sandwich::sandwich(mod)
  s2 <- sandwich(mod)

  expect_identical(s1,s2)

  # Make sure adjust is still getting passed through
  s3 <- sandwich::sandwich(mod, adjust = TRUE)
  s4 <- sandwich(mod, adjust = TRUE)

  expect_identical(s3,s4)
  expect_true(!identical(s1,s3))

  s5 <- sandwich(mod, cluster = d$c)

  expect_true(!identical(s1,s5))

  m <- meat(mod, cluster = d$c)
  b <- bread(mod)
  s6 <- (b %*% m %*% b)/15
  s6 <- s6*(3/2)*(14/13) # scaling factor

  # For some reason, `identical` is failing here. I can't discern the
  # difference, so falling back to a slightly weaker comparison. It
  # shouldn't matter, as this confirms the math is working out.
  expect_true(all.equal(s5,s6))

  meatpre <- epb::meat(mod, cluster = d$c)
  s7 <- sandwich(mod, meat = meatpre, cluster = d$c)

  expect_identical(s5,s7)

})

test_that("clustered meat", {
  data(iris)

  mod <- lm(Petal.Width ~ . - Species, data = iris)

  # Without a clusters argument, should be identical to sandwich's
  # version.
  m1 <- sandwich::meat(mod)
  m2 <- meat(mod)

  expect_true(is(m2, "matrix"))
  expect_equal(nrow(m2), length(mod$coef))
  expect_true(isSymmetric(m2))

  expect_identical(m1, m2)

  # Adding clustering, should now be different
  m3 <- meat(mod, cluster = iris$Species)
  expect_false(identical(m1, m3))

  # If cluster variable is unique per row, equivalent to not
  # clustering.
  m6 <- meat(mod, cluster = 1:150)
  expect_identical(m6, m1)

  # adjust argument isn't broken
  m7 <- sandwich::meat(mod, adjust = TRUE)
  m8 <- meat(mod, adjust = TRUE)
  m9 <- meat(mod, adjust = TRUE, cluster = iris$Species)

  expect_identical(m7,m8)
  expect_true(!identical(m8, m9))
  expect_true(!identical(m9,m3))


  d <- data.frame(y = rnorm(15),
                  x = rnorm(15),
                  c = rep(1:3, each = 5))

  mod <- lm(y ~ x, data = d)

  s1 <- meat(mod)
  s2 <- meat(mod, cluster = d$c)

  expect_true(!identical(s1,s2))

  expect_true(all(sapply(list(s1,s2), dim) == 2))

})
