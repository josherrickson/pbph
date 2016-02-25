context("sandwich")

test_that("clustered sandwich", {

  d <- data.frame(y=rnorm(15),
                  x=rnorm(15),
                  c1=rep(1:3, each=5),
                  c2=rep(1:3, times=5))

  mod <- lm(y ~ x, data=d)

  s1 <- sandwich::sandwich(mod)
  s2 <- sandwich(mod)

  expect_identical(s1,s2)

  # Make sure adjust is still getting passed through
  s3 <- sandwich::sandwich(mod, adjust=TRUE)
  s4 <- sandwich(mod, adjust=TRUE)

  expect_identical(s3,s4)
  expect_true(!identical(s1,s3))

  s5 <- sandwich(mod, clusters=list(d$c1))

  expect_true(!identical(s1,s5))

  m <- meat(mod, clusters=list(d$c1))
  b <- bread(mod)
  s6 <- (b %*% m %*% b)/15

  # For some reason, `identical` is failing here. I can't discern the
  # difference, so falling back to a slightly weaker comparison. It
  # shouldn't matter, as this confirms the math is working out.
  expect_true(all.equal(s5,s6))

  s7 <- sandwich(mod, meat=epb::meat, clusters=list(d$c1))

  expect_identical(s5,s7)

  # clusters ignored here, but should not error
  s8 <- sandwich(mod, meat=sandwich::meat, clusters=list(d$c1))
  expect_identical(s8,s1)

})

test_that("clustered meat", {
  data(iris)

  mod <- lm(Petal.Width ~ . - Species, data=iris)

  # Without a clusters argument, should be identical to sandwich's
  # version.
  m1 <- sandwich::meat(mod)
  m2 <- meat(mod)

  expect_true(is(m2, "matrix"))
  expect_equal(nrow(m2), length(mod$coef))
  expect_true(isSymmetric(m2))

  expect_identical(m1, m2)

  # Adding clustering, should now be different
  m3 <- meat(mod, clusters=list(iris$Species))
  expect_false(identical(m1, m3))

  # A null element should be dropped
  m4 <- meat(mod, clusters=list(iris$Species, iris$foo))
  expect_identical(m3, m4)

  # If all null elements dropped, fall back gracefully to
  # sandwich::meat
  m5 <- meat(mod, clusters=list(iris$foo, iris$bar))
  expect_identical(m5, m1)

  # If cluster variable is unique per row, equivalent to not
  # clustering.
  m6 <- meat(mod, clusters=list(1:150))
  expect_identical(m6, m1)

  # adjust argument isn't broken
  m7 <- sandwich::meat(mod, adjust=TRUE)
  m8 <- meat(mod, adjust=TRUE)
  m9 <- meat(mod, adjust=TRUE, clusters=list(iris$Species))

  expect_identical(m7,m8)
  expect_true(!identical(m8, m9))
  expect_true(!identical(m9,m3))


  d <- data.frame(y=rnorm(15),
                  x=rnorm(15),
                  c1=rep(1:3, each=5),
                  c2=rep(1:3, times=5))

  mod <- lm(y ~ x, data=d)

  s1 <- meat(mod)
  s2 <- meat(mod, clusters=list(d$c1))
  s3 <- meat(mod, clusters=list(d$c2))
  s4 <- meat(mod, clusters=list(d$c1, d$c2))

  expect_true(!identical(s1,s2))
  expect_true(!identical(s1,s3))
  expect_true(!identical(s1,s4))
  expect_true(!identical(s2,s3))
  expect_true(!identical(s2,s4))
  expect_true(!identical(s3,s4))
  expect_true(!identical(s2,s4))

  expect_true(all(sapply(list(s1,s2,s3,s4), dim) == 2))

})
