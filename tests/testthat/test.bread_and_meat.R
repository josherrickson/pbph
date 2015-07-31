context("bread_and_meat")

test_that("bread", {
  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)

  resp <- eval(form[[2]], envir=d)
  covs <- model.matrix(form, data=d)
  eta0 <- 1

  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  # B11
  b11 <- bread11(covs, treatment)
  expect_true(all(rownames(b11) == c("(Intercept)", "x1", "x2")))
  expect_true(all(b11 == matrix(c(4,10,11,10,30,21,11,21,39),
                                nrow=3)))

  #B21
  b21 <- bread21(eta=1, m$mod2$coef[1], resp, covs, m$pred, treatment)
  expect_true(all.equal(b21, matrix(c(-8,-7567/285, -14, -5847/95,
                                      -24, -26216/285 ), nrow=2),
                        check.attributes=FALSE))

  #B22
  b22 <- bread22(m$pred, treatment)
  expect_true(all.equal(b22, matrix(c(4,23/3,23/3,251/9), nrow=2),
                        check.attributes=FALSE))
})

test_that("meat", {
  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)

  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  resp <- eval(form[[2]], envir=d)
  covs <- model.matrix(form, data=d)

  # M11
  m11 <- meat11(mod1, covs, treatment)
  expect_true(all.equal(m11, matrix(c(8/3, 8, 16/3,
                                      8, 224/9, 136/9,
                                      16/3, 136/9, 104/9), nrow=3),
                        check.attributes=FALSE))

  # M22
  # check this
  m22 <- meat22(1, m$mod2$coef[1], resp, m$pred, treatment)
  expect_true(all.equal(m22, matrix(c(60.652767005232462338,
                                      176.40659484969751247,
                                      176.40659484969751247,
                                      603.89350432611854558),
                                    nrow=2),
                        check.attributes=FALSE))
})

test_that("corrected var", {
  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)

  resp <- eval(form[[2]], envir=d)
  covs <- model.matrix(form, data=d)

  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  b11 <- bread11(covs, treatment)
  b21 <- bread21(eta=1, m$mod2$coef[1], resp, covs, m$pred, treatment)
  b22 <- bread22(m$pred, treatment)
  m11 <- meat11(mod1, covs, treatment)
  m22 <- meat22(1, m$mod2$coef[1], resp, m$pred, treatment)

  # Double check this!
  expect_true(all.equal(correctedvar(b11,b21,b22,m11,m22)[2,2],
                        4.3974828563929113656))
})
