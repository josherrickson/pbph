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

  m <- modfit(form, treatment, d)

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

  resp <- eval(form[[2]], envir=d)
  covs <- model.matrix(form, data=d)
  eta0 <- 1

  m <- modfit(form, treatment, d)
  mod2b <- lm(resp[treatment==1] - (1 + eta0)*m$pred[treatment==1] ~ 1)

  # M11
  m11 <- meat11(m$mod1, covs, treatment)
  expect_true(all.equal(m11, matrix(c(8/3, 8, 16/3,
                                      8, 224/9, 136/9,
                                      16/3, 136/9, 104/9), nrow=3),
                        check.attributes=FALSE))

  # M22
  m22 <- meat22(eta0, mod2b$coef[1], resp, m$pred, treatment)
  expect_true(all.equal(m22, matrix(c(1051/36, 3383/432,
                                      3383/432, 164891/1296),
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
  eta0 <- 1

  m <- modfit(form, treatment, d)
  mod2b <- lm(resp[treatment==1] - (1 + eta0)*m$pred[treatment==1] ~ 1)

  b11 <- bread11(covs, treatment)
  b21 <- bread21(eta=1, mod2b$coef[1], resp, covs, m$pred, treatment)
  b22 <- bread22(m$pred, treatment)
  m11 <- meat11(m$mod1, covs, treatment)
  m22 <- meat22(eta0, mod2b$coef[1], resp, m$pred, treatment)

  expect_equal(correctedvar(b11,b21,b22,m11,m22), 149941479/17247583)
})
