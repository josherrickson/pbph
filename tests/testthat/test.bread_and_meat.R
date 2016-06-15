context("bread_and_meat")

test_that("bread", {
  d <- data.frame(y = c(3,4,1,3,2,1,4,2),
                  x1 = c(2,1,3,4,2,1,3,1),
                  x2 = c(3,5,2,1,3,2,3,4))
  form <- formula(y ~ x1 + x2)
  treatment <- c(0,0,0,0,1,1,1,1)

  resp <- eval(form[[2]], envir = d)
  covs <- model.matrix(form, data = d)
  eta0 <- 1

  mod1 <- lm(form, data = d, subset = treatment == 0)

  m <- pbph(mod1, treatment, d, center = FALSE)

  # B11
  b11 <- bread11(mod1)
  expect_true(all(rownames(b11) == c("(Intercept)", "x1", "x2")))
  expect_equal(solve(b11), matrix(c(4,10,11,10,30,21,11,21,39),
                                  nrow = 3),
               check.attributes = FALSE)

  #B21
  b21 <- bread21(m, eta = 1)
  expect_equal(b21, matrix(c(-8,-46/3, -14, -503/12,
                             -24, -175/3 ), nrow = 2),
               check.attributes = FALSE)

  #B22
  b22 <- bread22(m)
  expect_equal(solve(b22), matrix(c(4,23/3,23/3,251/9), nrow = 2),
               check.attributes = FALSE)

  # If we center in the second stage, intercept and pred should be
  # independent
  m2 <- pbph(mod1, treatment, d, center = TRUE)
  b22.2 <- bread22(m2)
  expect_equal(solve(b22.2), matrix(c(4, 0, 0, 475/36), nrow = 2),
               check.attributes = FALSE)

})

test_that("meat", {
  d <- data.frame(y = c(3,4,1,3,2,1,4,2),
                  x1 = c(2,1,3,4,2,1,3,1),
                  x2 = c(3,5,2,1,3,2,3,4))
  form <- formula(y ~ x1 + x2)
  treatment <- c(0,0,0,0,1,1,1,1)

  mod1 <- lm(form, data = d, subset = treatment == 0)

  m <- pbph(mod1, treatment, d)

  resp <- eval(form[[2]], envir = d)
  covs <- model.matrix(form, data = d)

  # M11
  m11 <- meat11(mod1)
  expect_equal(m11, matrix(c(8/3, 8, 16/3,
                             8, 224/9, 136/9,
                             16/3, 136/9, 104/9), nrow = 3),
               check.attributes = FALSE)

  # M22
  # check this
  m22 <- meat22(m)
  #expect_equal(m22, matrix(c(60.652767005232462338,
  #                           176.40659484969751247,
  #                           176.40659484969751247,
  #                           603.89350432611854558),
  #                         nrow = 2),
  #             check.attributes = FALSE)
})

test_that("corrected var", {
  b11 <- matrix(c(1,2,2,3), nrow = 2)
  b21 <- matrix(c(4,2,2,3), nrow = 2)
  b22 <- matrix(c(6,-1,-1,2), nrow = 2)
  m11 <- matrix(c(5,2,2,2), nrow = 2)
  m22 <- matrix(c(-3,-1,-1,1), nrow = 2)

  cv <- correctedvar(b11, b21, b22, m11, m22)

  expect_true(all(dim(cv) == 2))
  expect_true(all(sapply(dimnames(cv), "==", c("treatment", "pred"))))

  # off-diag's should be equal, on-diag should be positive.
  expect_equal(cv[1,2], cv[2,1])
  expect_true(all(diag(cv) > 0))
})
