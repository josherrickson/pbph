context("makemod2")

test_that("makemod2", {
  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)
  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  expect_true(is.list(m))

  expect_true(length(m) == 2)
  expect_true(all(names(m) == c("mod2", "pred")))

  expect_true(is(m$mod2, "lm"))
  expect_true(length(m$pred) == 8)

  expect_equal(m$pred, c(7/3, 4, 7/3, 7/3, 7/3, -1, 4, 7/3),
               check.attributes=FALSE)
  expect_equal(m$mod2$coef, c(116/95, -44/95),
               check.attributes=FALSE)


  # Centering should not effect eta, but will affect tau.
  m2 <- makemod2(mod1, treatment, d, center=TRUE)

  expect_equal(m$mod2$coef[2],m2$mod2$coef[2])
  expect_false(isTRUE(all.equal(m$mod2$coef[1], m2$mod2$coef[1])))

})

test_that("mod2 call should be informative", {
  d <- data.frame(abc=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- abc ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)
  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  expect_true(m$mod2$call$formula[[2]][[2]] == "abc_t")
  expect_true(all(!isTRUE(grepl("0", as.character(m$mod2$call$formula[[3]])))))
  expect_true(all(!isTRUE(grepl("y", colnames(model.frame(m$mod2))))))
})
