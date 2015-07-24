context("makemod2")

test_that("makemod2", {
  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)
  mod1 <- lm(form, data=d, subset=treatment==0)

  m <- makemod2(mod1, treatment, d)

  expect_true(all.equal(m$pred, c(7/3, 4, 7/3, 7/3, 7/3, -1, 4, 7/3),
                        check.attributes=FALSE))
  expect_true(all.equal(m$mod2$coef, c(116/95, -44/95),
                        check.attributes=FALSE))
})
