context("modfit")

test_that("modfit", {
  resp <- c(3,4,1,3,2,1,4,2)
  covs <- data.frame(x1=c(2,1,3,4,2,1,3,1),
                     x2=c(3,5,2,1,3,2,3,4))
  treatment <- c(0,0,0,0,1,1,1,1)

  m <- modfit(resp, covs, treatment)
  expect_true(all.equal(m$mod1$coef, c(-6, 5/3, 5/3),
                        check.attributes=FALSE))
  expect_true(all.equal(m$pred, c(7/3, 4, 7/3, 7/3, 7/3, -1, 4, 7/3),
                        check.attributes=FALSE))
  expect_true(all.equal(m$mod2$coef, c(116/95, -44/95),
                        check.attributes=FALSE))
})
