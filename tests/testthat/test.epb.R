context("epb")

test_that("epb", {
  # Need to expand these a LOT

  resp <- rnorm(10)
  covs <- data.frame(x1=rnorm(10), x2=rnorm(10))
  treatment <- rep(0:1, each=5)

  epb(resp, covs, treatment)
})
