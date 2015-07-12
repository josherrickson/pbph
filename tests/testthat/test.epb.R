context("epb")

test_that("epb", {
  # Need to expand these a LOT

  d <- data.frame(y=c(3,4,1,3,2,1,4,2),
                  x1=c(2,1,3,4,2,1,3,1),
                  x2=c(3,5,2,1,3,2,3,4))
  form <- y ~ x1 + x2
  treatment <- c(0,0,0,0,1,1,1,1)

  e <- epb(form, treatment, d)

  expect_equal(length(e), 3)
  expect_true(is.numeric(e))
  # Regardless of input, bounds should capture estimate
  expect_true(e[1] > e[2])
  expect_true(e[1] < e[3])
})
