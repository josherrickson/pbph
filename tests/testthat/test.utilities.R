context("Utilites")

test_that("addIntercept", {
  d <- data.frame(x = 1:4, y = 1:4)
  m <- matrix(1:8, ncol = 2)

  dout <- addIntercept(d)
  expect_true(class(dout) == class(d))
  expect_true(nrow(dout) == nrow(d))
  expect_true(ncol(dout) == ncol(d) + 1)
  expect_true(all(dout[,-1] == d))
  expect_true(all(dout[,1] == 1))
  expect_true(all(colnames(dout) == c("Intercept", colnames(d))))

  mout <- addIntercept(m)
  expect_true(class(mout) == class(m))
  expect_true(nrow(mout) == nrow(m))
  expect_true(ncol(mout) == ncol(m) + 1)
  expect_true(all(mout[,-1] == m))
  expect_true(all(mout[,1] == 1))
  expect_true(is.null(colnames(mout)))

  # add some column names to m
  colnames(m) <- c("x", "y")
  m2out <- addIntercept(m)
  expect_true(all(m2out == mout))
  expect_true(all(colnames(m2out) == c("Intercept", colnames(m))))

  # Vectors
  v1 <- addIntercept(1)
  expect_identical(v1, matrix(c(1,1), ncol=2, dimnames=list(NULL, c("Intercept", "x"))))

  v2 <- addIntercept(1:10)
  expect_identical(v2, matrix(c(rep(1,10),1:10), byrow=FALSE, ncol=2, dimnames=list(NULL, c("Intercept", "x"))))

  # Unknown class
  s <- 1
  expect_silent(s1 <- addIntercept(s))
  class(s) <- "foo"
  expect_warning(s2 <- addIntercept(s), "intercept to foo")
  expect_identical(s, s2)
})


test_that("makeSaveMatrix", {
  names <- letters[1:5]
  reps <- 10

  d <- makeSaveMatrix(names, reps)

  expect_true(all.equal(dim(d), c(reps, length(names))))
  expect_true(all.equal(colnames(d), names))
  expect_true(all(is.na(d)))

  d <- makeSaveMatrix(1:5, reps)
  expect_true(all.equal(dim(d), c(reps, length(names))))
  expect_true(all.equal(colnames(d), as.character(1:5)))
  expect_true(all(is.na(d)))

  expect_error(makeSaveMatrix(names, TRUE))
  expect_error(makeSaveMatrix(names, "20"))

})

test_that("quadratic formula", {
  q1 <- quad(1,1,1)
  expect_true(length(q1) == 2)
  expect_true(is.vector(q1))
  expect_true(is.vector(q1))

  q2 <- quad(1,0,-1)
  expect_true(all(q2 == c(-1,1)))

  # examples from http://regentsprep.org/Regents/math/algtrig/ATE3/quadformula.htm
  expect_true(all.equal(quad(1,2,-8), c(-4,2)))
  expect_true(all.equal(quad(3,-10,5),
                        c((5 - sqrt(10))/3,(5 + sqrt(10))/3)))
  expect_true(all.equal(quad(1,4,5),
                        c(NA, NA)))
  expect_true(all.equal(quad(1,-4,4), c(2,2)))
  expect_true(all.equal(quad(2,1,-.5),
                        c((-1 - sqrt(5))/4,(-1 + sqrt(5))/4)))
})
