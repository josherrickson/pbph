context("Utilites")

test_that("addIntercept", {
  d <- data.frame(x=1:4, y=1:4)
  m <- matrix(1:8, ncol=2)

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

  # Handling other objects
  expect_warning(v1 <- addIntercept(1:3), "intercept to integer")
  expect_warning(v2 <- addIntercept("abc"), "intercept to character")
  expect_identical(v1, 1:3)
  expect_identical(v2, "abc")
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
