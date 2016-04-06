##' Internal function to add a column for the intercept to a matrix.
##'
##' Given a \code{matrix} or \code{data.frame} of size nxp, returns an
##' nx(p+1) object of the same type where the first column is an
##' intercept if the intercept is not already in the object (defined
##' as a first column being all 1's).
##'
##' @param x A \code{matrix} or \code{data.frame}.
##' @return A \code{matrix} (if \code{x} is a \code{matrix}) or
##'   \code{data.frame} (if \code{x} is a \code{data.frame})
##'   guaranteed to have an intercept column.
addIntercept <- function(x) {
  if (class(x) == "data.frame") {
    if (any(x[,1] != 1)) {
      x$Intercept <- 1
      x <- x[,c(ncol(x), 1:(ncol(x) - 1))]
    }
    return(x)
  }

  if (class(x) == "matrix") {
    if (any(x[,1] != 1)) {
      x <- cbind(1, x)
      if (!is.null(colnames(x))) {
        colnames(x)[1] <- "Intercept"
      }
    }
    return(x)
  }

  if (is(x, "vector")) {
    x <- cbind(1, x)
    colnames(x)[1] <- "Intercept"
    return(x)
  }

  warning(paste0("Don't know how to add intercept to ",
                 class(x), "."))
  return(x)
}

##' Generate an empty matrix suitable for saving simulation results.
##'
##' Given a \code{vector} of column names and a number of \code{reps}, generates an
##' appropriately sized \code{matrix} with given column names.
##'
##' @param colNames A \code{vector} of column names.
##' @param reps Total reps.
##' @return An empty \code{matrix}.
makeSaveMatrix <- function(colNames, reps) {
  stopifnot(is.numeric(reps))

  colNames <- as.character(colNames)

  save <- matrix(nrow = reps, ncol = length(colNames))
  colnames(save) <- colNames
  return(save)
}

##' Solve quadratic formula
##'
##' Given an expression a*x^2 + b*x + c = 0, solve for x.
##' @param a Coefficient on second-order term.
##' @param b Coefficient on first-order term.
##' @param c Coefficient on the constant.
##' @return Zeroes of the function a*x^2 + b*x + c = 0.
quad <- function(a,b,c) {
  # Short circuit if we get into imaginary number land.
  if (b^2 < 4 * a * c) {
    return(c(NA, NA))
  }
  return(sort(c((-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
                (-b + sqrt(b^2 - 4 * a * c)) / (2 * a))))
}
