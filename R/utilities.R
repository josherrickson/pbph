##' Internal function to add a column for the intercept to a matrix.
##'
##' Given a matrix or data.frame of size nxp, returns an nx(p+1) object where
##' the first column is an intercept; if the intercept is not already in the
##' object (defined as a first column being all 1's).
##'
##' @param x A matrix or data.frame.
##' @return If `x` has no intercept, a matrix with an intercept column
addIntercept <- function(x) {
  if (class(x) == "data.frame") {
    if (any(x[,1] != 1)) {
      x$Intercept <- rep(1,nrow(x))
      x <- x[,c(ncol(x), 1:(ncol(x) - 1))]
    }
    return(x)
  }

  if (class(x) == "matrix") {
    if (any(x[,1] != 1)) {
      x <- cbind(rep(1, nrow(x)), x)
      if(!is.null(colnames(x))) {
        colnames(x)[1] <- "Intercept"
      }
    }
    return(x)
  }
  warning(paste0("Don't know how to add intercept to ",
                 class(x), "."))
  return(x)
}

##' Generate an empty matrix suitable for saving simulation results.
##'
##' Given a vector of column names and a number of reps, generates an
##' appropriately sized matrix with given column names.
##'
##' @param colNames Vector of column names.
##' @param reps Total reps.
##' @return An empty matrix.
makeSaveMatrix <- function(colNames, reps) {
  stopifnot(is.numeric(reps))

  colNames <- as.character(colNames)

  save <- matrix(nrow=reps, ncol=length(colNames))
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
