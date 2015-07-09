##' Internal function to add a column for the intercept to a matrix.
##'
##' Given a matrix or data.frame of size nxp, returns an nx(p+1) object where
##' the first column is an intercept; if the intercept is not already in the
##' object (defined as a first column being all 1's).
##' @param x A matrix or data.frame.
##' @return If `x` has no intercept, a matrix with an interecpt column
##' @author Josh Errickson
addIntercept <- function(x) {
  if (class(x) == "data.frame") {
    if (any(x[,1] !=1)) {
      x$Intercept <- rep(1,nrow(x))
      x <- x[,c(ncol(x), 1:(ncol(x)-1))]
    }
    return(x)
  }

  if (class(x) == "matrix") {
    if (any(x[,1] !=1)) {
      x <- cbind(rep(1, nrow(x)), x)
      if(!is.null(colnames(x))) {
        colnames(x)[1] <- "Intercept"
      }
    }
    return(x)
  }
  warning(paste0("Don't know how to add intercept to ", class(x), "."))
  return(x)
}
