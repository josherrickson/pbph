##' Sandwich estimator allowing clustering
##'
##' This `sandwich` function is an overloaded function of `sandwich`
##' from the `sandwich` package which enables a `clusters` argument.
##' @param x a fitted model object.
##' @param bread. either a bread matrix or a function for computing
##'   this via `bread.(x)`.
##' @param meat. either a meat matrix or a function for computing
##'   this via `meat.(x)`.
##' @param clusters A list of variables to cluster on.
##' @param ... Additional arguments to `meat`.
##' @return A covariance matrix.
##' @export
##' @import sandwich
sandwich <- function (x, bread. = bread, meat. = meat, clusters = list(), ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  if (is.function(bread.))
    bread. <- bread.(x)
  if (is.function(meat.))
    meat. <- meat.(x, clusters = clusters, ...)
  n <- NROW(sandwich::estfun(x))
  return(1/n * (bread. %*% meat. %*% bread.))
}

##' Clustered meat matrix for a sandwich estimator
##'
##' This `meat` function is an overloaded function of `meat` from the
##' `sandwich` package which enables a `clusters` argument.
##' @param x a fitted model object.
##' @param adjust See `sandwich::meat`.
##' @param clusters A list of variables to cluster on.
##' @param ... Additional arguments to `sandwich::estfun`.
##' @return A meat matrix
##' @export
##' @import sandwich
meat <- function(x, adjust = FALSE, clusters=list(), ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  psi <- sandwich::estfun(x, ...)
  k <- NCOL(psi)
  n <- NROW(psi)

  # Drop any empty cluster variables
  clusters <- clusters[!sapply(clusters, is.null)]
  if (length(clusters) > 0) {
    psi <- aggregate(psi, by=clusters, FUN=sum)
    # aggregate adds columns identifying subgroups
    psi <- as.matrix(psi[, -c(1:length(clusters))])
  }

  rval <- crossprod(as.matrix(psi))/n
  if (adjust)
    rval <- n/(n - k) * rval
  rownames(rval) <- colnames(rval) <- colnames(psi)
  return(rval)
}
