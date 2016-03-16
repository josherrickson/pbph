##' Sandwich estimator allowing clustering
##'
##' This `sandwich` function is an overloaded function of `sandwich` from the
##' `sandwich` package which enables a `clusters` argument.
##' @param x a fitted model object.
##' @param bread. either a bread matrix or a function for computing this via
##'   `bread.(x)`.
##' @param meat. either a meat matrix or a function for computing this via
##'   `meat.(x)`.
##' @param cluster A variable identifying cluster.
##' @param ... Additional arguments to `meat`.
##' @return A covariance matrix.
##' @export
##' @import sandwich
sandwich <- function(x, bread. = bread, meat. = meat, cluster = NULL, ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  if (is.function(bread.))
    bread. <- bread.(x)
  if (is.function(meat.))
    meat. <- meat.(x, cluster = cluster, ...)
  n <- NROW(sandwich::estfun(x))
  return(1/n * (bread. %*% meat. %*% bread.))
}

##' Clustered meat matrix for a sandwich estimator
##'
##' This `meat` function is an overloaded function of `meat` from the `sandwich`
##' package which enables a `clusters` argument.
##' @param x a fitted model object.
##' @param adjust See `sandwich::meat`.
##' @param cluster A vector identifying cluster membership
##' @param ... Additional arguments to `sandwich::estfun`.
##' @return A meat matrix
##' @export
##' @import sandwich
meat <- function(x, adjust = FALSE, cluster = NULL, ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  psi <- sandwich::estfun(x, ...)

  k <- NCOL(psi)
  n <- NROW(psi)

  if (!is.null(cluster)) {
    psi <- aggregate(psi, by = list(cluster), FUN = sum)[,-1]
    # Finite population corrected is needed
    C <- length(unique(cluster))
    N <- length(cluster)
    P <- x$rank
    fpc <- (C / (C - 1)) * ((N - 1) / (N - P))
  } else {
    fpc <- 1
  }

  rval <- fpc * crossprod(as.matrix(psi))/n
  if (adjust)
    rval <- n/(n - k) * rval
  rownames(rval) <- colnames(rval) <- colnames(psi)
  return(rval)
}
