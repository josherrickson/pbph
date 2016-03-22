##' Sandwich estimator allowing clustering
##'
##' This \code{sandwich} function is an overloaded function of
##' \code{sandwich} from the \code{sandwich} package which enables a
##' \code{clusters} argument.
##' @param x a fitted model object.
##' @param bread. either a bread matrix or a function for computing
##'   this via \code{bread.(x)}.
##' @param meat. either a meat matrix or a function for computing this
##'   via \code{meat.(x)}.
##' @param cluster A variable identifying cluster.
##' @param ... Additional arguments to \code{meat}.
##' @return A covariance \code{matrix}.
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
##' This \code{meat} function is an overloaded function of \code{meat}
##' from the \code{sandwich} package which enables a \code{clusters}
##' argument.
##' @param x a fitted model object.
##' @param adjust See \code{sandwich::meat}. If a cluster is given,
##'   this argument is ignored as the adjustment is always used.
##' @param cluster A vector identifying cluster membership
##' @param ... Additional arguments to \code{sandwich::estfun}.
##' @return A meat \code{matrix}.
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
    adjust <- TRUE
  }

  rval <- crossprod(as.matrix(psi))/n
  if (adjust)
    rval <- n/(n - k) * rval
  rownames(rval) <- colnames(rval) <- colnames(psi)
  return(rval)
}
