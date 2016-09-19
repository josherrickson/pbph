##' (Internal) Return covariance matrix associated with a given choice of eta.
##'
##' For a given eta, the covariance can vary. For example, etahat or
##' eta_0.
##' @param eta Value of eta to use.
##' @param object A \code{pbph} object.
##' @param breadAndMeat By default, will create \code{breadAndMeat}
##'   associated with \code{object}. If speed a concern, it is faster
##'   to compute this once and pass into corrVar.
##' @return A covariance \code{matrix}.
corrVar <- function(eta, object,
                    breadAndMeat = createBreadAndMeat(object, cluster = object$pbph$cluster),
                    taubound = Inf) {

  b21 <- bread21(object, eta, taubound)

  corrected <- correctedvar(breadAndMeat$b11,
                                         b21,
                            breadAndMeat$b22,
                            breadAndMeat$m11,
                            breadAndMeat$m22)

  return(corrected)
}

##' (Internal) Conducts a hypothesis test for a given null.
##'
##' @param object A \code{pbph} object.
##' @param null Defaults to 0.
##' @return A test statistic, with distribution t(k) where k is the
##'   number of parameters in the first stage model, less 2.
hypothesisTest <- function(object, null = 0) {
  return( (object$coef[2] - null) / sqrt(corrVar(eta = null, object)[2,2]))
}


##' (Internal) Computes a confidence interval for eta via test inversion.
##'
##' @param object A \code{pbph} object.
##' @param level Confidence level. Default is 95\%.
##' @return A \code{vector} of length two with the lower and upper
##'   bounds.
testinverse <- function(object, level = .95) {

  bAndM <- createBreadAndMeat(object, object$pbph$cluster)

  tosolve <- function(eta) {
    corrected <- corrVar(eta, object, bAndM)[2,2]
    mod1 <- object$pbph$mod1
    df <- ifelse(is(mod1, "glm"), mod1$df.null, mod1$df)
    stat <- qt( (1 - level) / 2, df)
    return( (object$coef[2] - eta)^2 - stat^2 * corrected)
  }

  # Faster than lm
  t <- sapply(-1:1,tosolve)
  dmat <- cbind(1, (-1:1), (-1:1)^2)
  coefs <- solve(t(dmat) %*% dmat) %*% t(dmat) %*% t

  midpoint <- coefs[1] - coefs[2]^2/(4*coefs[3])

  # If a is positive, convex and therefore finite.
  #   (Can show that all reject is impossible.)
  # If a is negative AND midpoint is positive, concave and disjoint.
  # if a is negative AND midpoint is negative, concave and infinite.
  type <- ifelse(sign(coefs[3]) == 1, "finite",
                 ifelse(midpoint > 0, "disjoint", "infinite"))

  if (type != "infinite") {
    bounds <- quad(coefs[3], coefs[2], coefs[1])
  } else {
    bounds <- c(-Inf, Inf)
  }

  attr(bounds, "type") <- as.vector(type)

  return(bounds)
}

##' (Internal) Compute B^-1*M*B^T
##'
##' @param b11,b21,b22,m11,m22 Pieces of bread and meat.
##' @return Variance estimate.
correctedvar <- function(b11, b21, b22, m11, m22) {
  covmat <- (b22 %*% (m22 + b21 %*% b11 %*% m11 %*% b11 %*% t(b21)) %*% b22)
  dimnames(covmat) <- list(c("treatment", "pred"),
                           c("treatment", "pred"))
  return(covmat)
}
