#' (Internal) Piece-wise generation of Bread and Meat matrices.
#'
#' Computes the Bread and Meat matricies. The diagonal elements are typical
#' sandwich estimators, with scaling, and thus only use the \code{model}
#' argument. The off-diagonal Bread element requires further specification.
#' @param model For Bread & Meat that can be calculated using sandwich package,
#'   we only need the model, either first or second stage, depending.
#' @param cluster For Meat only; A vector defining clusters.
#' @param eta Estimated version of the coefficient on the interaction between
#'   predicted and treatment. This could be from a model or a hypothesis.
#' @param resp Vector of responses.
#' @param covs Data frame of covariates.
#' @param treatment Vector of 0/1 treatment indicators.
#' @param pred Predicted values from first stage.
#' @import sandwich
#' @name bread_and_meat
NULL
#> NULL

##' @rdname bread_and_meat
bread11 <- function(model) {
  sandwich::bread(model) / length(residuals(model))
}

##' @rdname bread_and_meat
bread22 <- function(model) {
  sandwich::bread(model) / length(residuals(model))
}

##' @rdname bread_and_meat
meat11 <- function(model, cluster = NULL) {
  meat(model, cluster = cluster) * length(residuals(model))
}

##' @rdname bread_and_meat
meat22 <- function(model, cluster = NULL) {
  meat(model, cluster = cluster) * length(residuals(model))
}

##' @rdname bread_and_meat
bread21 <- function(eta, resp, covs, pred, treatment) {
  covsInt <- addIntercept(covs)

  # Replace tauhat with tauhat_eta0
  tau <- lm(resp - (1 + eta) * pred ~ 1, subset = (treatment == 1))$coef

  rbind(apply(-(1 + eta) * covsInt[treatment == 1,,drop = FALSE], 2, sum),
        apply( (resp[treatment == 1] - tau - 2 * (1 + eta) * pred[treatment == 1]) *
                 covsInt[treatment == 1,,drop = FALSE],
              2, sum))
}

##' (Internal) Computes Bread and Meat matrices.
##'
##' Computes the pieces of the Bread and Meat which do not depend on eta. (e.g.
##' all but B21)
##' @param object A pblm object.
##' @param cluster A vector defining clustering.
##' @return A list of b11, b22, m11, and m22.
createBreadAndMeat <- function(object, cluster = NULL) {

  mod1 <- object$epb$mod1

  b11 <- bread11(mod1)
  b22 <- bread22(object)

  cluster.control <- cluster[object$epb$treatment == 0]
  cluster.treatment <- cluster[object$epb$treatment == 1]

  m11 <- meat11(mod1, cluster = cluster.control)
  m22 <- meat22(object, cluster = cluster.treatment)

  return(list(b11 = b11,
              b22 = b22,
              m11 = m11,
              m22 = m22))
}
