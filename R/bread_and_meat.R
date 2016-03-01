#' (Internal) Piece-wise generation of Bread and Meat matrices.
#'
#' Computes the Bread and Meat matricies. The diagonal elements are
#' typical sandwich estimators, with scaling, and thus only use the
#' \code{model} argument. The off-diagonal Bread element requires
#' further specification.
#' @param model For Bread & Meat that can be calculated using sandwich
#'   package, we only need the model, either first or second stage,
#'   depending.
#' @param clusters For Meat only; list of cluster variables.
#' @param eta Estimated version of the coefficient on the interaction
#'   between predicted and treatment. This could be from a model or a
#'   hypothesis.
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
meat11 <- function(model, clusters=list()) {
  meat(model, clusters=clusters) * length(residuals(model))
}

##' @rdname bread_and_meat
meat22 <- function(model, clusters=list()) {
  meat(model, clusters=clusters) * length(residuals(model))
}

##' @rdname bread_and_meat
bread21 <- function(eta, resp, covs, pred, treatment) {
  covsInt <- addIntercept(covs)

  # Replace tauhat with tauhat_eta0
  tau <- lm(resp - (1 + eta) * pred ~ 1, subset=(treatment == 1))$coef

  rbind(apply(- (1 + eta) * covsInt[treatment == 1,,drop=FALSE], 2, sum),
        apply( (resp[treatment == 1] - tau -
                  2 * (1 + eta) * pred[treatment == 1]) *
                 covsInt[treatment == 1,,drop=FALSE],
              2, sum))
}

##' (Internal) Compute B^-1*M*B^T
##'
##' @param b11,b21,b22,m11,m22 Pieces of bread and meat.
##' @return Variance estimate.
##' @author Josh Errickson
correctedvar <- function(b11, b21, b22, m11, m22) {
  covmat <- (b22 %*% (m22 + b21 %*% b11 %*% m11 %*% b11 %*% t(b21)) %*% b22)
  dimnames(covmat) <- list(c("treatment", "pred"),
                           c("treatment", "pred"))
  return(covmat)
}
