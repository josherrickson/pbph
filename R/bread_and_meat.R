#' (Internal) Piece-wise generation of Bread and Meat matrices.
#'
#' @param eta Estimated version of the coefficient on the interaction
#'   between predicted and treatment. This could be from a model or a
#'   hypothesis.
#' @param resp Vector of responses.
#' @param covs Data frame of covariates.
#' @param treatment Vector of 0/1 treatment indicators.
#' @param mod1 First stage model.
#' @param pred Predicted values from first stage.
#' @param tau Estimated value of constant coefficient in second stage
#'   model.
#'
#' @name bread_and_meat
NULL
#> NULL

##' @rdname bread_and_meat
bread11 <- function(model) {
  sandwich::bread(model)/length(residuals(model))
}

##' @rdname bread_and_meat
bread22 <- function(model) {
  sandwich::bread(model)/length(residuals(model))
}

##' @rdname bread_and_meat
meat11 <- function(model) {
  sandwich::meat(model)*length(residuals(model))
}

##' @rdname bread_and_meat
bread21 <- function(eta, tau, resp, covs, pred, treatment) {
  covsInt <- addIntercept(covs)

  rbind(apply(-(1 + eta) * covsInt[treatment==1,,drop=FALSE], 2, sum),
        apply((resp[treatment==1] - tau -
               2*(1 + eta) * pred[treatment==1]) *
                covsInt[treatment==1,,drop=FALSE],
              2, sum))
}

##' @rdname bread_and_meat
meat22 <- function(model) {
  sandwich::meat(model)*length(residuals(model))
}

##' (Internal) Compute B^-1*M*B^T
##'
##' @param b11,b21,b22,m11,m22 Pieces of bread and meat.
##' @return Variance estimate.
##' @author Josh Errickson
correctedvar <- function(b11, b21, b22, m11, m22) {
  covmat <- (b22 %*% (m22 + b21 %*% b11 %*% m11 %*% b11 %*% t(b21))%*% b22)
  dimnames(covmat) <- list(c("treatment", "pred"),
                           c("treatment", "pred"))
  return(covmat)
}
