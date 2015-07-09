#' Internal piece-wise generation of Bread and Meat matrices.
#'
#' @param eta Estimated version of the coefficient on the interaction
#' between predicted and treatment. This could be from a model or a
#' hypothesis.
#' @param resp Vector of responses.
#' @param covs Data frame of covariates.
#' @param treatment Vector of 0/1 treatment indicators.
#' @param mod1 First stage model.
#' @param pred Predicted values from first stage.
#' @param tau Estimated value of constant coefficient in second stage model.
#'
#' @name bread_and_meat
NULL
#> NULL

##' @rdname bread_and_meat
bread.11 <- function(covs, treatment) {

  covsInt <- addIntercept(covs)

  t(as.matrix(covsInt[treatment==0,,drop=FALSE])) %*%
    as.matrix(covsInt[treatment==0,,drop=FALSE])
}

##' @rdname bread_and_meat
bread.22 <- function(pred, treatment) {
  crossprod(cbind(rep(1,length(pred[treatment==1])), pred[treatment==1]))
}

##' @rdname bread_and_meat
meat.11 <- function(mod1, covs, treatment) {
  covsInt <- addIntercept(covs)

  t(mod1$res*as.matrix(covsInt[treatment==0,,drop=FALSE])) %*%
  (mod1$res*as.matrix(covsInt[treatment==0,,drop=FALSE]))
}

##' @rdname bread_and_meat
bread.21 <- function(eta, tau, resp, covs, pred, treatment) {
  covsInt <- addIntercept(covs)

  rbind(apply(-(1 + eta) * covsInt[treatment==1,,drop=FALSE], 2, sum),
        apply((resp[treatment==1] - tau -
               2*(1 + eta) * pred[treatment==1]) * covsInt[treatment==1,,drop=FALSE],
              2, sum))
}

##' @rdname bread_and_meat
meat.22 <- function(eta, tau, resp, pred, treatment) {
  mod2res <- (resp[treatment==1] - (1 + eta)*pred[treatment==1] - tau)
  crossprod(cbind(mod2res, mod2res*pred[treatment==1]))
}

## Compute B^-1*M*B^T
correctedvar <- function(b11, b21, b22, m11, m22) {
  (solve(b22)%*% (m22 + b21%*%solve(b11)%*%m11%*%
                   solve(b11)%*%t(b21))%*% solve(b22))[2,2]
}
