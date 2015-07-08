##' Perform model fitting for the enchanced PB method.
##'
##' This  will fit a linear first stage model on the control group,
##' then use that model to predict $Y_c$ in the treatment group. Then a
##' linear second stage model will be fitted on the treatment group.
##'
##' The first stage model is
##'
##' resp ~ covs | treatment == 0
##'
##' Let the predicted values be "pred." The second stage model is
##'
##' resp - pred ~ pred | treatment == 1
##'
##' @param resp The response variable.
##' @param covs A data.frame of covariates. All will be included, so remove
##' variables that shouldn't be included.
##' @param treatment A vector of treatment statuses, should be all `0` or `1`.
##' @param center Default FALSE. Should the covariates be centered in the
##' second stage?
##' @return A list consisting of the two models and predicted values (across both
##' groups)
##' @author Josh Errickson
modfit <- function(resp, covs, treatment, center=FALSE) {
  # First stage model, linear
  mod1 <- lm(resp[treatment==0] ~ ., data=covs[treatment==0,,drop=FALSE])

  # center the covariates. Why do we do this?
  if (center) covs.center <- data.frame(scale(covs, scale=FALSE))

  # Get predicted values.
  pred <- predict(mod1, newdata=if(center) covs.center else covs)

  # Second stage linear model.
  mod2 <- lm(resp[treatment==1] - pred[treatment==1] ~ pred[treatment==1])

  return(list(mod1=mod1,
              mod2=mod2,
              pred=pred))
}

## Computing the bread & meat.
## These three don't depend on eta
bread.11 <- function(covs, treatment) {
  # If we don't have an intercept column in the covariates, add it.
  if (any(covs[,1] !=1)) {
    covs$Intercept <- rep(1,nrow(covs))
    covs <- covs[,c(ncol(covs), 1:(ncol(covs)-1))]
  }

  t(as.matrix(covs[treatment==0,,drop=FALSE])) %*%
    as.matrix(covs[treatment==0,,drop=FALSE])
}

bread.22 <- function(pred, treatment) {
  crossprod(cbind(rep(1,length(pred[treatment==1])), pred[treatment==1]))
}

meat.11 <- function(mod1, covs, treatment) {
  # If we don't have an intercept column in the covariates, add it.
  if (any(covs[,1] !=1)) {
    covs$Intercept <- rep(1,nrow(covs))
    covs <- covs[,c(ncol(covs), 1:(ncol(covs)-1))]
  }

  t(mod1$res*as.matrix(covs[treatment==0,,drop=FALSE])) %*%
  (mod1$res*as.matrix(covs[treatment==0,,drop=FALSE]))
}

## These two depend on eta (through eta and mod2)
bread.21 <- function(eta, mod2, resp, covs, pred, treatment) {
  # If we don't have an intercept column in the covariates, add it.
  if (any(covs[,1] !=1)) {
    covs$Intercept <- rep(1,nrow(covs))
    covs <- covs[,c(ncol(covs), 1:(ncol(covs)-1))]
  }

  rbind(apply(-(1 + eta) * covs[treatment==1,,drop=FALSE], 2, sum),
        apply((resp[treatment==1] - mod2$coef[1] -
                 2*(1 + eta) * pred[treatment==1]) * covs[treatment==1,,drop=FALSE],
              2, sum))
}

meat.22 <- function(mod2, pred, treatment) {
  crossprod(cbind(mod2$res, mod2$res*pred[treatment==1]))
}

## Compute B^-1*M*B^T
correctedvar <- function(b11, b21, b22, m11, m22) {
  (solve(b22)%*% (m22 + b21%*%solve(b11)%*%m11%*%
                   solve(b11)%*%t(b21))%*% solve(b22))[2,2]
}


pb_method <- function(resp, covs, treatment) {
  stopifnot(all(treatment %in% 0:1))

  mods <- modfit(resp, covs, treatment)

  bread11 <- bread.11(covs, treatment)
  bread22 <- bread.22(mods$pred, treatment)
  meat11 <- meat.11(mods$mod1, covs, treatment)

  tosolve <- function(eta) {
    mod2b <- lm(resp[treatment==1] - eta*mods$pred[treatment==1] ~ 1)

    meat22 <- meat.22(mod2b, mods$pred, treatment)
    bread21 <- bread.21(eta, mod2b, resp,
                        covs, mods$pred, treatment)

    corrected <- correctedvar(bread11, bread21, bread22, meat11, meat22)

    stat <- qt(.975, mods$mod1$df+2)
    return((mods$mod2$coef[2] - eta)^2 - stat^2*corrected)
  }

  midpoint <- optimize(tosolve, c(-1000, 1000))$minimum

  if (1000 - abs(midpoint) > 1e-3) {
    bounds <- c(uniroot(tosolve, c(-1000, midpoint))$root,
                uniroot(tosolve, c(midpoint, 1000))$root)
    return(c(midpoint, bounds))
  } else {
    return(c(midpoint, -Inf, Inf))
  }
}
