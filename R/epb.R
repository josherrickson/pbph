##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data. First stage is fit on
##' `resp ~ covs`. *TODO* REPLACE WITH A FORMULA.
##' @param resp Vector of responses.
##' @param covs Data.frame of covariates.
##' @param treatment Vector of 0/1 treatment indicators.
##' @return Vector consisting of an estimate of eta and confidence bounds.
##' @author Josh Errickson
epb <- function(resp, covs, treatment) {
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
