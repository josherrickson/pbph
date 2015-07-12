##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data. First stage is fit on
##' `resp ~ covs`. *TODO* REPLACE WITH A FORMULA.
##'
##' @param form First stage model formula.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in `form` live.
##'
##' @return Vector consisting of an estimate of eta and confidence
##'   bounds.
##' @export
##' @author Josh Errickson
##'
epb <- function(form, treatment, data) {
  stopifnot(length(form) == 3)
  stopifnot(class(form) == "formula")
  stopifnot(all(treatment %in% 0:1))

  mods <- modfit(form, treatment, data)

  resp <- eval(form[[2]], envir=data)
  covs <- model.matrix(form, data=data)

  b11 <- bread11(covs, treatment)
  b22 <- bread22(mods$pred, treatment)
  m11 <- meat11(mods$mod1, covs, treatment)
  m22 <- meat22(mods$mod2$coef[2], mods$mod2$coef[1], resp, mods$pred,
                treatment)

  tosolve <- function(eta) {
    mod2b <- lm(resp[treatment==1] - eta*mods$pred[treatment==1] ~ 1)

    b21 <- bread21(eta, mod2b$coef[1], resp, covs, mods$pred,
                   treatment)

    corrected <- correctedvar(b11, b21, b22, m11, m22)

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
