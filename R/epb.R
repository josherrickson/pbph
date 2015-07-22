##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data.
##'
##' @param mod1 First stage fitted model.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in `form` live.
##'
##' @return Summary of second stage model.
##' @export
##' @author Josh Errickson
##'
epb <- function(mod1, treatment, data) {
  if (!is.logical(treatment)) {
    warning("treatment not logical, attempting to convert")
    isTreated <- as.logical(treatment)
    if(any(is.na(isTreated))) {
      stop("treatment cannot be converted to logical")
    }
    isTreated <- as.numeric(isTreated)
  }

  mod2 <- makemod2(mod1, isTreated, data)

  pred <- mod2$pred
  mod2 <- mod2$mod2

  est <- epbsolve(mod1, mod2, pred, isTreated, data)

  smod2 <- summary(mod2)

  smod2$coefficients <- cbind(smod2$coef,
                              matrix(c(NA, NA, est$bounds),
                                     byrow=TRUE, nrow=2))

  smod2$coefficients[2,1] <- est$estimate
  colnames(smod2$coefficients)[5:6] <- c("LB", "UB")

  return(smod2)
}

##' (Internal) Computes corrected estimate and bounds.
##'
##' @param mod1 First stage model.
##' @param mod2 Second stage model.
##' @param pred Predicted values.
##' @param isTreated Treatment.
##' @param data Data.
##' @return Estimate and bounds.
##' @author Josh Errickson
epbsolve <- function(mod1, mod2, pred, isTreated, data) {

  resp <- eval(formula(mod1)[[2]], envir=data)
  covs <- model.matrix(formula(mod1), data=data)

  b11 <- bread11(covs, isTreated)
  b22 <- bread22(pred, isTreated)
  m11 <- meat11(mod1, covs, isTreated)
  m22 <- meat22(mod2$coef[2], mod2$coef[1], resp, pred, isTreated)

  tosolve <- function(eta) {
    b21 <- bread21(eta, mod2$coef[1], resp, covs, pred, isTreated)

    corrected <- correctedvar(b11, b21, b22, m11, m22)
    stat <- qt(.975, mod1$df+2)
    return((mod2$coef[2] - eta)^2 - stat^2*corrected)
  }

  midpoint <- nlm(tosolve, mod2$coef[2])

  # nlm's $code output has 1:2 for convergence, 3:5 for issues.
  if (midpoint$code %in% 1:2) {
    lb <- tryCatch(uniroot(tosolve, c(-100, midpoint$estimate))$root,
                   error=function(c) -Inf)
    ub <- tryCatch(uniroot(tosolve, c(midpoint$estimate, 100))$root,
                   error=function(c) Inf)
    bounds <- c(lb,ub)
  } else {
    bounds <- c(-Inf, Inf)
  }
  return(list(estimate=midpoint$estimate,
              bounds=bounds))
}
