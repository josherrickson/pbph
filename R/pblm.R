pblm <- setClass("pblm", contains = "lm")

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
pblm <- function(mod1, treatment, data) {
  if( !all(treatment %in% 0:1)) {
    stop("treatment must be indicator (0/1) for treatment status")
  }
  isTreated <- treatment

  mod2 <- makemod2(mod1, isTreated, data)

  pred <- mod2$pred
  mod2 <- mod2$mod2

  est <- epbsolve(mod1, mod2, pred, isTreated, data)

  mod2$etabounds <- est$bounds
  mod2$cov.unscaled <- est$covmat

  mod2 <- as(mod2, "pblm")

  return(mod2)
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

    corrected <- correctedvar(b11, b21, b22, m11, m22)[2,2]
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

  covmat <- correctedvar(b11, bread21(mod2$coef[2], mod2$coef[1],
                                      resp, covs, pred, isTreated),
                         b22, m11, m22)

  return(list(estimate=midpoint$estimate,
              bounds=bounds,
              covmat=covmat))
}

##' Summary for pblm object
##'
##' Similar to `summary.lm`.
##' @param object An object of class `pblm`.
##' @param ... Additional arguments to `summary.lm`.
##' @return A summary
##' @export
##' @author Josh Errickson
setMethod("summary", signature(object = "pblm"),
          function(object, ...)
          {
            ss <- summary(as(object, "lm"), ...)

            ss$cov.unscaled <- object$cov.unscaled

            # Remove standard error & p-value
            ss$coefficients[,2] <- sqrt(diag(ss$cov.unscaled))
            ss$coefficients[2,3:4] <- NA
            ss$coefficients[1,3] <- ss$coef[1,1]/ss$coeff[1,2]
            ss$coefficients[1,4] <- min(pnorm(ss$coef[1,3]),
                                              1 - pnorm(ss$coef[1,3]))

            return(ss)
          } )


##' Confidence intervals for pblm object
##'
##' Similar to `confint.lm`.
##' @param object An object of class `pblm`.
##' @param parm Parameters
##' @param level Confidence level
##' @param ... Additional arguments to `confint.lm`.
##' @return Confidence intervals
##' @export
##' @author Josh Errickson
confint.pblm <- function(object, parm, level = 0.95,...)
{
  ci <- confint(as(object, "lm"), parm=parm, level=level, ...)
  if ("pred" %in% rownames(ci)) {
    ci["pred",] <- object$etabounds
  }
  return(ci)
}
