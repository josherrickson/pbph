pblm <- setClass("pblm", contains = "lm")

##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data.
##'
##' @param mod1 First stage fitted model.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in `form` live.
##'
##' @return A pblm object which extends lm. Can be passed to `summary`
##'   or `confint`.
##'
##'   The return contains an additional object, `epb`, which `lm`
##'   doesn't include. `epb` is a list which contains several pieces
##'   necessary for `summary` and `confint` support.
##' @export
##' @author Josh Errickson
##'
pblm <- function(mod1, treatment, data) {
  if( !all(treatment %in% 0:1)) {
    stop("treatment must be indicator (0/1) for treatment status")
  }

  if (sum(1-treatment) != length(resid(mod1))) {
    stop("It appears the first stage model is not fit only on control units.")
  }

  if (length(treatment) != nrow(data)) {
    stop("treatment must be same length as number of observations in data.")
  }

  mm <- makemod2(mod1, treatment, data)

  pred <- mm$pred
  mod2 <- mm$mod2

  mod2$epb <- list(mod1=mod1,
                   pred=pred,
                   treatment=treatment,
                   data=data)

  mod2 <- as(mod2, "pblm")

  return(mod2)
}


##' (Internal) Return covariance matrix associated with a given choice
##' of eta.
##'
##' For a given eta, the covariance can vary. For example, etahat or
##' eta_0.
##' @param eta Value of eta to use.
##' @param object A pblm object.
##' @param breadAndMeat By default, will create breadAndMeat
##'   associated with 'object'. If speed a concern, it is faster to
##'   compute this once and pass into corrVar.
##' @return A covariance matrix
##' @author Josh Errickson
corrVar <- function(eta, object,
                    breadAndMeat=createBreadAndMeat(object)) {

  mod1 <- object$epb[["mod1"]]
  data <- object$epb[["data"]]

  b21 <- bread21(eta,
                 tau = object$coef[1],
                 resp = eval(formula(mod1)[[2]], envir = data),
                 covs = model.matrix(formula(mod1), data = data),
                 pred = object$epb[["pred"]],
                 treatment = object$epb[["treatment"]])

  corrected <- correctedvar(breadAndMeat$b11,
                                         b21,
                            breadAndMeat$b22,
                            breadAndMeat$m11,
                            breadAndMeat$m22)

  return(corrected)
}

##' Computes Bread and Meat matrices.
##'
##' Computes the pieces of the Bread and Meat which do not depend on
##' eta. (e.g. all but B21)
##' @param object A pblm object.
##' @return A list of b11, b22, m11, and m22.
##' @author Josh Errickson
createBreadAndMeat <- function(object) {

  mod1 <- object$epb[["mod1"]]
  data <- object$epb[["data"]]
  covs <- model.matrix(formula(mod1),
                       data = data)
  treatment <- object$epb[["treatment"]]
  pred <- object$epb[["pred"]]

  b11 <- bread11(covs      = covs,
                 treatment = treatment)

  b22 <- bread22(pred      = pred,
                 treatment = treatment)

  m11 <- meat11(mod1       = mod1,
                covs       = covs,
                treatment  = treatment)

  m22 <- meat22(eta        = object$coef[2],
                tau        = object$coef[1],
                resp       = eval(formula(mod1)[[2]], envir = data),
                pred       = pred,
                treatment  = treatment)

  return(list(b11 = b11,
              b22 = b22,
              m11 = m11,
              m22 = m22))
}

##' Conducts a hypothesis test for a given null.
##'
##' @param object A pblm object.
##' @param null Defaults to 0.
##' @return A test statistic, with distribution t(k) where k is the
##'   number of parameters in the first stage model, less 2.
##' @author Josh Errickson
hypothesisTest <- function(object, null=0) {
  return((object$coef[2] - null)/sqrt(corrVar(eta=null, object)[2,2]))
}


##' Returns covariance matrix calculated via sandwich estimation.
##'
##' Returns a covariance matrix. Computed using enhanced PB
##' methods. The variance for pred should NOT be used directly in
##' confidence intervals.
##'
##' @param object pblm object.
##' @return A covariance matrix.
##' @author Josh Errickson
vcov.pblm <- function(object) {
  return(corrVar(object$coef[2], object))
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

            ss$cov.unscaled <- vcov(object)

            # Input corrected Std. Error
            ss$coefficients[,2] <- sqrt(diag(ss$cov.unscaled))

            # Correct test statistic & p-value.
            ss$coefficients[2,3] <- hypothesisTest(object)
            ss$coefficients[2,4] <- pt(abs(ss$coefficients[2,3]),
                                       mod1$df+2,
                                       lower.tail=FALSE)

            # Correct test statistic & p-value for intercept with
            # corrected Std. Error.
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
##' @param level Confidence level.
##' @param ... Additional arguments to `confint.lm`.
##' @param wald.style Logical. Defaults to FALSE so the method
##'   performs test inversion to obtain proper coverage. If TRUE,
##'   generates wald-style CI's, which will likely suffer from
##'   undercoverage.
##' @return Confidence intervals
##' @export
##' @author Josh Errickson
##'
confint.pblm <- function(object, parm, level = 0.95, ...,
                         wald.style=FALSE)
{

  # Do not use confint(as(object, "lm")). `confint.lm` includes a call
  # to vcov; doing it that way will return the originals rather than
  # the corrected versions.
  ci <- confint.lm(object, parm=parm, level=level,...)
  if ("pred" %in% rownames(ci) & !wald.style) {
    ci["pred",] <- testinverse(object, level=level)
  }
  return(ci)
}

##' (Internal) Computes a confidence interval for eta via test
##' inversion.
##'
##' @param object A pblm object.
##' @param level Confidence level. Default is 95\%.
##' @return Vector of length two with the lower and upper bounds.
##' @author Josh Errickson
testinverse <- function(object, level=.95) {

  bAndM <- createBreadAndMeat(object)

  tosolve <- function(eta) {
    corrected <- corrVar(eta, object, bAndM)[2,2]
    stat <- qt((1-level)/2, object$epb[["mod1"]]$df+2)
    return((object$coef[2] - eta)^2 - stat^2*corrected)
  }

  midpoint <- nlm(tosolve, object$coef[2])

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

  return(bounds)
}
