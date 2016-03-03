pblm <- setClass("pblm", contains = "lm")

##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data.
##'
##' @param mod1 First stage fitted model.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in `form` live.
##' @param center Default TRUE. Should the predicted values be
##'   centered in the second stage?
##' @param clusters List of clusters
##' @return A pblm object which extends lm. Can be passed to `summary`
##'   or `confint`.
##'
##'   The return contains an additional object, `epb`, which `lm`
##'   doesn't include. `epb` is a list which contains several pieces
##'   necessary for `summary` and `confint` support.
##' @export
##' @author Josh Errickson
pblm <- function(mod1, treatment, data, center=TRUE, clusters=list()) {
  if( !all(treatment %in% 0:1)) {
    stop("treatment must be indicator (0/1) for treatment status")
  }

  if (sum(1 - treatment) != length(resid(mod1))) {
    stop("It appears the first stage model is not fit only on control units.")
  }

  if (length(treatment) != nrow(data)) {
    stop("treatment must be same length as number of observations in data.")
  }

  mm <- makemod2(mod1, treatment, data, center=center)

  pred <- mm$pred
  mod2 <- mm$mod2

  mod2$epb <- list(mod1=mod1,
                   pred=pred,
                   treatment=treatment,
                   data=data,
                   clusters=clusters)

  mod2 <- as(mod2, "pblm")

  return(mod2)
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
setMethod("summary", "pblm",
          function(object, ...) {
            ss <- summary(as(object, "lm"), ...)

            ss$cov.unscaled <- vcov(object)

            # Input corrected Std. Error
            ss$coefficients[,2] <- sqrt(diag(ss$cov.unscaled))

            # Correct test statistic & p-value.
            ss$coefficients[2,3] <- hypothesisTest(object)
            mod1 <- object$epb$mod1
            df <- ifelse(is(mod1, "glm"), mod1$df.null, mod1$df)
            ss$coefficients[2,4] <- 2 * pt(abs(ss$coefficients[2,3]),
                                           df,
                                           lower.tail=FALSE)

            # Correct test statistic & p-value for intercept with
            # corrected Std. Error.
            ss$coefficients[1,3] <- ss$coef[1,1] / ss$coeff[1,2]
            ss$coefficients[1,4] <- 2 * pnorm(abs(ss$coef[1,3]),
                                              lower.tail=FALSE)

            class(ss) <- "summary.pblm"

            return(ss)
          } )

##' Print `summary.pblm` objects correctly
##'
##' @param x A `summary.pblm` object.
##' @param ... Additional arguments to `print`.
##' @return Outputs a printed result similar to `print.summary.lm`.
##' @author Josh Errickson
##' @export
print.summary.pblm <- function(x, ...) {
  class(x) <- "summary.lm"
  print(x, ...)
}

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
##' @param forceDisplayConfInt Logical. Defaults to FALSE. If a
##'   hypothesis test for the interaction term fails, a confidence
##'   interval is unreliable (and ultimately unnecessary) and thus not
##'   returned. Set this to TRUE to force computation of the interval
##'   regardless of the hypothesis test.
##' @return Confidence intervals
##' @export
##' @author Josh Errickson
##'
confint.pblm <- function(object, parm, level = 0.95, ...,
                         wald.style=FALSE, forceDisplayConfInt=FALSE) {
  if (wald.style & forceDisplayConfInt) {
    warning("Argument 'forceDisplayConfInt' ignored when 'wald.style' is TRUE.")
  }

  # Temp disabling this
  forceDisplayConfInt <- TRUE

  # Do not use confint(as(object, "lm")). `confint.lm` includes a call
  # to vcov; doing it that way will return the originals rather than
  # the corrected versions.
  ci <- confint.lm(object, parm=parm, level=level,...)
  if ("pred" %in% rownames(ci) & !wald.style) {
    if (summary(object)$coef[2,4] > .05 & !forceDisplayConfInt) {
      cat("Interaction term not significant, suppressing associated confidence interval. ")
      cat("Use 'forceDisplayConfInt=TRUE' to force it.\n")
      ci["pred",] <- c(NA, NA)
    } else {
      ti <- testinverse(object, level=level)
      ci["pred",] <- ti
      attr(ci, "type") <- attr(ti, "type")
    }
  }
  return(ci)
}
