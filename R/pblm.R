pblm <- setClass("pblm", contains = "lm")

##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data.
##' @param mod1 First stage fitted model.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in \code{form} live.
##' @param center Default \code{TRUE}. Should the predicted values be
##'   centered in the second stage?
##' @param cluster A \code{vector} defining the clusters.
##' @return A \code{pblm} object which extends \code{lm}. Can be
##'   passed to \code{summary} or \code{confint}.
##'
##'   The return contains an additional object, \code{epb}, which
##'   \code{lm} doesn't include. \code{epb} is a list which contains
##'   several pieces necessary for \code{summary} and \code{confint}
##'   support.
##' @export
pblm <- function(mod1, treatment, data, center = TRUE, cluster = NULL) {
  if (!all(treatment %in% 0:1)) {
    stop("treatment must be indicator (0/1) for treatment status")
  }

  if (sum(1 - treatment) != length(resid(mod1))) {
    stop("It appears the first stage model is not fit only on control units.")
  }

  if (length(treatment) != nrow(data)) {
    stop("treatment must be same length as number of observations in data.")
  }

  mm <- makemod2(mod1, treatment, data, center = center)

  pred <- mm$pred
  mod2 <- mm$mod2

  mod2$epb <- list(mod1      = mod1,
                   pred      = pred,
                   treatment = treatment,
                   data      = data,
                   cluster   = cluster)

  mod2 <- as(mod2, "pblm")

  return(mod2)
}

##' Returns covariance matrix calculated via sandwich estimation.
##'
##' Returns a covariance \code{matrix}. Computed using enhanced PB
##' methods. The variance for pred should NOT be used directly in
##' confidence intervals.
##' @param object \code{pblm} object.
##' @return A covariance \code{matrix}.
vcov.pblm <- function(object) {
  return(corrVar(object$coef[2], object))
}

##' @export
summary.pblm <- function(object, ...) {
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
                                 lower.tail = FALSE)

  # Correct test statistic & p-value for intercept with
  # corrected Std. Error.
  ss$coefficients[1,3] <- ss$coef[1,1] / ss$coeff[1,2]
  ss$coefficients[1,4] <- 2 * pnorm(abs(ss$coef[1,3]),
                                    lower.tail = FALSE)

  class(ss) <- "summary.pblm"

  return(ss)
}

##' @export
print.summary.pblm <- function(x, ...) {
  class(x) <- "summary.lm"
  print(x, ...)
}

##' Confidence intervals for pblm object
##'
##' Similar to \code{confint.lm}.
##' @param object An object of class \code{pblm}.
##' @param parm Parameters
##' @param level Confidence level.
##' @param ... Additional arguments to \code{confint.lm}.
##' @param wald.style Logical. Defaults to \code{FALSE} so the method
##'   performs test inversion to obtain proper coverage. If
##'   \code{TRUE}, generates wald-style CI's, which will likely suffer
##'   from undercoverage.
##' @param forceDisplayConfInt Logical. Defaults to \code{FALSE}. If a
##'   hypothesis test for the interaction term fails, a confidence
##'   interval is unreliable (and ultimately unnecessary) and thus not
##'   returned. Set this to \code{TRUE} to force computation of the
##'   interval regardless of the hypothesis test.
##' @return Confidence intervals
##' @export
confint.pblm <- function(object, parm, level = 0.95, ...,
                         wald.style = FALSE, forceDisplayConfInt = FALSE) {
  if (wald.style & forceDisplayConfInt) {
    warning("Argument 'forceDisplayConfInt' ignored when 'wald.style' is TRUE.")
  }

  # Temp disabling this
  forceDisplayConfInt <- TRUE

  # Do not use confint(as(object, "lm")). `confint.lm` includes a call to vcov;
  # doing it that way will return the originals rather than the corrected
  # versions.
  ci <- confint.lm(object, parm = parm, level = level,...)
  if ("pred" %in% rownames(ci) & !wald.style) {
    if (summary(object)$coef[2,4] > .05 & !forceDisplayConfInt) {
      cat("Interaction term not significant, suppressing associated confidence interval. ")
      cat("Use 'forceDisplayConfInt = TRUE' to force it.\n")
      ci["pred",] <- c(NA, NA)
    } else {
      ti <- testinverse(object, level = level)
      ci["pred",] <- ti
      attr(ci, "type") <- attr(ti, "type")
    }
  }
  return(ci)
}
