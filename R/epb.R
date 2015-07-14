##' Constructor function for Enhanced Peters-Belson Model
##'
##' @param estimate Estimate of $eta$
##' @param bounds Confidence bounds for $eta$. Can in -Inf,Inf.
##' @param mod1 First stage model.
##' @param mod2 Second stage model.
##' @return An object of class "ebpm"
##' @export
##' @author Josh Errickson
##'
epbm <- function(estimate, bounds, mod1, mod2) {
  stopifnot(is.numeric(estimate))
  stopifnot(is.numeric(bounds))
  stopifnot(length(bounds) == 2)
  stopifnot(is(mod1, "lm"))
  stopifnot(is(mod2, "lm"))

structure(list(estimate=estimate,
               bounds=bounds,
               mod1=mod1,
               mod2=mod2),
          class="epbm")
}


##' Enhanced Peters-Belson method
##'
##' Performs enhanced Peters-Belson on the data. First stage is fit on
##' `resp ~ covs`. *TODO* REPLACE WITH A FORMULA.
##'
##' @param form First stage model formula.
##' @param treatment Vector of 0/1 treatment indicators.
##' @param data Data where variables in `form` live.
##' @param profile.likelihood logical. If TRUE, $tau$ in b21 is
##'   estimated using profile likelihood, by default it is just
##'   tauhat.
##'
##' @return Vector consisting of an estimate of eta and confidence
##'   bounds.
##' @export
##' @author Josh Errickson
##'
epb <- function(form, treatment, data, profile.likelihood=FALSE) {
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
    if (profile.likelihood) {
      mod2b <- lm(resp[treatment==1] - eta*mods$pred[treatment==1] ~ 1)
    }

    b21 <- bread21(eta, if(profile.likelihood) mod2b$coef[1] else
                   mods$mod2$coef[1], resp, covs, mods$pred,
                   treatment)

    corrected <- correctedvar(b11, b21, b22, m11, m22)

    stat <- qt(.975, mods$mod1$df+2)
    return((mods$mod2$coef[2] - eta)^2 - stat^2*corrected)
  }

  midpoint <- nlm(tosolve, mods$mod2$coef[2])

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
  return(epbm(estimate=midpoint$estimate,
              bounds=bounds,
              mod1=mods$mod1,
              mod2=mods$mod2))
}


##' @export
print.epbm <- function(x, digits=3, ...) {
  # Idea cribbed from print.data.frame
  d <- data.frame(estimate=x$estimate,
                  lb=x$bounds[1],
                  up=x$bounds[2])
  m <- as.matrix(format.data.frame(d, digits=digits))

  colnames(m) <- c("Estimate", "Lower Bound", "Upper Bound")
  rownames(m) <- ""

  print(m, ..., quote=FALSE, right=TRUE)
}

show.epbm <- function(x) print(x)
