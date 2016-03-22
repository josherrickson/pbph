##' (Internal) Fit the second stage Enhanced Peters-Belson model.
##'
##' Given a first stage model fit only on the control group, fit a second stage
##' model with formula.
##'
##' resp - pred ~ pred | treatment == 1
##'
##' @param mod1 First stage model fit
##' @param trtmt A vector of treatment statuses, should be all
##'   \code{0} or \code{1}.
##' @param data Data where variables in \code{form} live.
##' @param center Default \code{FALSE}. Should the predicted values be
##'   centered in the second stage?
##' @return A \code{list} consisting of \code{mod2}, the second stage
##'   model, and \code{pred}, the predicted values from \code{mod1}.
makemod2 <- function(mod1, trtmt, data, center = FALSE) {

  # Center the covariates to orthogonalize intercept and slope.
  if (center) {
    data.center <- data
    numeric_cols <- sapply(data, is.numeric)
    data.center[numeric_cols] <- data.frame(scale(data[numeric_cols],
                                                  scale = FALSE))
  }

  # Get predicted values.
  predicted <- predict(mod1,
                       type = "response",
                       newdata = if (center) data.center else data)

  # Second stage linear model.
  respname <- formula(mod1)[[2]]
  newdata <- data.frame(pred      = predicted[trtmt == 1],
                        treatment = rep(1, sum(trtmt)))
  newdata[[paste0(respname, "_t")]] <- eval(respname, envir = data)[trtmt == 1]


  mod2 <- lm(as.formula(paste0(respname,
                               "_t - pred ~ treatment + pred + 0")),
             data = newdata)

  mod2$call$formula <- as.formula(paste0(respname,
                                  "_t - pred ~ treatment + pred"))

  return(list(mod2 = mod2, pred = predicted))
}
