##' INTERNAL. Perform model fitting for the enhanced PB method.
##'
##' Predict the values from the first stage model `mod1` through
##' `data`, then fit
##'
##' resp - pred ~ pred | treatment == 1
##'
##' @param mod1 First stage model fit
##' @param isTreated A vector of treatment statuses, should be all `0`
##'   or `1`.
##' @param data Data where variables in `form` live.
##' @param center Default FALSE. Should the covariates be centered in
##'   the second stage?
##'
##' @return A list consisting of `mod2`, the second stage model, and
##'   `pred`, the predicted values from `mod1`.
##' @author Josh Errickson
##'
makemod2 <- function(mod1, isTreated, data, center=FALSE) {

  # center the covariates. Why do we do this?
  if (center) data.center <- data.frame(scale(data, scale=FALSE))

  # Get predicted values.
  predicted <- predict(mod1, newdata=if(center) data.center else data)

  # Second stage linear model.
  y_t <- eval(formula(mod1)[[2]], envir=data)[isTreated==1]
  pred <- predicted[isTreated==1]
  treatment <- rep(1, sum(isTreated))
  mod2 <- lm(y_t - pred ~ treatment + pred + 0)

  return(list(mod2=mod2, pred=predicted))
}