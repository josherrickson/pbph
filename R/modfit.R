##' Perform model fitting for the enhanced PB method.
##'
##' This  will fit a linear first stage model on the control group,
##' then use that model to predict $Y_c$ in the treatment group. Then a
##' linear second stage model will be fitted on the treatment group.
##'
##' The first stage model is
##'
##' resp ~ covs | treatment == 0
##'
##' Let the predicted values be "pred." The second stage model is
##'
##' resp - pred ~ pred | treatment == 1
##'
##' @param form First stage model formula.
##' @param treatment A vector of treatment statuses, should be all `0` or `1`.
##' @param data Data where variables in `form` live.
##' @param center Default FALSE. Should the covariates be centered in the
##' second stage?
##'
##' @return A list consisting of the two models and predicted values (across both
##' groups)
##' @author Josh Errickson
modfit <- function(form, treatment, data, center=FALSE) {

  # First stage model, linear
  mod1 <- lm(form, data=data[treatment==0,,drop=FALSE])

  # center the covariates. Why do we do this?
  if (center) data.center <- data.frame(scale(data, scale=FALSE))

  # Get predicted values.
  pred <- predict(mod1, newdata=if(center) data.center else data)

  # Second stage linear model.
  res <- eval(form[[2]], envir=data) - pred
  mod2 <- lm(res[treatment==1] ~ pred[treatment==1])

  return(list(mod1=mod1,
              mod2=mod2,
              pred=pred))
}
