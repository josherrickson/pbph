#' (Internal) Piece-wise generation of Bread and Meat matrices.
#'
#' Computes the Bread and Meat matrices. The diagonal elements are typical
#' sandwich estimators, with scaling, and thus only use the \code{model}
#' argument. The off-diagonal Bread element requires further specification.
#' @param model For Bread & Meat that can be calculated using sandwich package,
#'   we only need the model, either first or second stage, depending.
#' @param eta Estimated version of the coefficient on the interaction between
#'   predicted and treatment. This could be from a model or a hypothesis.
#' @param cluster For Meat only; A vector defining clusters.
#' @import sandwich
#' @name bread_and_meat
NULL
#> NULL

##' (Internal) Return scaling factor for GLM models
##'
##' @param model A model (lm or glm).
##' @param newdata Data to generate the scale.
##' @return Scaling factor. 1 if lm.
glmScale <- function(model, newdata) {
  if (is(model, "glm")) {
    resp <- predict(model, type = "response", newdata = newdata)
    switch(model$family$family,
           "binomial" = scale <- resp*(1 - resp),
           "poisson" = scale <- resp,
           stop(paste("glm family", model$family$family,
                      "not yet supported")
                )
           )
  } else {
    return(1)
  }

  return(scale)
}

##' @rdname bread_and_meat
bread11 <- function(model) {
  x <- model.matrix(model)
  scale <- glmScale(model, newdata=model.frame(model))

  solve(crossprod(x, x * scale))
}

##' @rdname bread_and_meat
bread22 <- function(model) {
  x <- model.matrix(model)

  solve(crossprod(x))
}

##' @rdname bread_and_meat
meat11 <- function(model, cluster = NULL) {
  meat(model, cluster = cluster) * length(residuals(model))
}

##' @rdname bread_and_meat
meat22 <- function(model, cluster = NULL) {
  meat(model, cluster = cluster) * length(residuals(model))
}

##' @rdname bread_and_meat
bread21 <- function(model, eta) {
  mod1 <- model$epb$mod1
  data <- model$epb$data

  treatment <- model$epb$treatment

  resp <- eval(formula(mod1)[[2]], envir = data)[treatment == 1]
  covs <- model.matrix(formula(mod1), data = data)
  covs <- addIntercept(covs)[treatment == 1, , drop=FALSE]
  pred <- model$epb$pred[treatment == 1]

  scale <- glmScale(mod1, newdata=as.data.frame(covs))

  # Replace tauhat with tauhat_eta0
  tau <- lm(resp - (1 + eta) * pred ~ 1)$coef

  b21.1 <- apply(-(1 + eta) * covs * scale, 2, sum)
  b21.2 <- apply( (resp - tau - 2 * (1 + eta) * pred) * covs * scale, 2, sum)

  rbind(b21.1, b21.2)
}

##' (Internal) Computes Bread and Meat matrices.
##'
##' Computes the pieces of the Bread and Meat which do not depend on eta. (e.g.
##' all but B21)
##' @param object A pblm object.
##' @param cluster A vector defining clustering.
##' @return A list of b11, b22, m11, and m22.
createBreadAndMeat <- function(object, cluster = NULL) {

  mod1 <- object$epb$mod1

  b11 <- bread11(mod1)
  b22 <- bread22(object)

  cluster.control <- cluster[object$epb$treatment == 0]
  cluster.treatment <- cluster[object$epb$treatment == 1]

  m11 <- meat11(mod1, cluster = cluster.control)
  m22 <- meat22(object, cluster = cluster.treatment)

  return(list(b11 = b11,
              b22 = b22,
              m11 = m11,
              m22 = m22))
}
