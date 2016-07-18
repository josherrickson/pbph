n <- 1000
p <- 13
pc <- .4
informative <- .4
sigma2 <- .3
params <- data.frame(tau = rep(c(.25, .5, .75), each = 3),
                     eta = c(-.5, -.75, -1,
                             -.75, -1, -1.25,
                             -1, -1.25, -1.5))
params <- rbind(params, data.frame(tau = c(-.25, -.25,
                                           .25, .25, .25,
                                           .75, .75, .75,
                                           1.25, 1.25),
                                   eta = c(.25, -.25,
                                           .25, -1.75, -2.25,
                                           .25, -.25, -2.25,
                                           -1.75, -2.25)))

reps <- 1000
bigsave <- pbph:::makeSaveMatrix(c("true tau", "true eta", "coverage", "cont", "disjoint"),
                                reps = nrow(params))
for (j in 1:nrow(params)) {
  tt <- params[j, 1]
  ti <- params[j, 2]
  save <- pbph:::makeSaveMatrix(c("covered", "type"), reps = reps)
  for (i in 1:reps) {

    covs <- data.frame(matrix(rnorm(n*p), nrow = n))
    truebeta <- c(rnorm(round(informative * p)),
                  rep(0, round((1 - informative) * p)))

    treatment <- rep(0:1, c(n * pc, n * (1 - pc)))

    pr <- 1/(1 + exp(-as.matrix(covs) %*% truebeta))
    pr <- pr + tt * treatment + ti * treatment * pr
    resp <- rbinom(n, 1, pmax(pmin(pr,1),0))
    d <- data.frame(y = resp, covs)

    mod1 <- glm(y ~ ., data = d[treatment == 0, ], family = binomial)
    mod2 <- pbph(mod1, treatment, d)

    ci <- confint(mod2, "pred", returnShape = TRUE)
    type <- attr(ci, "shape")
    if (type == "finite") {
      covered <- ci[1] < ti & ti < ci[2]
    } else if (type == "infinite") {
      covered <- TRUE
    } else if (type == "disjoint") {
      covered <- ti < ci[1] | ci[2] < ti
    } else {
      stop(paste("Problem:", type))
    }
    type <- switch(type,
                   finite = 1,
                   infinite = 1,
                   disjoint = 2)


    save[i,] <- c(covered, type)

  }
  coveraged <- table(save[,1])["1"]
  bigsave[j,] <- c(tt, ti, coveraged,
                   sum(save[,2] == 1),
                   sum(save[,2] == 2))

}

bigsave

plot(NULL, xlim = c(-.5, 1.5), ylim = c(-2.5, 0.5), xlab = expression(tau), ylab = expression(eta))
polygon(c(1, 1, -4), c(3, -2, 3), col = 'lightblue')
polygon(c(0, 3, 0), c(0, -3, -3), col = 'lightgreen')
polygon(c(0, 1, 1, 0), y = c(0, -1, -2, -1), col = 'red')
perc <- paste0(round(bigsave[,3]/reps*100), "%")
text(bigsave[,1:2], labels = perc)
