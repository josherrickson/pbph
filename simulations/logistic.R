n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .1
sigma2 <- .3
true_inter <- seq(-.4,.4,by = .1)

reps <- 1000
bigsave <- epb:::makeSaveMatrix(c("truth", "coverage"),
                          reps = length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- round(true_inter[j], 1)
  save <- vector(length = reps)
  for (i in 1:reps) {

    covs <- data.frame(matrix(rnorm(n*p), nrow = n))
    truebeta <- c(rnorm(round(informative*p)),
                  rep(0, round((1 - informative)*p)))

    treatment <- rep(0:1, c(n*pc, n*(1 - pc)))

    pr <- 1/(1 + exp(-as.matrix(covs) %*% truebeta)) + true_t*treatment + ti*treatment*yc_un
    rep <- rbinom(n, 1, pmax(pmin(pr,1),0))
    d <- data.frame(y = resp, covs)

    mod1 <- glm(y ~ ., data = d[treatment == 0,], family = 'binomial',)
    mod2 <- pblm(mod1, treatment, d)

    save[i] <- 2*pt(abs(hypothesisTest(mod2, ti)), mod1$df.null, lower.tail = FALSE)
  }
  coveraged <- table(save <= .05)["FALSE"]
  bigsave[j,] <- c(ti, coveraged)

}

bigsave
