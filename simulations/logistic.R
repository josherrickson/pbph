n <- 100
p <- 5
pc <- .4
informative <- .7
true_t <- .5
sigma2 <- .3
true_inter <- seq(-1,2,by = .5)

reps <- 100
bigsave <- epb:::makeSaveMatrix(c("truth", "coverage"),
                          reps = length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- vector(length = reps)
  for (i in 1:reps) {
    covs <- data.frame(matrix(rnorm(n*p), nrow = n))
    truebeta <- c(rnorm(round(informative*p)),
                  rep(0, round((1 - informative)*p)))

    treatment <- rep(0:1, c(n*pc, n*(1 - pc)))

    noise <- runif(n, -.5, .5)
    yc_un <- 1/(1 + exp(as.matrix(covs) %*% truebeta))
    yt_un <- yc_un + true_t*treatment + ti*treatment*yc_un
    resp <- round(ifelse(treatment == 1, yt_un, yc_un) + noise)
    d <- data.frame(y = resp, covs)

    mod1 <- glm(y ~ ., data = d[treatment == 0,], family = 'binomial')
    mod2 <- pblm(mod1, treatment, d)

    save[j] <- 2*pt(abs(hypothesisTest(mod2, ti)), mod1$df.null, lower.tail = FALSE)
  }
  coveraged <- table(save <= .05)["FALSE"]
  bigsave[j,] <- c(ti, coveraged)

}

bigsave
