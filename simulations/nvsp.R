rm(list = ls())
pc <- .5
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by = .5)
ps <- 1:50
ns <- c(50, 100, 500, 1000)
reps <- 100
bigsave <- matrix(ncol = length(ps),
                  nrow = length(ns))

for (j in seq_along(ps)) {
  for (k in seq_along(ns)) {
    p <- ps[j]
    n <- ns[k]
    if (p > .25*n) next()
    ti <- sample(true_inter,1)
    save <- vector(length = reps)
    for (i in 1:reps) {
      covs <- data.frame(matrix(rnorm(n*p), nrow = n))
      truebeta <- rnorm(p)

      treatment <- rbinom(n, 1, 1 - pc)

      noise <- rnorm(n)
      yc_un <- as.matrix(covs) %*% truebeta
      yt_un <- yc_un + true_t*treatment + ti*treatment*yc_un
      resp <- ifelse(treatment == 1, yt_un, yc_un) + noise
      d <- data.frame(y = resp, covs)

      mod1 <- lm(y ~ ., data = d, subset = treatment == 0)

      e <- pblm(mod1, treatment, d)
      ci <- confint(e, "pred", returnShape = TRUE)
      type <- attr(ci, "type")
      covered <- ci[1] < ti & ti < ci[2]
      if (type == "disjoint") {
        covered <- !covered
      }

      save[i] <- covered

    }
    bigsave[k,j] <- mean(save)
  }
}
rownames(bigsave) <- ns
colnames(bigsave) <- ps
bigsave

#plot(bigsave ~ ps, ylim = c(.75, 1), type = 'l')
