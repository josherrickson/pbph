rm(list = ls())
pc <- .5
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by = .5)
ps <- 1:30
ns <- c(25, 100, 200, 500, 1000)
reps <- 1000
bigsave <- matrix(ncol = length(ps),
                  nrow = length(ns))
rownames(bigsave) <- ns
colnames(bigsave) <- ps

for (j in seq_along(ps)) {
  print(j)
  p <- ps[j]
  for (k in seq_along(ns)) {
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
bigsave

plot(NULL, xlim = c(0, max(as.numeric(colnames(bigsave)))), ylim = c(.8, 1))
for (i in seq_len(nrow(bigsave))) {
  #lines(bigsave[i,] ~ as.numeric(colnames(bigsave)), col = i)
  lines(predict(loess(bigsave[i,] ~ as.numeric(colnames(bigsave)))), col = i)
}
legend("bottomleft", legend = rownames(bigsave), col = 1:4, lty = 1)
abline(h = .95, col = 'lightgrey', lty = 2)

# Add lines for the rule
f <- function(n) n^2*log(n)
abline(v = 5, col = 1, lty = 2)
abline(v = 7, col = 2, lty = 2)
abline(v = 13, col = 3, lty = 2)
abline(v = 18, col = 4, lty = 2)
