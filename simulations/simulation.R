n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by=.5)

reps <- 100
bigsave <- makeSaveMatrix(c("truth", "estimate", "#cov", "#inf",
                            "#uncov"),
                          length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- makeSaveMatrix(c("estimate", "lb", "ub"),
                         reps)
  for (i in 1:reps) {
    covs <- data.frame(matrix(rnorm(n*p), nrow=n))
    truebeta <- rep(0, p)
    truebeta[sample(1:p, round(informative*p))] <- rnorm(round(informative*p),0,1)

    treatment <- rep(0:1, c(n*pc, n*(1-pc)))

    noise <- rnorm(n)
    yc_un <- as.matrix(covs)%*%truebeta
    yt_un <- yc_un + true_t*treatment + ti*treatment*yc_un
    resp <- ifelse(treatment==1, yt_un, yc_un) + noise
    d <- data.frame(y=resp, covs)

    mod1 <- lm(y ~ ., data=d, subset=treatment==0)

    e <- pblm(mod1, treatment, d)
    save[i,] <- c(e$coef[2], summary(e)$coef[2,5:6])
  }
  saveFinite <- save[save[,2] != -Inf,]
  numcov <- sum(saveFinite[,2] < ti & saveFinite[,3] > ti)
  numuncov <- nrow(saveFinite) - numcov
  numinf <- reps - nrow(saveFinite)
  bigsave[j,] <- c(ti, mean(save[save[,2] != -Inf,1]),
                   numcov, numinf, numuncov)

}

#pdf("~/Desktop/coverage.pdf")
#plot(bigsave[,3]~bigsave[,1], type='l', ylim=c(80,100), xlab="True Eta", ylab="Coverage")
#abline(h=95)
#legend("topright", legend=c("Basic", "General", "Simplified"), lty=c(1,1), col=1:3)
#dev.off()

bigsave
