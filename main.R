n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by=.5)

reps <- 100
bigsave <- matrix(nrow=length(true_inter), ncol=3)
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- matrix(nrow=reps, ncol=3)
  for (i in 1:reps) {
    covs <- data.frame(matrix(rnorm(n*p), nrow=n))
    truebeta <- rep(0, p)
    truebeta[sample(1:p, round(informative*p))] <- rnorm(round(informative*p),,1)

    treatment <- rep(0:1, c(n*pc, n*(1-pc)))

    noise <- rnorm(n)
    yc_un <- as.matrix(covs)%*%truebeta
    yt_un <- yc_un + true_t*treatment + ti*treatment*yc_un
    resp <- ifelse(treatment==1, yt_un, yc_un) + noise

    save[i,] <- epb(resp, covs, treatment)
  }
  bigsave[j,] <- c(ti, mean(save[save[,2] != -Inf,1]),
                         sum(save[,2] < ti & save[,3] > ti))
}

#pdf("~/Desktop/coverage.pdf")
#plot(bigsave[,3]~bigsave[,1], type='l', ylim=c(80,100), xlab="True Eta", ylab="Coverage")
#abline(h=95)
#legend("topright", legend=c("Basic", "General", "Simplified"), lty=c(1,1), col=1:3)
#dev.off()

bigsave
