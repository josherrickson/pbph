n <- 100
p <- 5
pc <- .4
informative <- .4
sigma2 <- 1

reps <- 100
savenames <- c("truth", "estimate", "lb", "ub", "F-stat",
               "R^2", "adj.R^2", "sigma^2", "mean_res^2",
               "max_beta", "min_p", "sig_betas")
save <- matrix(nrow=reps, ncol=length(savenames))
colnames(save) <- savenames

for (i in seq_len(reps)) {
  ti <- rnorm(1)
  tt <- rnorm(1)

  covs <- data.frame(matrix(rnorm(n*p), nrow=n))
  truebeta <- rep(0, p)
  truebeta[sample(1:p, round(informative*p))] <- rnorm(round(informative*p),0,1)

  treatment <- rep(0:1, c(n*pc, n*(1-pc)))

  noise <- rnorm(n)
  yc_un <- as.matrix(covs)%*%truebeta
  yt_un <- yc_un + tt*treatment + ti*treatment*yc_un
  resp <- ifelse(treatment==1, yt_un, yc_un) + noise
  d <- data.frame(y=resp, covs)

  e <- epb(y ~ ., treatment, data=d)
  sm <- summary(e$mod1)

  save[i,] <- c(ti, e$estimate, e$bounds, sm$fstat[1], sm$r.s,
                sm$adj.r.s, sm$sigma, mean(sm$res^2),
                max(abs(e$mod1$coef)), min(sm$coef[-1,4]),
                sum(sm$coef[-1,4] < .05))
}

boxplot(log(save[,5]) ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="log(F-stat)")
boxplot(save[,6] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="R^2")
boxplot(save[,7] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Adj. R^2")
boxplot(save[,6] - save[,7] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Change from R^2 to Adj. R^2")
boxplot(save[,8] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Sigma^2")
boxplot(save[,9] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Mean Res^2")
boxplot(save[,10] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Max Abs beta")
boxplot(save[,11] ~ is.finite(save[,4]), xlab="Finite CI",
        ylab="Min p-value", ylim=c(0,.1))
table(save[,12], is.finite(save[,4])) # # significant beta's
