n <- 100
p <- 5
pc <- .4
informative <- .4
sigma2 <- 1

reps <- 1000

save <- makeSaveMatrix(c("truth", "estimate", "lb", "ub", "isfinite",
               "F-stat", "R^2", "adj.R^2", "sigma^2", "mean_res^2",
               "max_beta", "min_p", "sig_betas"), reps=reps)

for (i in seq_len(reps)) {
  ti <- rnorm(1)
  tt <- rnorm(1)

  covs <- data.frame(matrix(rnorm(n*p), nrow=n))
  truebeta <- c(rnorm(round(informative*p)),
                rep(0, round((1-informative)*p)))
  treatment <- rep(0:1, c(n*pc, n*(1-pc)))

  noise <- rnorm(n)
  yc_un <- as.matrix(covs)%*%truebeta
  yt_un <- yc_un + tt*treatment + ti*treatment*yc_un
  resp <- ifelse(treatment==1, yt_un, yc_un) + noise
  d <- data.frame(y=resp, covs)

  e <- epb(y ~ ., treatment, data=d)
  sm <- summary(e$mod1)

  save[i,] <- c(ti, e$estimate, e$bounds, all(is.finite(e$bound)),
                sm$fstat[1], sm$r.s, sm$adj.r.s, sm$sigma,
                mean(sm$res^2), max(abs(e$mod1$coef)),
                min(sm$coef[-1,4]), sum(sm$coef[-1,4] < .05))
}

boxplot(save[,6] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="log(F-stat)")
boxplot(save[,7] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="R^2")
boxplot(save[,8] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Adj. R^2")
boxplot(save[,7] - save[,8] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Change from R^2 to Adj. R^2")
boxplot(save[,9] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Sigma^2")
boxplot(save[,10] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Mean Res^2")
boxplot(save[,11] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Max Abs beta")
boxplot(save[,12] ~ save[,"isfinite"], xlab="Finite CI",
        ylab="Min p-value", ylim=c(0,.01))
table(save[,13], save[,"isfinite"]) # # significant beta's
