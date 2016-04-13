n <- 100
p <- 5
pc <- .4
informative <- .4
sigma2 <- 1

reps <- 100

save <- makeSaveMatrix(c("truth", "estimate", "lb", "ub", "isfinite",
               "F-stat", "R^2", "adj.R^2", "sigma^2", "rss",
               "max_beta", "min_p"), reps = reps)

for (i in seq_len(reps)) {
  ti <- rnorm(1)
  tt <- rnorm(1)

  covs <- data.frame(matrix(rnorm(n*p), nrow = n))
  truebeta <- c(rnorm(round(informative*p)),
                rep(0, round((1 - informative)*p)))
  treatment <- rep(0:1, c(n*pc, n*(1 - pc)))

  noise <- rnorm(n)
  yc_un <- as.matrix(covs) %*% truebeta
  yt_un <- yc_un + tt*treatment + ti*treatment*yc_un
  resp <- ifelse(treatment == 1, yt_un, yc_un) + noise
  d <- data.frame(y = resp, covs)

  mod1 <- lm(y ~ ., data = d, subset = treatment == 0)
  smod1 <- summary(mod1)

  e <- pblm(mod1, treatment, d)
  sm <- summary(e)

  save[i,] <- c(ti, e$coef[2], confint(e, returnType = TRUE)["pred",],
                all(is.finite(confint(e, returnType = TRUE)["pred",])), smod1$fstat[1],
                smod1$r.s, smod1$adj.r.s, smod1$sigma,
                sum(smod1$res^2), max(abs(smod1$coef)),
                min(smod1$coef[-1,4]))

}

save <- as.data.frame(save)


boxplot(log(save$"F-stat") ~ save$isfinite, xlab = "Finite CI_1",
        ylab = "log(F-stat)")
boxplot(1 - pf(save$"F-stat", 5, 34) ~ save$isfinite, xlab = "Finite CI_1",
        ylab = "F-test p-value")
abline(h = .05)
boxplot(save$"R^2" ~ save$isfinite, xlab = "Finite CI_1",
        ylab = "R^2")
boxplot(save$"adj.R^2" ~ save$isfinite, xlab = "Finite CI",
        ylab = "Adj. R^2")
boxplot(save$sigma ~ save$isfinite, xlab = "Finite CI",
        ylab = "Sigma^2")
boxplot(save$rss ~ save$isfinite, xlab = "Finite CI",
        ylab = "RSS")
boxplot(save$max_beta~ save$isfinite, xlab = "Finite CI",
        ylab = "Max Abs beta")
boxplot(save$min_p~ save$isfinite, xlab = "Finite CI",
        ylab = "Min p-value", ylim = c(0,.01))

max(save$"R^2"[save$isfinite == 0])

table(.bincode(save$"R^2", seq(0,1,by = .1))/10, save$isfinite)
