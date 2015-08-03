n <- 100
p <- 5
pc <- .4
informative <- .4
sigma2 <- 1

reps <- 100

save <- makeSaveMatrix(c("truth", "estimate", "naivese", "correctedse"), reps=reps)

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

  mod1 <- lm(y ~ ., data=d, subset=treatment==0)

  e <- pblm(mod1, treatment, d)
  sm <- summary(e)
  smlm <- summary.lm(e)

  save[i,] <- c(tt, sm$coef[1,1:2], smlm$coef[1,2])


}

save <- as.data.frame(save)


save$ubnaive <- save$estimate + 1.96*save$naivese
save$lbnaive <- save$estimate - 1.96*save$naivese

save$ubcorrected <- save$estimate + 1.96*save$correctedse
save$lbcorrected <- save$estimate - 1.96*save$correctedse

table(save$truth < save$ubnaive & save$truth > save$lbnaive)
table(save$truth < save$ubcorrected & save$truth > save$lbcorrected)
