n <- 100
p <- 5
pc <- .4
sigma2 <- 1

reps <- 100

saveCols <- c("truth", "estimate", "lb", "ub", "isfinite","F-stat",
              "R^2", "adj.R^2", "sigma^2", "mean_res^2", "max_beta",
              "min_p", "sig_betas")

saveoracle <- makeSaveMatrix(saveCols, reps = reps)
saveunder <- makeSaveMatrix(saveCols, reps = reps)
saveover <- makeSaveMatrix(saveCols, reps = reps)

for (i in seq_len(reps)) {
  ti <- rnorm(1)
  tt <- rnorm(1)

  covs <- data.frame(matrix(rnorm(n*p), nrow = n))
  truebeta <- c(rnorm(2), 0,0,0)
  treatment <- rep(0:1, c(n*pc, n*(1 - pc)))

  noise <- rnorm(n)
  yc_un <- as.matrix(covs) %*% truebeta
  yt_un <- yc_un + tt*treatment + ti*treatment*yc_un
  resp <- ifelse(treatment == 1, yt_un, yc_un) + noise
  d <- data.frame(y = resp, covs)

  mod1oracle <- lm(y ~ X1 + X2, data = d, subset = treatment == 0)
  mod1under  <- lm(y ~ X1     , data = d, subset = treatment == 0)
  mod1over   <- lm(y ~ .      , data = d, subset = treatment == 0)

  sm1oracle <- summary(mod1oracle)
  sm1under  <- summary(mod1under)
  sm1over   <- summary(mod1over)

  eoracle <- pbph(mod1oracle, treatment, d)
  eunder  <- pbph(mod1under, treatment, d)
  eover   <- pbph(mod1over, treatment, d)
  smoracle <- summary(eoracle)
  smunder  <- summary(eunder)
  smover   <- summary(eover)

  saveoracle[i,] <- c(ti, eoracle$coef[2], confint(eoracle, returnType = TRUE)["pred",],
                      all(is.finite(confint(eoracle, returnType = TRUE)["pred",])),
                      sm1oracle$fstat[1], sm1oracle$r.s,
                      sm1oracle$adj.r.s, sm1oracle$sigma,
                      mean(sm1oracle$res^2),
                      max(abs(mod1oracle$coef)),
                      min(sm1oracle$coef[-1,4]),
                      sum(sm1oracle$coef[-1,4] < .05))

  saveover[i,] <- c(ti, eover$coef[2], confint(eover, returnType = TRUE)["pred",],
                      all(is.finite(confint(eover, returnType = TRUE)["pred",])),
                      sm1over$fstat[1], sm1over$r.s,
                      sm1over$adj.r.s, sm1over$sigma,
                      mean(sm1over$res^2),
                      max(abs(mod1over$coef)),
                      min(sm1over$coef[-1,4]),
                      sum(sm1over$coef[-1,4] < .05))

  saveunder[i,] <- c(ti, eunder$coef[2], confint(eunder, returnType = TRUE)["pred",],
                      all(is.finite(confint(eunder, returnType = TRUE)["pred",])),
                      sm1under$fstat[1], sm1under$r.s,
                      sm1under$adj.r.s, sm1under$sigma,
                      mean(sm1under$res^2),
                      max(abs(mod1under$coef)),
                      min(sm1under$coef[-1,4]),
                      sum(sm1under$coef[-1,4] < .05))

}

mean(saveoracle[,1] > saveoracle[,3] & saveoracle[,1] < saveoracle[,4])
mean(saveunder[,1] > saveunder[,3] & saveunder[,1] < saveunder[,4])
mean(saveover[,1] > saveover[,3] & saveover[,1] < saveover[,4])

table(saveoracle[,"isfinite"])/reps
table(saveunder[,"isfinite"])/reps
table(saveover[,"isfinite"])/reps

var <- "min_p"
boxplot(saveoracle[,var] ~ saveoracle[, "isfinite"])
boxplot(saveunder[,var] ~ saveunder[, "isfinite"])
boxplot(saveover[,var] ~ saveover[, "isfinite"])
