n <- 100
p <- 5
pc <- .4
sigma2 <- 1

reps <- 100

saveCols <- c("truth", "estimate", "lb", "ub", "F-stat", "R^2",
              "adj.R^2", "sigma^2", "mean_res^2", "max_beta", "min_p",
              "sig_betas")
saveoracle <- makeSaveMatrix(saveCols, reps=reps)
saveunder <- makeSaveMatrix(saveCols, reps=reps)
saveover <- makeSaveMatrix(saveCols, reps=reps)

for (i in seq_len(reps)) {
  ti <- rnorm(1)
  tt <- rnorm(1)

  covs <- data.frame(matrix(rnorm(n*p), nrow=n))
  truebeta <- c(rnorm(2), 0,0,0)
  treatment <- rep(0:1, c(n*pc, n*(1-pc)))

  noise <- rnorm(n)
  yc_un <- as.matrix(covs)%*%truebeta
  yt_un <- yc_un + tt*treatment + ti*treatment*yc_un
  resp <- ifelse(treatment==1, yt_un, yc_un) + noise
  d <- data.frame(y=resp, covs)

  eoracle <- epb(y ~ X1 + X2 , treatment, data=d)
  eunder  <- epb(y ~ X1      , treatment, data=d)
  eover   <- epb(y ~ .       , treatment, data=d)
  smoracle <- summary(eoracle$mod1)
  smunder  <- summary(eunder$mod1)
  smover   <- summary(eover$mod1)

  saveoracle[i,] <- c(ti, eoracle$estimate, eoracle$bounds,
                      smoracle$fstat[1], smoracle$r.s,
                      smoracle$adj.r.s, smoracle$sigma,
                      mean(smoracle$res^2),
                      max(abs(eoracle$mod1$coef)),
                      min(smoracle$coef[-1,4]),
                      sum(smoracle$coef[-1,4] < .05))

  saveunder[i,] <- c(ti, eunder$estimate, eunder$bounds,
                     smunder$fstat[1], smunder$r.s, smunder$adj.r.s,
                     smunder$sigma, mean(smunder$res^2),
                     max(abs(eunder$mod1$coef)),
                     min(smunder$coef[-1,4]), sum(smunder$coef[-1,4]
                                                  < .05))

  saveover[i,] <- c(ti, eover$estimate, eover$bounds, smover$fstat[1],
                    smover$r.s, smover$adj.r.s, smover$sigma,
                    mean(smover$res^2), max(abs(eover$mod1$coef)),
                    min(smover$coef[-1,4]), sum(smover$coef[-1,4] <
                                                  .05))

}


sum(saveoracle[,1] > saveoracle[,3] & saveoracle[,1] < saveoracle[,4])
sum(saveunder[,1] > saveunder[,3] & saveunder[,1] < saveunder[,4])
sum(saveover[,1] > saveover[,3] & saveover[,1] < saveover[,4])
