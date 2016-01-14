n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by=.5)

reps <- 100
bigsave <- epb:::makeSaveMatrix(c("truth", "estimate", "finite_un",
                            "finite_cov", "inf_un", "inf_cov",
                            "disjoint_un", "disjoint_cov"),
                          reps=length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- epb:::makeSaveMatrix(c("estimate","lb","ub", "type", "covered"),
                         reps=reps)
  for (i in 1:reps) {
    covs <- data.frame(matrix(rnorm(n*p), nrow=n))
    truebeta <- c(rnorm(round(informative*p)),
                  rep(0, round((1-informative)*p)))

    treatment <- rep(0:1, c(n*pc, n*(1-pc)))

    noise <- runif(n, -.5, .5)
    yc_un <- 1/(1 + exp(as.matrix(covs)%*%truebeta))
    yt_un <- yc_un + true_t*treatment + ti*treatment*yc_un
    resp <- round(ifelse(treatment==1, yt_un, yc_un) + noise)
    d <- data.frame(y=resp, covs)

    mod1 <- glm(y ~ ., data=d[treatment==0,], family='binomial')
    mod2 <- pblm(mod1, treatment, d)

    ci <- confint(mod2, "pred")
    type <- attr(ci, "type")
    if (type == "finite") {
      covered <- ci[1] < ti & ti < ci[2]
    } else if (type == "infinite") {
      covered <- TRUE
    } else if (type == "disjoint") {
      covered <- ti < ci[1] | ci[2] < ti
    } else {
      stop(paste("Problem:", type))
    }
    type <- switch(type,
                   finite=1,
                   infinite=2,
                   disjoint=3)

    save[i,] <- c(mod2$coef[2], ci[1], ci[2], type, covered)
  }
  save <- data.frame(save)
  save$type <- as.factor(save$type)
  levels(save$type) <- 1:3

  bytype <- aggregate(save$covered, by=list(save$type), FUN=mean)[,2]
  overall <- mean(save$covered)

  bigsave[j,] <- c(ti, mean(save$estimate),
                   as.vector(table(save$covered, save$type)))

}

bigsave <- as.data.frame(bigsave)
bigsave$overall_un <- apply(bigsave[names(bigsave)[grepl("_un", names(bigsave))]], 1, sum)
bigsave$overall_cov <- apply(bigsave[names(bigsave)[grepl("_cov", names(bigsave))]], 1, sum)

bigsave$overall_per <- bigsave$overall_cov/(bigsave$overall_un + bigsave$overall_cov)
bigsave$finite_per <- bigsave$finite_cov/(bigsave$finite_un + bigsave$finite_cov)
bigsave$inf_per <- bigsave$inf_cov/(bigsave$inf_un + bigsave$inf_cov)
bigsave$disjoint_per <- bigsave$disjoint_cov/(bigsave$disjoint_un + bigsave$disjoint_cov)

bigsave <- bigsave[,c(1,2,9:11,3,4,12,5,6,13,7,8,14)]

xtable(bigsave[,1:5])
xtable(bigsave[,c(1,6:14)])
