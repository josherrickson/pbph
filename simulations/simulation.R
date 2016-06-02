n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by = .5)

reps <- 100
bigsave <- epb:::makeSaveMatrix(c("truth", "estimate", "overall_un",
                            "overall_cov", "cont_un", "cont_cov",
                            "disjoint_un", "disjoint_cov"),
                          reps = length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- epb:::makeSaveMatrix(c("estimate", "lb", "ub", "type", "covered"),
                         reps)
  for (i in 1:reps) {
    covs <- data.frame(matrix(rnorm(n*p), nrow = n))
    truebeta <- rep(0, p)
    truebeta[sample(1:p, round(informative*p))] <- rnorm(round(informative*p),0,1)

    treatment <- rep(0:1, c(n*pc, n*(1 - pc)))

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
    type <- switch(type,
                   finite = 1,
                   infinite = 1,
                   disjoint = 2)

    save[i,] <- c(e$coef[2], ci[1], ci[2], type, covered)

  }
  save <- data.frame(save)
  save$type <- as.factor(save$type)
  levels(save$type) <- 1:2
  save$covered <- factor(save$covered, levels = 0:1)

  #bytype <- aggregate(save$covered, by = list(save$type), FUN = mean)[,2]
  overall <- mean(save$covered == 1)

  bigsave[j,] <- c(ti, mean(save$estimate), table(save$covered),
                   as.vector(table(save$covered, save$type)))

}

#pdf("~/Desktop/coverage.pdf")
#plot(bigsave[,3]~bigsave[,1], type = 'l', ylim = c(80,100), xlab = "True Eta", ylab = "Coverage")
#abline(h = 95)
#legend("topright", legend = c("Basic", "General", "Simplified"), lty = c(1,1), col = 1:3)
#dev.off()

bigsave
