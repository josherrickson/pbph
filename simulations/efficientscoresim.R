n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by = .5)

reps <- 100
bigsave1 <- pbph:::makeSaveMatrix(c("truth", "estimate", "overall_un",
                                    "overall_cov", "cont_un", "cont_cov",
                                    "disjoint_un", "disjoint_cov", "medwidth"),
                                  reps = length(true_inter))
bigsave2 <- pbph:::makeSaveMatrix(c("truth", "estimate", "overall_un",
                                    "overall_cov", "cont_un", "cont_cov",
                                    "disjoint_un", "disjoint_cov", "medwidth"),
                                  reps = length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save1 <- pbph:::makeSaveMatrix(c("estimate", "lb", "ub", "type", "covered", "width"),
                                 reps)
  save2 <- pbph:::makeSaveMatrix(c("estimate", "lb", "ub", "type", "covered", "width"),
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

    e1 <- pbph(mod1, treatment, d)
    ci1 <- confint(e1, "pred", returnShape = TRUE)
    type1 <- attr(ci1, "shape")
    covered1 <- ci1[1] < ti & ti < ci1[2]
    if (type1 == "disjoint") {
      covered1 <- !covered1
    }
    type1 <- switch(type1,
                   finite = 1,
                   infinite = 1,
                   disjoint = 2)
    width1 <- diff(ci1[1,])

    save1[i,] <- c(e1$coef[2], ci1[1], ci1[2], type1, covered1, width1)


    e2 <- pbph(mod1, treatment, d, efficientScore = TRUE)
    ci2 <- confint(e2, "pred", returnShape = TRUE)
    type2 <- attr(ci2, "shape")
    covered2 <- ci2[1] < ti & ti < ci2[2]
    if (type2 == "disjoint") {
      covered2 <- !covered2
    }
    type2 <- switch(type2,
                   finite = 1,
                   infinite = 1,
                   disjoint = 2)
    width2 <- diff(ci2[1,])

    save2[i,] <- c(e2$coef[2], ci2[1], ci2[2], type2, covered2, width2)
  }
  save1 <- data.frame(save1)
  save1$type <- as.factor(save1$type)
  levels(save1$type) <- 1:2
  save1$covered <- factor(save1$covered, levels = 0:1)

  #bytype <- aggregate(save$covered, by = list(save$type), FUN = mean)[,2]
  overall1 <- mean(save1$covered == 1)

  bigsave1[j,] <- c(ti, mean(save1$estimate), table(save1$covered),
                   as.vector(table(save1$covered, save1$type)),
                   median(save1$width))


  save2 <- data.frame(save2)
  save2$type <- as.factor(save2$type)
  levels(save2$type) <- 1:2
  save2$covered <- factor(save2$covered, levels = 0:1)

  #bytype <- aggregate(save$covered, by = list(save$type), FUN = mean)[,2]
  overall2 <- mean(save2$covered == 1)

  bigsave2[j,] <- c(ti, mean(save2$estimate), table(save2$covered),
                   as.vector(table(save2$covered, save2$type)),
                   median(save2$width))

}

#pdf("~/Desktop/coverage.pdf")
#plot(bigsave[,3]~bigsave[,1], type = 'l', ylim = c(80,100), xlab = "True Eta", ylab = "Coverage")
#abline(h = 95)
#legend("topright", legend = c("Basic", "General", "Simplified"), lty = c(1,1), col = 1:3)
#dev.off()

bigsave1
bigsave2
