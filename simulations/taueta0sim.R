n <- 100
p <- 5
pc <- .4
informative <- .4
true_t <- .5
sigma2 <- 1
true_inter <- seq(-1,2,by = .5)

reps <- 100
bigsave <- pbph:::makeSaveMatrix(c("truth", "estimate", "overall_un",
                            "overall_cov", "cont_un", "cont_cov",
                            "disjoint_un", "disjoint_cov"),
                          reps = length(true_inter))
for (j in 1:length(true_inter)) {
  ti <- true_inter[j]
  save <- pbph:::makeSaveMatrix(c("estimate", "lb", "ub", "shape", "covered", "tau_eta0"),
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

    e <- pbph(mod1, treatment, d)
    ci <- confint(e, "pred", returnShape = TRUE)
    shape <- attr(ci, "shape")
    covered <- ci[1] < ti & ti < ci[2]
    if (shape == "disjoint") {
      covered <- !covered
    }
    shape <- switch(shape,
                    finite = 1,
                    infinite = 2,
                    disjoint = 3)

    save[i,] <- c(e$coef[2], ci[1], ci[2], shape, covered,
                  mean(e$pbph$data$y[e$pbph$data$treatment == 1] - e$pbph$data$pred[e$pbph$data$treatment == 1]))

  }
  save <- data.frame(save)
  save$shape <- as.factor(save$shape)
  levels(save$shape) <- 1:3
  save$covered <- factor(save$covered, levels = 0:1)

  #byshape <- aggregate(save$covered, by = list(save$shape), FUN = mean)[,2]
  overall <- mean(save$covered == 1)

  bigsave[j,] <- c(ti, mean(save$estimate), table(save$covered),
                   as.vector(table(save$covered, save$shape == 1)))

  pdf(file = paste0("byshape_", ti, ".pdf"))
  boxplot(save$tau_eta0 ~ save$shape, xlab = "Shape (1 = Fi, 2 = Inf, 3 = Dis)", ylab = expression(tau[eta[0]]),
          main = paste("true eta = ", ti))
  dev.off()

  pdf(file = paste0("bycov_", ti, ".pdf"))
  boxplot(save$tau_eta0 ~ save$covered, xlab = "Coverage (No/Yes)", ylab = expression(tau[eta[0]]),
          main = paste("true eta = ", ti))
  dev.off()


}

#pdf("~/Desktop/coverage.pdf")
#plot(bigsave[,3]~bigsave[,1], type = 'l', ylim = c(80,100), xlab = "True Eta", ylab = "Coverage")
#abline(h = 95)
#legend("topright", legend = c("Basic", "General", "Simplified"), lty = c(1,1), col = 1:3)
#dev.off()

bigsave
