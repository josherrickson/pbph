

f <- function(t, e) {
  yc <- seq(0, 1, length = 100)
  yt <- t + (1 + e) * yc
  out <- table(yt >= 0 & yt <= 1)["TRUE"]
  ifelse(is.na(out), 0, out)
}

eta <- seq(2.5, -2.5, by = -.1)
tau <- seq(-2.5, 2.5, by = .1)
n <- length(eta)
save <- matrix(nrow = n, ncol = n)
# tau col -1 ->  1, i
# eta row  1 -> -1, j
for (i in 1:n) {
  for (j in 1:n) {
    save[j,i] <- f(tau[i], eta[j])
  }
}
save <- save/100
rownames(save) <- eta
colnames(save) <- tau

plotrix::color2D.matplot(save, c(1,0), c(0,0), c(0,1), show.legend = TRUE,
                         xlab = "tau", ylab = "eta", axes = FALSE)
axis(1, at = quantile(1:n),labels = round(quantile(eta), 1))
axis(2, at = quantile(1:n),labels = round(quantile(eta), 1))

plotrix::color2D.matplot(0 + (save > 0), c(1,0), c(0,0), c(0,1), show.legend = TRUE,
                         xlab = "tau", ylab = "eta", axes = FALSE)
axis(1, at = quantile(1:n),labels = round(quantile(eta), 1))
axis(2, at = quantile(1:n),labels = round(quantile(eta), 1))

plot(NULL, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), xlab = "tau", ylab = "eta")
polygon(c(1, 1, -4), c(3, -2, 3), col = 'lightblue')
polygon(c(0, 3, 0), c(0, -3, -3), col = 'lightgreen')
polygon(c(0, 1, 1, 0), y = c(0, -1, -2, -1), col = 'red')

points(bigsave[,1:2])

