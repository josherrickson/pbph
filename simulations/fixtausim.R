#start from simulations/simulation.R
load("simulations/counterexample.Rdata")
object <- e
bAndM <- pbph:::createBreadAndMeat(object, object$pbph$cluster)

tosolve <- function(eta) {
  pbph:::corrVar(eta, object, bAndM)[2,2]
}

x <- -1:1
t <- sapply(x,tosolve)
dmat <- cbind(1, x, (x)^2)
coefs_mid <- solve(t(dmat) %*% dmat) %*% t(dmat) %*% t

x <- -8:-10
t <- sapply(x,tosolve)
dmat <- cbind(1, x, (x)^2)
coefs_left <- solve(t(dmat) %*% dmat) %*% t(dmat) %*% t

x <- 8:10
t <- sapply(x,tosolve)
dmat <- cbind(1, x, (x)^2)
coefs_right <- solve(t(dmat) %*% dmat) %*% t(dmat) %*% t

cbind(coefs_left,coefs_mid,coefs_right)

plot(function(x) coefs_mid[1] + coefs_mid[2]*x + coefs_mid[3]*x^2, -5, 5, xlim = c(-10,10), ylim = c(0,400))
plot(function(x) coefs_left[1] + coefs_left[2]*x + coefs_left[3]*x^2, -10, -5, add = TRUE, col = 'red')
plot(function(x) coefs_right[1] + coefs_right[2]*x + coefs_right[3]*x^2, 5, 10, add = TRUE, col = 'blue')
