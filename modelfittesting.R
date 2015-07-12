rm(list=ls())
x1 <- rnorm(10)
x2 <- rnorm(10)
y <- x1+x2+rnorm(10)
d <- data.frame(y,x1,x2)
rm(y,x1,x2)
treatment <- rep(0:1, each=5)

m <- modfit(y ~ ., treatment, data=d)
e <- epb(y ~ . , treatment, d)

summary(m$mod1)
e

r2 <- summary(m$mod1)$r.sq

1 - (1 - r2)*(10-1)/(10-2-1)

anova(m$mod1)
