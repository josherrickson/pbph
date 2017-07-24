set.seed(3)
n <- 238
salesdata <- data.frame(newtechnique = rep(0:1, each = n/2),
                        experience = round(abs(rnorm(n, sd = 7))),
                        previoussales = sample(1:25, size = n, replace = TRUE))

# Generate relationship between predictors and response in control
pc <- 1 / (1 + exp(scale(salesdata$experience) - scale(salesdata$previoussales)))
# Add main effect and interaction effect
pt <- pc + .1*salesdata$newtechnique - .7*salesdata$newtechnique*pc
pt <- pmax(pmin(pt, 1), 0)

salesdata$sale <- rbinom(n, 1, ifelse(salesdata$newtechnique, pt, pc))
