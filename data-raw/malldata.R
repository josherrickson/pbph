set.seed(3)
n <- 238
malldata <- data.frame(coveredgarage = rep(0:1, each = n/2),
                       year = sample(seq(1956, 1997, by = 1), n, TRUE),
                       size = sample(seq(30, 1200, by = 1), n, TRUE))

# Generate relationship between predictors and response in control
pc <- 1 / (1 + exp(scale(malldata$year) - scale(malldata$size)))
# Add main effect and interaction effect
pt <- pc + .1*malldata$coveredgarage - .7*malldata$coveredgarage*pc
pt <- pmax(pmin(pt, 1), 0)

malldata$winterhigher <- rbinom(n, 1, ifelse(malldata$coveredgarage, pt, pc))

devtools::use_data(malldata, overwrite = TRUE)

