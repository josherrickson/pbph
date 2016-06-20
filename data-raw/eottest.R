set.seed(1)
n <- 80
eottest <- data.frame(afterschool = rep(0:1, each = n/2),
                         male = sample(0:1, n, TRUE),
                         gpa = sample(seq(1, 4, by = .1), n, TRUE) )

# Generate relationship between predictors and response in control
yc <- 60 + eottest$gpa*10 - eottest$male
# Add main effect and interaction effect
yt <- yc + 2*eottest$afterschool
yt[eottest$afterschool == 1 & yc < 80] <- yt[eottest$afterschool == 1 & yc < 80] + 7
yt[eottest$afterschool == 1 & yc > 90] <- yt[eottest$afterschool == 1 & yc > 90] - 3
# Response in yt or yc + noise, truncated at 100.
eottest$test <- pmin(100, ifelse(eottest$afterschool, yt, yc) + rnorm(n))
eottest$test <- round(eottest$test)

devtools::use_data(eottest, overwrite = TRUE)

#plot(eottest$test);arrows(1:80, yc, 1:80, eottest$test)
