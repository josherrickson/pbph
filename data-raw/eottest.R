set.seed(1)
n <- 40
eottest <- data.frame(afterschool = rep(0:1, each = n/2),
                         male = sample(0:1, n, TRUE),
                         gpa = sample(seq(2.5, 4, by = .1), n, TRUE) )

# Generate relationship between predictors and response in control
yc <- eottest$gpa*25
# Add main effect and interaction effect
yt <- yc + eottest$afterschool*1 + eottest$afterschool*yc*.3
# Response in yt or yc + noise, truncated at 100.
eottest$test <- pmin(100, ifelse(eottest$afterschool, yt, yc) + rnorm(n))

devtools::use_data(eottest, overwrite=TRUE)
