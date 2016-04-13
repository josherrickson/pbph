---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# epb: Enhanced Peters-Belson implementation for R

The `epb` package implements a two-stage variation of the Peters-Belson approach to treatment effect detection in a causal inference
framework. Traditional Peters-Belson methods generate a model to predict $\hat{Y}_c$, the predicted outcome in the absense of treatment, then estimate
the average $Y - \hat{Y}_c$ in the treatment group to obtain a treatment effect estimate (the ETT, or effect of treatment on the treated, to be
precise).

The enhanced version of this uses $\hat{Y}_c$ in a second stage regression model, to allow us to answer whether an individual's predicted risk (their
predicted response in the absense of treatment) affects the treatment effect.

Consider a classroom of students. We are testing whether students enrolled in an after-school program show a performance increase on an end-of-term
standardized exam. It is relatively straightforward to test whether students enrolled in that program show an improvement on their exam
scores. However, we may hypothesize that we would see little-to-no improvement amongst those students who would perform well on the test regardless of
intervention, but will be a large benefit on those students who would perform poorly. The enchanced Peters-Belson method addresses this.

`epb` is not yet available on CRAN. To use, please install `devtools` and use `install_github` to obtain a working version.


```r
install.packages("devtools")
devtools::install_github("josherrickson/epb")
```

## Usage

A short example. Lets predict the effect of `Treatment == "chilled"` on `uptake` in this CO2 data set.


```r
data(CO2)
mod1 <- lm(uptake ~ conc + Type, data = CO2, subset = (Treatment == "nonchilled"))
mod2 <- pblm(mod1, treatment = (CO2$Treatment == "chilled"), data = CO2)
summary(mod2)
#> 
#> Call:
#> lm(formula = uptake_t - pred ~ treatment + pred, data = newdata)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -16.8462  -3.2198   0.1601   4.0107  13.1441 
#> 
#> Coefficients:
#>           Estimate Std. Error t value Pr(>|t|)    
#> treatment  -6.8595     1.4579  -4.705 2.54e-06 ***
#> pred        0.1463     0.5508   0.304    0.762    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 6.645 on 40 degrees of freedom
#> Multiple R-squared:  0.5343,	Adjusted R-squared:  0.511 
#> F-statistic: 22.95 on 2 and 40 DF,  p-value: 2.3e-07
confint(mod2, parm="pred")
#>          2.5 %     97.5 %
#> pred -69.45056 -0.4285882
#> attr(,"type")
#> [1] "disjoint"
```

## Development

Included are both a Makefile and an RStudio project file. Either can be used.

If using the Makefile, useful commands include


- `make interactive`: Starts up an interactive session with `epb` loaded.
- `make test`: Test all testthat files in `tests/testthat` directory.
- `make check`: Runs `R CMD check` on the package.
- `make document`: Convert all inline roxygen documentation into .Rd files, and update NAMESPACE as needed.
- `make vignette`: Builds any vignettes in `vignettes/` directory.
- `make clean`: Removes files built by `make vignette`, `make document` or `make check`.  Should not be generally necessary, but can be useful for
   debugging.
