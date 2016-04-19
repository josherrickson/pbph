<!-- README.md is generated from README.Rmd. Please edit that file -->
epb: Enhanced Peters-Belson implementation for R
================================================

[![Travis-CI Build Status](https://travis-ci.org/josherrickson/epb.svg?branch=master)](https://travis-ci.org/josherrickson/epb) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/josherrickson/epb?branch=master&svg=true)](https://ci.appveyor.com/project/josherrickson/epb) [![Coverage Status](https://img.shields.io/codecov/c/github/josherrickson/epb/master.svg)](https://codecov.io/github/josherrickson/epb?branch=master) [![Coverage Status](https://img.shields.io/coveralls/josherrickson/epb.svg)](https://coveralls.io/r/josherrickson/epb?branch=master)

The `epb` package implements a two-stage variation of the Peters-Belson approach to treatment effect detection in a causal inference framework. Traditional Peters-Belson methods generate a model to predict the outcome in the absense of treatment, then estimate the treatment effect by the difference in that predicted outcome and the observed outcome in the treatment group.

The enhanced version implemented in this package uses the predicted response in the absense of treatment in a second stage regression model, to allow us to examine whether an individual's predicted risk (their predicted response in the absense of treatment) affects the treatment effect.

Consider a classroom of students. We are testing whether students enrolled in an after-school program show a performance increase on an end-of-term standardized exam. It is relatively straightforward to test whether students enrolled in that program show an improvement on their exam scores. However, we may hypothesize that we would see little-to-no improvement amongst those students who would perform well on the test regardless of intervention, but will be a large benefit on those students who would perform poorly. The enchanced Peters-Belson method addresses this.

Installation
------------

`epb` is not yet available on CRAN. To use, please install `devtools` and use `install_github` to obtain a working version.

``` r
install.packages("devtools")
devtools::install_github("josherrickson/epb")
library(epb)
```

Usage
-----

A short example. Lets predict the effect of an afterschool program on an end-of-term exam on the `eottest` (fake) data.

``` r
data(eottest)
mod1 <- lm(test ~ gpa + male, data = eottest, subset = (afterschool == 0))
mod2 <- pblm(mod1, treatment = eottest$afterschool, data = eottest)
summary(mod2)
#> 
#> Call:
#> lm(formula = test_t - pred ~ treatment + pred, data = newdata)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -9.1368 -1.0697 -0.1559  2.2389  2.7636 
#> 
#> Coefficients:
#>           Estimate Std. Error t value Pr(>|t|)    
#> treatment 14.32344    0.57302  24.996  < 2e-16 ***
#> pred      -0.79942    0.08846  -5.354 5.26e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.697 on 18 degrees of freedom
#> Multiple R-squared:  0.9766, Adjusted R-squared:  0.974 
#> F-statistic: 375.1 on 2 and 18 DF,  p-value: 2.131e-15
confint(mod2)
#>                2.5 %    97.5 %
#> treatment 13.0566651 15.590208
#> pred      -0.9788453 -0.591279
```

The negative coefficient (-.799) on `pred` shows evidence that students with lower predicted test score in the absense of treatment are showing a larger treatment effect than those with higher predicted test scores.

Development
-----------

Included are both a Makefile and an RStudio project file. Either can be used.

After cloning the repo, the first thing to do is ensure all dependencies are installed.

If using RStudio, after loading the project file, run

``` r
devtools::install_deps(dependencies = TRUE)
```

If using the Makefile, you can run

    make dependencies

Additional useful Makefile commands include

-   `make interactive`: Starts up an interactive session with `epb` loaded.
-   `make test`: Test all testthat files in `tests/testthat` directory.
-   `make check`: Runs `R CMD check` on the package.
-   `make document`: Convert all inline roxygen documentation into .Rd files, and update NAMESPACE as needed.
-   `make vignette`: Builds any vignettes in `vignettes/` directory.
-   `make clean`: Removes files built by `make vignette`. Should not be generally necessary, but can be useful for debugging.
