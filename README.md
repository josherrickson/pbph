<!-- README.md is generated from README.Rmd. Please edit that file -->





# pbph: Peters-Belson with Prognostic Heterogeneity implementation for R

[![R-build-check](https://github.com/josherrickson/pbph/workflows/R-build-check/badge.svg)](https://github.com/josherrickson/pbph/actions)

The **pbph** package implements a two-stage variation of the Peters-Belson approach to treatment effect detection in a causal inference
framework. Traditional Peters-Belson methods generate a model to predict the outcome in the absense of treatment, then estimate the treatment effect
by the difference in that predicted outcome and the observed outcome in the treatment group.

The Peters-Belson with Prognostic Heterogeneity (PBPH) method implemented in this package uses the predicted response in the absense of treatment in a
second stage regression model, to allow us to examine whether an individual's predicted risk (their predicted response in the absense of treatment)
affects the treatment effect.

Consider a classroom of students. We are testing whether students enrolled in an after-school program show a performance increase on an end-of-term
standardized exam. It is relatively straightforward to test whether students enrolled in that program show an improvement on their exam
scores. However, we may hypothesize that we would see little-to-no improvement amongst those students who would perform well on the test regardless of
intervention, but will be a large benefit on those students who would perform poorly. The PBPH method addresses this.

## Installation

**pbph** is not yet available on CRAN. To use, please install **devtools** and use `install_github` to obtain a working version.


```r
install.packages("devtools")
devtools::install_github("josherrickson/pbph")
library(pbph)
```

## Usage

A short example. Lets predict the effect of an afterschool program on an end-of-term exam on the `eottest` (fake) data.


```r
data(eottest)
mod1 <- lm(test ~ gpa + male, data = eottest, subset = (afterschool == 0))
mod2 <- pbph(mod1, treatment = afterschool, data = eottest)
summary(mod2)
#>
#> Call:
#> lm(formula = test - pred ~ treatment + pred, data = newdata,
#>     subset = (treatment == 1))
#>
#> Residuals:
#>     Min      1Q  Median      3Q     Max
#> -3.2983 -0.9854 -0.2190  0.9647  3.4119
#>
#> Coefficients:
#>           Estimate Std. Error t value Pr(>|t|)
#> treatment   3.3085     0.2592  12.765   <2e-16 ***
#> pred       -0.4885     0.1137  -2.265   0.0295 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
#> Residual standard error: 1.61 on 38 degrees of freedom
#> Multiple R-squared:   0.91,	Adjusted R-squared:  0.9053
#> F-statistic: 192.1 on 2 and 38 DF,  p-value: < 2.2e-16
confint(mod2)
#>                2.5 %      97.5 %
#> treatment  2.7930010  3.82397466
#> pred      -0.6528415 -0.08992958
```

The negative coefficient (-.799) on `pred` shows evidence that students with lower predicted test score in the absense of treatment are showing a
larger treatment effect than those with higher predicted test scores.

## Development

Included are both a Makefile and an RStudio project file. Either can be used.

After cloning the repo, the first thing to do is ensure all dependencies are installed.

If using RStudio, after loading the project file, run


```r
devtools::install_deps(dependencies = TRUE)
```

If using the Makefile, you can run

```
make dependencies
```

Additional useful Makefile commands include

- `make interactive`: Starts up an interactive session with **pbph** loaded.
- `make test`: Test all testthat files in `tests/testthat` directory.
- `make check`: Runs `R CMD check` on the package.
- `make document`: Convert all inline roxygen documentation into .Rd files, and update NAMESPACE as needed.
- `make vignette`: Builds any vignettes in `vignettes/` directory.
- `make clean`: Removes files built by `make vignette`. Should not be generally necessary, but can be useful for debugging.
