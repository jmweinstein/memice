
# memice

<img src="https://github.com/jmweinstein/hex/blob/main/memice_hexsticker.png?raw=true" align="right" width="162"/>

`memice` is a package with several functions to facilitate the
estimation of marginal effects in linear and generalized linear models.
It takes a model object (`lm()` or `glm()` objects) and a `.mids` object
(a data object containing data multiply imputed by `mice::mice()`),
estimates the marginal effects in each dataset within the `.mids`
object, and pools the marginal effect estimates according to Rubin’s
Rules. This package takes inspiration from Daniel Klein’s mimrgns
program for Stata.

## Installation

You can install the development version of memice like so:

``` r
devtools::install_github("jmweinstein/memice")
```

## Recommended workflow

The recommended workflow for using all `memice` functions is:

1.  Create your desired model with the dataset that contains missing
    data
2.  Use `mice::mice()` to impute data via chained equations and save to
    an object
3.  Call `memice::initialize_memice()` to initialize your workspace for
    using memice (this adds formatting functions to your workspace,
    which are necessary for `memice` properly handling different
    variable types)
4.  Use one of the `memice` functions, feeding in your model from Step 1
    and your `.mids` object (from `mice::mice()`) from Step 2

Model estimation is done prior to the use of `memice` functions to
ensure that all quirks with models have been figured out before being
fed into any of the `memice` functions. Things that may cause issues
with `memice` functions pooling marginal effects estimates include
non-convergence of generalized linear models or improperly-formatted
variables (numbers that should be factors, factors that should be
numeric, etc.)- by estimating the model before using the `memice`
functions, the user has the opportunity to identify and remedy these
issues beforehand.

## Example usage of `memice::ame_mice()`

The function `memice::ame_mice()` estimates average marginal effects
(AME) in multiply imputed data created by `mice::mice()`.

``` r
library(memice)
library(missMethods)


## Create dataset with artificially-induced missingness in 'cyl' variable
data_with_missings <- mtcars |>
  missMethods::delete_MCAR(0.3, "cyl") |>
  dplyr::mutate(cyl = as.factor(cyl))
```

Create your desired model with the dataset that contains missing data

``` r
lm_mod <- lm(formula = mpg ~ wt + hp + cyl + cyl*wt + disp,
             data = data_with_missings)
```

Create `.mids` object of imputed data using `mice::mice()`

``` r
imputation <- mice::mice(data = data_with_missings, m = 5, seed = 01701)
#> 
#>  iter imp variable
#>   1   1  cyl
#>   1   2  cyl
#>   1   3  cyl
#>   1   4  cyl
#>   1   5  cyl
#>   2   1  cyl
#>   2   2  cyl
#>   2   3  cyl
#>   2   4  cyl
#>   2   5  cyl
#>   3   1  cyl
#>   3   2  cyl
#>   3   3  cyl
#>   3   4  cyl
#>   3   5  cyl
#>   4   1  cyl
#>   4   2  cyl
#>   4   3  cyl
#>   4   4  cyl
#>   4   5  cyl
#>   5   1  cyl
#>   5   2  cyl
#>   5   3  cyl
#>   5   4  cyl
#>   5   5  cyl
```

Initialize `memice` using `initialize_memice`, then estimate and pool
marginal effects across the imputed datasets

``` r
memice::initialize_memice()
pooled_mfx <- memice::ame_mice(model = lm_mod, mice_object = imputation)
pooled_mfx
#>   term contrast     estimate  std.error  statistic     p.value
#> 1   wt    dY/dX -3.454409623 1.12719656 -3.0646027 0.002190045
#> 2   hp    dY/dX -0.022379412 0.01171196 -1.9108176 0.056036848
#> 3  cyl    6 - 4 -1.186579675 1.84860872 -0.6418771 0.520971953
#> 4  cyl    8 - 4 -1.611105025 2.64171172 -0.6098716 0.541949282
#> 5 disp    dY/dX -0.002238081 0.01201281 -0.1863078 0.852204214
```

## Acknowledgements

I thank both Dr. Vincent Arel-Bundock and Dr. Noah Greifer for their
technical assistance in developing this package. This package takes
inspiration from Daniel Klein’s mimrgns program for Stata.
