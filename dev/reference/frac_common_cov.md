# Parameter objects for Regularized Discriminant Models

[`discrim_regularized()`](https://parsnip.tidymodels.org/reference/discrim_regularized.html)
describes the effect of `frac_common_cov()` and `frac_identity()`.
`smoothness()` is an alias for the `adjust` parameter in
[`stats::density()`](https://rdrr.io/r/stats/density.html).

## Usage

``` r
frac_common_cov(range = c(0, 1), trans = NULL)

frac_identity(range = c(0, 1), trans = NULL)

smoothness(range = c(0.5, 1.5), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::log10_trans()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::reciprocal_trans()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A function with classes "quant_param" and "param"

## Details

These parameters can modulate a RDA model to go between linear and
quadratic class boundaries.

## Examples

``` r
frac_common_cov()
#> Fraction of the Common Covariance Matrix (quantitative)
#> Range: [0, 1]
```
