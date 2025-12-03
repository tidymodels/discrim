# Wrapper for sparsediscrim models

Wrapper for sparsediscrim models

## Usage

``` r
fit_regularized_linear(
  x,
  y,
  regularization_method = "diagonal",
  ...,
  call = rlang::caller_env()
)

fit_regularized_quad(
  x,
  y,
  regularization_method = "diagonal",
  ...,
  call = rlang::caller_env()
)
```

## Arguments

- x:

  A matrix or data frame.

- y:

  A factor column.

- regularization_method:

  A character string.

- ...:

  Not currently used.

## Value

A sparsediscrim object
