# Changelog

## discrim (development version)

## discrim 1.0.2

CRAN release: 2025-08-23

- Updated document linking to stay on CRAN.

## discrim 1.0.1

CRAN release: 2023-03-08

- Updated <Authors@R>.

## discrim 1.0.0

CRAN release: 2022-06-23

- Case weights were enabled for \[discrim_flexible()\] and
  \[discrim_linear()\] (`"mda"` engine).

## discrim 0.2.0

CRAN release: 2022-03-09

- Model definition functions
  (e.g. [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html))
  were moved to the parsnip package.

## discrim 0.1.3

CRAN release: 2021-07-21

- An LDA engine was added for the shrunken discriminant analysis method
  of Ahdesmaki and Strimmer (2010) with `engine = "sda"`.

- LDA and QDA models now have an engine that fits several regularized
  discriminant models from the `sparsediscrim` package.

## discrim 0.1.2

CRAN release: 2021-05-28

- Added
  [`discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html)

- Fixed a bug in the parameter definitions
  ([\#19](https://github.com/tidymodels/discrim/issues/19))

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/discrim/issues/22).

## discrim 0.1.1

CRAN release: 2020-10-28

- Small updates so that `discrim` can be run in parallel using psock
  clusters ([\#13](https://github.com/tidymodels/discrim/issues/13))

- Updates for encoding requirements related to current version of
  `parsnip`.

## discrim 0.1.0

CRAN release: 2020-07-07

- Small updates to work with new `parsnip` version.

## discrim 0.0.2

CRAN release: 2020-04-09

- Added `naivebayes` engine for
  [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  ([\#5](https://github.com/tidymodels/discrim/issues/5)).

- Change for new `parsnip` version 0.1.0.

## discrim 0.0.1

CRAN release: 2019-10-11

First CRAN version
