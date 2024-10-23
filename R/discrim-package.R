#' parsnip methods for discriminant analysis
#'
#' \pkg{discrim} offers various functions to fit classification models via the
#' discriminant analysis.
#'
#' The model function works with the tidymodels infrastructure so that the model
#' can be resampled, tuned, tided, etc.
#'
#' @includeRmd man/rmd/example.Rmd details
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats predict
#' @importFrom parsnip set_new_model
## usethis namespace: end
NULL

# Global vars ------------------------------------------------------------------

utils::globalVariables(c("x", "y"))

