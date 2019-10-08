#' Parameter objects for Regularized Discriminant Models
#'
#' `discrim_regularized()` describes the effect of these two parameters.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#' largest possible values, respectively.
#'
#' @param trans A `trans` object from the `scales` package, such as
#' `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#' the default is used which matches the units used in `range`. If no
#' transformation, `NULL`.
#'
#' @return A function with classes "quant_param" and "param"
#'
#' @details
#' These parameters can modulate a RDA model to go between linear and quadratic
#' class boundaries.
#' @examples
#' frac_common_cov()
#' @export
frac_common_cov <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
    label = c(threshold = "Fraction of the Common Covariance Matrix"),
    finalize = NULL
  )
}


#' @export
#' @rdname frac_common_cov
frac_identity <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
    label = c(threshold = "Fraction of the Identity Matrix"),
    finalize = NULL
  )
}
