#' @importFrom rlang enquo
#' @importFrom purrr map_lgl

# ------------------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # This defines discrim_linear in model database
  make_discrim_linear_MASS()
  make_discrim_linear_FDA()
  make_discrim_regularized()
}
