#' @importFrom rlang enquo
#' @importFrom purrr map_lgl

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines discrim_linear in the model database
  make_discrim_linear_MASS()
  make_discrim_linear_FDA()

  # This defines discrim_regularized in the model database
  make_discrim_regularized()
}
