#' @importFrom rlang enquo
#' @importFrom purrr map_lgl
#' @importFrom tibble is_tibble as_tibble
#' @importFrom parsnip set_new_model
#' @importFrom withr with_options
#' @importFrom stats predict
#' @importFrom dials new_quant_param

# ------------------------------------------------------------------------------

utils::globalVariables(c("x", "y"))

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines discrim_linear in the model database
  make_discrim_linear_MASS()
  make_discrim_linear_mda()
  make_discrim_linear_sda()
  make_discrim_linear_sparsediscrim()

  # This defines discrim_quad in the model database
  make_discrim_quad_MASS()

  # This defines discrim_regularized in the model database
  make_discrim_regularized()

  # This defines discrim_flexible in the model database
  make_discrim_flexible()

  # This defines naive_Bayes in the model database
  make_naive_Bayes_klaR()
  make_naive_Bayes_naivebayes()
}
