#' General Interface for Linear Discriminant Models
#'
#' `discrim_linear()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R. The main
#'  arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'   in the model. Note that this only used for the "FDA" engine where it is a
#'   pure L2 penalty (a.k.a ridge regression).
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param penalty An non-negative number representing the
#'  amount of regularization used by some of the engines.
#' @details
#' For `discrim_linear()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"MASS"`(the default) or `"FDA"`
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call.  For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{MASS}
#'
#' \preformatted{
#' MASS::lda(x = missing_arg(), grouping = missing_arg())
#' }
#'
#'
#' \pkg{FDA}
#'
#' \preformatted{
#' mda::fda(formula = missing_arg(), data = missing_arg(), lambda = penalty,
#'          method = mda::gen.ridge, keep.fitted = FALSE)
#' }
#' @examples
#' discrim_linear()
#' @export
discrim_linear <-
  function(mode = "classification",
           penalty = NULL) {

    args <- list(
      penalty = rlang::enquo(penalty)
    )

    new_model_spec(
      "discrim_linear",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.discrim_linear <- function(x, ...) {
  cat("Linear Discriminant Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @param object A linear discriminant model specification.
#' @examples
#' model <- discrim_linear(penalty = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update discrim_linear
#' @rdname discrim_linear
#' @export
update.discrim_linear <-
  function(object,
           penalty = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <- list(
      penalty = rlang::enquo(penalty)
    )

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "discrim_linear",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.discrim_linear <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0)) {
    stop("The amount of regularization should be >= 0", call. = FALSE)
  }

  invisible(object)
}

# ------------------------------------------------------------------------------
# post-processing helpers

post_to_tibble <- function(x, object) {
  probs <- x$posterior
  probs <- tibble::as_tibble(probs)
}

get_class <- function(x, object) {
  x$class
}

prob_matrix_to_tibble <- function(x, object) {
  tibble::as_tibble(x)
}

# ------------------------------------------------------------------------------

# This is needed since parnsip is looking for a funciton like `foo(x, y)` but
# we have `lda(x, grouping)`. It just maps x -> grouping
#' Helper functions for using parsnip with the MASS package
#' @export
#' @keywords internal
#' @rdname mass_helpers
mass_lda_wrapper <- function(x, y, ...) {
  args <- list(x = rlang::enquo(x), grouping = rlang::enquo(y))
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2("lda", .ns = "MASS", !!!args)
  rlang::eval_tidy(cl)
}

