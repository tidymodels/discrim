#' General Interface for Linear Discriminant Models
#'
#' `discrim_linear()` is a way to generate a _specification_ of a linear
#'  discriminant analysis (LDA) model before fitting and allows the model to be
#'  created using different packages in R.
#'
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param penalty An non-negative number representing the amount of
#'  regularization used by some of the engines.
#' @details
#' For `discrim_linear()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the following
#'  _engines_:

#' \itemize{
#' \item \pkg{R}:  `"MASS"`(the default) or `"mda"`
#' }
#'
#' The main argument for the model is:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization in the model. Note
#'    that this only used for the "mda" engine where it is a pure L2 penalty
#'   (a.k.a ridge regression).
#' }
#'
#' This argument is converted to its specific names at the time that the model
#'  is fit. Other options and argument can be set using `set_engine()`. If left
#'  to their defaults here (`NULL`), the values are taken from the underlying
#'  model functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @includeRmd man/rmd/discrim-lin-engine.Rmd
#'
#' @examples
#' parabolic_grid <-
#'   expand.grid(X1 = seq(-5, 5, length = 100),
#'               X2 = seq(-5, 5, length = 100))
#'
#' lda_mod <-
#'   discrim_linear(penalty = .1) %>%
#'   set_engine("mda") %>%
#'   fit(class ~ ., data = parabolic)
#'
#' parabolic_grid$lda <-
#'   predict(lda_mod, parabolic_grid, type = "prob")$.pred_Class1
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2)) +
#'   geom_point(aes(col = class), alpha = .5) +
#'   geom_contour(data = parabolic_grid, aes(z = lda), col = "black", breaks = .5) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   coord_equal()
#' @export
discrim_linear <-
  function(mode = "classification",
           penalty = NULL) {

    args <- list(
      penalty = rlang::enquo(penalty)
    )

    parsnip::new_model_spec(
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
    print(parsnip::show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @inheritParams update.discrim_flexible
#' @param object A linear discriminant model specification.
#' @examples
#'
#'
#' model <- discrim_linear(penalty = 0.1)
#' model
#' update(model, penalty = 1)
#' @method update discrim_linear
#' @rdname discrim_linear
#' @export
update.discrim_linear <-
  function(object,
           penalty = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list(
      penalty = rlang::enquo(penalty)
    )

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, parsnip::null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    parsnip::new_model_spec(
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
