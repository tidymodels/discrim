#' General Interface for Quadratic Discriminant Models
#'
#' `discrim_quad()` is a way to generate a _specification_ of a quadratic
#'  discriminant analysis (QDA) model before fitting and allows the model to be
#'  created using different packages in R.
#'
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @details
#' For `discrim_quad()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the following
#'  _engines_:

#' \itemize{
#' \item \pkg{R}:  `"MASS"`
#' }
#'
#'
#' This argument is converted to its specific names at the time that the model
#'  is fit. Other options and argument can be set using `set_engine()`. If left
#'  to their defaults here (`NULL`), the values are taken from the underlying
#'  model functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @includeRmd man/rmd/discrim-quad-engine.Rmd
#'
#' @examples
#' parabolic_grid <-
#'   expand.grid(X1 = seq(-5, 5, length = 100),
#'               X2 = seq(-5, 5, length = 100))
#'
#' qda_mod <-
#'   discrim_quad() %>%
#'   set_engine("MASS") %>%
#'   fit(class ~ ., data = parabolic)
#'
#' parabolic_grid$qda <-
#'   predict(qda_mod, parabolic_grid, type = "prob")$.pred_Class1
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2)) +
#'   geom_point(aes(col = class), alpha = .5) +
#'   geom_contour(data = parabolic_grid, aes(z = qda), col = "black", breaks = .5) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   coord_equal()
#' @export
discrim_quad <-
  function(mode = "classification") {

    args <- list()

    parsnip::new_model_spec(
      "discrim_quad",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.discrim_quad <- function(x, ...) {
  cat("Quadratic Discriminant Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @inheritParams update.discrim_flexible
#' @param object A quadratic discriminant model specification.
#' @method update discrim_quad
#' @rdname discrim_quad
#' @export
update.discrim_quad <-
  function(object,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list()

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
      "discrim_quad",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }
