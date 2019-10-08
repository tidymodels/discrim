#' General Interface for Naive Bayes Models
#'
#' `naive_Bayes()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using different packages in R.
#'
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param smoothness An non-negative number representing the the relative
#'  smoothness of the class boundary. Smaller examples result in model flexible
#'  boundaries and larger values generate class boundaries that are less
#'  adaptable
#' @param Laplace A non-negative value for the Laplace correction to smoothing
#' low-frequency counts.
#' @details
#'
#' The main
#'  arguments for the model are:
#' \itemize{
#'   \item \code{smoothness}: The total amount of regularization
#'   in the model. Note that this only used for the "klaR" engine where it is a
#'   pure L2 smoothness (a.k.a ridge regression).
#'   \item \code{Laplace}: Laplace correction for smoothing low-frequency counts.
#' }
#' These arguments are converted to their specific names at the time that the
#'  model is fit. Other options and argument can be set using `set_engine()`. If
#'  left to their defaults here (`NULL`), the values are taken from the
#'  underlying model functions. If parameters need to be modified, `update()`
#'  can be used in lieu of recreating the object from scratch.
#'
#' For `naive_Bayes()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"klaR"`(the default)
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the model fit call.
#' For this type of model, the template of the fit calls are:
#'
#' \pkg{klaR} engine
#'
#' \preformatted{
#' klaR::NaiveBayes(x = missing_arg(), grouping = missing_arg(),
#'                  adjust = 0.8, usekernel = TRUE)
#' }
#'
#' Note that `usekernel` is always set to `TRUE` here. This model does not
#'  need to make dummy variables from factor predictors. However, if
#'  `parsnip::fit()` is used to fit the model, dummy variables _would_ be
#'  created while `parsnip::fit_xy()` will preserve the factor predictors in
#'  their original encoding.
#'
#' @examples
#' parabolic_grid <-
#'   expand.grid(X1 = seq(-5, 5, length = 100),
#'               X2 = seq(-5, 5, length = 100))
#'
#' nb_mod <-
#'   naive_Bayes(smoothness = .8) %>%
#'   set_engine("klaR") %>%
#'   fit(class ~ ., data = parabolic)
#'
#' parabolic_grid$nb <-
#'   predict(nb_mod, parabolic_grid, type = "prob")$.pred_Class1
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2)) +
#'   geom_point(aes(col = class), alpha = .5) +
#'   geom_contour(data = parabolic_grid, aes(z = nb), col = "black", breaks = .5) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   coord_equal()
#' @export
naive_Bayes <-
  function(mode = "classification", smoothness = NULL, Laplace = NULL) {
    args <-
      list(
        smoothness = rlang::enquo(smoothness),
        Laplace = rlang::enquo(Laplace)
      )

    new_model_spec(
      "naive_Bayes",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.naive_Bayes <- function(x, ...) {
  cat("Naive Bayes Model Specification (", x$mode, ")\n\n", sep = "")
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
#' model <- naive_Bayes(smoothness = 0.1)
#' model
#' update(model, smoothness = 1)
#' update(model, smoothness = 1, fresh = TRUE)
#' @method update naive_Bayes
#' @rdname naive_Bayes
#' @export
update.naive_Bayes <-
  function(object,
           smoothness = NULL, Laplace = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <-
      list(
        smoothness = rlang::enquo(smoothness),
        Laplace = rlang::enquo(Laplace)
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
      "naive_Bayes",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname discrim_helpers
klar_bayes_wrapper <- function(x, y, ...) {
  args <- list(x = rlang::enquo(x), grouping = rlang::enquo(y))
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2("NaiveBayes", .ns = "klaR", !!!args)
  rlang::eval_tidy(cl)
}
