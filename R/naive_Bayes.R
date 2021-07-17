#' Naive Bayes models
#'
#' `naive_Bayes()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using different packages in R.
#'
#' @inheritParams discrim_linear
#' @param smoothness An non-negative number representing the the relative
#'  smoothness of the class boundary. Smaller examples result in model flexible
#'  boundaries and larger values generate class boundaries that are less
#'  adaptable
#' @param Laplace A non-negative value for the Laplace correction to smoothing
#' low-frequency counts.
#'
#' @template spec-details
#'
#' @template spec-references
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
  function(mode = "classification", engine = "klaR", smoothness = NULL, Laplace = NULL) {
    args <-
      list(
        smoothness = rlang::enquo(smoothness),
        Laplace = rlang::enquo(Laplace)
      )

    parsnip::new_model_spec(
      "naive_Bayes",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
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
#'
#'
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
    parsnip::update_dot_check(...)
    args <-
      list(
        smoothness = rlang::enquo(smoothness),
        Laplace = rlang::enquo(Laplace)
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
      "naive_Bayes",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

