#' Quadratic discriminant analysis
#'
#' @description
#'
#' `discrim_quad()` defines a model that estimates a multivariate
#'  distribution for the predictors separately for the data in each class
#'  (usually Gaussian with separate covariance matrices). Bayes' theorem is used
#'  to compute the probability of each class, given the predictor values.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_quad", "discrim")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine to use
#'  for fitting.
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`shrink_cov`", and
#'  "`shrink_mean`" (`sparsediscrim` engine only).
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_quad", "discrim")}
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
  function(mode = "classification", engine = "MASS", regularization_method = NULL) {

    args <- list(regularization_method = rlang::enquo(regularization_method))

    parsnip::new_model_spec(
      "discrim_quad",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
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

#' @method update discrim_quad
#' @rdname discrim_update
#' @inheritParams discrim_quad
#' @export
update.discrim_quad <-
  function(object,
           regularization_method = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list(regularization_method = rlang::enquo(regularization_method))

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
