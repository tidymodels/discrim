#' General Interface for Flexible Discriminant Models
#'
#' `discrim_flexible()` is a way to generate a _specification_ of a flexible
#'  discriminant model using features created using multivariate adaptive
#'  regression splines (MARS).
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @details
#' Flexible discriminant analysis (FDA) uses the work of Hastie et al (1994)
#'  to create a discriminant model using different feature expansions. For this
#'  function, MARS (Friedman, 1991) hinge functions are used to nonlinearly
#'  model the class boundaries (see example below). The \pkg{mda} and
#'  \pkg{earth} packages are needed to fit this model.
#'
#' The main arguments for the model are:
#' \itemize{
#'   \item \code{num_terms}: The number of features that will be retained in the
#'    final model.
#'   \item \code{prod_degree}: The highest possible degree of interaction between
#'    features. A value of 1 indicates and additive model while a value of 2
#'    allows, but does not guarantee, two-way interactions between features.
#'   \item \code{prune_method}: The type of pruning. Possible values are listed
#'    in `?earth`.
#' }
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' The model can be created using the `fit()` function using the following
#'  _engines_:
#' \itemize{
#' \item \pkg{R}:  `"earth"`  (the default)
#' }
#'
#' @includeRmd man/rmd/discrim-flexible-engine.Rmd
#'
#' @references
#' Friedman (1991), Multivariate Adaptive Regression Splines (with discussion),
#' _Annals of Statistics_ 19:1, 1–141.
#' Hastie, Tibshirani and Buja (1994), Flexible Discriminant Analysis by Optimal
#' Scoring, _Journal of the American Statistical Association_, 1255-1270.
#'
#' @examples
#' parabolic_grid <-
#'   expand.grid(X1 = seq(-5, 5, length = 100),
#'               X2 = seq(-5, 5, length = 100))
#'
#' fda_mod <-
#'   discrim_flexible(num_terms = 3) %>%
#'   # increase `num_terms` to find smoother boundaries
#'   set_engine("earth") %>%
#'   fit(class ~ ., data = parabolic)
#'
#' parabolic_grid$fda <-
#'   predict(fda_mod, parabolic_grid, type = "prob")$.pred_Class1
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2)) +
#'   geom_point(aes(col = class), alpha = .5) +
#'   geom_contour(data = parabolic_grid, aes(z = fda), col = "black", breaks = .5) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   coord_equal()
#' @export
discrim_flexible <-
  function(mode = "classification", num_terms = NULL, prod_degree = NULL,
           prune_method = NULL) {

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    parsnip::new_model_spec(
      "discrim_flexible",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.discrim_flexible <- function(x, ...) {
  cat("Flexible Discriminant Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @param object A flexible discriminant model specification.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @examples
#'
#'
#' model <- discrim_flexible(num_terms = 10)
#' model
#' update(model, num_terms = 6)
#' @method update discrim_flexible
#' @rdname discrim_flexible
#' @export
update.discrim_flexible <-
  function(object,
           num_terms = NULL,
           prod_degree = NULL,
           prune_method = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
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
      "discrim_flexible",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.discrim_flexible <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$prod_degree) && args$prod_degree < 0)
    stop("`prod_degree` should be >= 1", call. = FALSE)

  if (is.numeric(args$num_terms) && args$num_terms < 0)
    stop("`num_terms` should be >= 1", call. = FALSE)

  if (!is.character(args$prune_method) &&
      !is.null(args$prune_method) &&
      !is.character(args$prune_method))
    stop("`prune_method` should be a single string value", call. = FALSE)

  invisible(object)
}

