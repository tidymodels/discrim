#' Flexible discriminant analysis
#'
#' @description
#'
#' `discrim_flexible()` defines a model that fits a discriminant analysis model
#' that can use nonlinear features created using multivariate adaptive
#'  regression splines (MARS).
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_flexible", "discrim")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams discrim_linear
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_flexible", "discrim")}
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
  function(mode = "classification", engine = "earth", num_terms = NULL, prod_degree = NULL,
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
      engine = engine
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

#' Update a model specification
#' @param object A model specification.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @method update discrim_flexible
#' @rdname discrim_update
#' @inheritParams discrim_flexible
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

