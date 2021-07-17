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
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`min_distance`",
#'  "`shrink_cov`", and "`shrink_mean`" (`sparsediscrim` engine only).
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
           penalty = NULL, regularization_method = NULL) {

    args <- list(
      penalty = rlang::enquo(penalty),
      regularization_method = rlang::enquo(regularization_method)
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
           regularization_method = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list(
      penalty = rlang::enquo(penalty),
      regularization_method = rlang::enquo(regularization_method)
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

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# regularized method helpers

lda_regularization_method_vals <-
  c("diagonal", "min_distance", "shrink_cov", "shrink_mean")
qda_regularization_method_vals <-
  c("diagonal", "shrink_cov", "shrink_mean")

discrim_regularized_call_linear <- function(method, ...) {
  basic_opts <- list(x = rlang::expr(x), y = rlang::expr(y), ...)
  mass_opts <- list(x = rlang::expr(x), grouping = rlang::expr(y), ...)
  linear_calls <-
    list(
      shrink_mean = rlang::call2(.ns = "sparsediscrim", .fn = "lda_shrink_mean", !!!basic_opts),
      shrink_cov = rlang::call2(.ns = "sparsediscrim", .fn = "lda_shrink_cov", !!!basic_opts),
      diagonal = rlang::call2(.ns = "sparsediscrim", .fn = "lda_diag", !!!basic_opts),
      min_distance = rlang::call2(.ns = "sparsediscrim", .fn = "lda_emp_bayes_eigen", !!!basic_opts)
    )
  linear_calls[[method]]
}

discrim_regularized_call_quad <- function(method, ...) {
  basic_opts <- list(x = rlang::expr(x), y = rlang::expr(y), ...)
  mass_opts <- list(x = rlang::expr(x), grouping = rlang::expr(y), ...)

  quad_calls <-
    list(
      shrink_mean = rlang::call2(.ns = "sparsediscrim", .fn = "qda_shrink_mean", !!!basic_opts),
      shrink_cov = rlang::call2(.ns = "sparsediscrim", .fn = "qda_shrink_cov", !!!basic_opts),
      diagonal = rlang::call2(.ns = "sparsediscrim", .fn = "qda_diag", !!!basic_opts)
    )

  quad_calls[[method]]
}

#' Wrapper for sparsediscrim models
#' @param x A matrix or data frame.
#' @param y A factor column.
#' @param method A character string.
#' @param ... Not currently used.
#' @return A sparsediscrim object
#' @keywords internal
#' @export
fit_regularized_linear <- function(x, y, method = "diagonal", ...) {
  if (!(method %in% lda_regularization_method_vals)) {
    rlang::abort(
      paste0("'method' should be one of: ",
             paste0("'", lda_regularization_method_vals, "'", collapse = ", "))
    )
  }
  cl <- discrim_regularized_call_linear(method, ...)
  rlang::eval_tidy(cl)
}

#' @rdname fit_regularized_linear
#' @export
fit_regularized_quad <- function(x, y, method = "diagonal", ...) {
  if (!(method %in% qda_regularization_method_vals)) {
    rlang::abort(
      paste0("'method' should be one of: ",
             paste0("'", qda_regularization_method_vals, "'", collapse = ", "))
    )
  }
  cl <- discrim_regularized_call_quad(method, ...)
  rlang::eval_tidy(cl)
}
