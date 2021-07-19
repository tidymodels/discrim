#' Regularized discriminant analysis
#'
#' @description
#'
#' `discrim_regularized()` defines a model that estimates a multivariate
#'  distribution for the predictors separately for the data in each class. The
#'  structure of the model can be LDA, QDA, or some amalgam of the two. Bayes'
#'  theorem is used to compute the probability of each class, given the
#'  predictor values.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_regularized", "discrim")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#'
#' @inheritParams discrim_linear
#' @param frac_common_cov,frac_identity Numeric values between zero and one.
#'
#' @details
#' There are many ways of regularizing models. For example, one form of
#'  regularization is to penalize model parameters. Similarly, the classic
#'  James–Stein regularization approach shrinks the model structure to a less
#'  complex form.
#'
#' The model fits a very specific type of regularized model by Friedman (1989)
#'  that uses two types of regularization. One modulates how class-specific the
#'  covariance matrix should be. This allows the model to balance between LDA
#'  and QDA. The second regularization component shrinks the covariance matrix
#'  towards the identity matrix.
#'
#' For the penalization approach, [discrim_linear()] with a `mda` engine can be
#'  used. Other regularization methods can be used with [discrim_linear()] and
#'  [discrim_quad()] can used via the `sparsediscrim` engine for those functions.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @references
#' Friedman, J (1989). Regularized Discriminant Analysis. _Journal of the
#' American Statistical Association_, 84, 165-175.
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_regularized", "discrim")}
#'
#' @examples
#' parabolic_grid <-
#'   expand.grid(X1 = seq(-5, 5, length = 100),
#'               X2 = seq(-5, 5, length = 100))
#'
#' rda_mod <-
#'   discrim_regularized(frac_common_cov = .5, frac_identity = .5) %>%
#'   set_engine("klaR") %>%
#'   fit(class ~ ., data = parabolic)
#'
#' parabolic_grid$rda <-
#'   predict(rda_mod, parabolic_grid, type = "prob")$.pred_Class1
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2)) +
#'   geom_point(aes(col = class), alpha = .5) +
#'   geom_contour(data = parabolic_grid, aes(z = rda), col = "black", breaks = .5) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   coord_equal()
#' @export
discrim_regularized <-
  function(mode = "classification", engine = "klaR",
           frac_common_cov = NULL, frac_identity = NULL) {

    args <- list(
      frac_common_cov = rlang::enquo(frac_common_cov),
      frac_identity = rlang::enquo(frac_identity)
    )

    parsnip::new_model_spec(
      "discrim_regularized",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.discrim_regularized <- function(x, ...) {
  cat("Regularized Discriminant Model Specification (", x$mode, ")\n\n", sep = "")
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
#' model <- discrim_regularized(frac_common_cov = 10)
#' model
#' update(model, frac_common_cov = 1)
#' @method update discrim_regularized
#' @rdname discrim_regularized
#' @export
update.discrim_regularized <-
  function(object,
           frac_common_cov = NULL,
           frac_identity = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)
    args <- list(
      frac_common_cov = rlang::enquo(frac_common_cov),
      frac_identity = rlang::enquo(frac_identity)
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
      "discrim_regularized",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.discrim_regularized <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$frac_common_cov) &&
      (args$frac_common_cov < 0 | args$frac_common_cov > 1)) {
    stop("The common covariance fraction should be between zero and one", call. = FALSE)
  }
  if (is.numeric(args$frac_identity) &&
      (args$frac_identity < 0 | args$frac_identity > 1)) {
    stop("The identity matrix fraction should be between zero and one", call. = FALSE)
  }
  invisible(object)
}


