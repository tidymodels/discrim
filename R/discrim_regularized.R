#' General Interface for Regularized Discriminant Models
#'
#' `discrim_regularized()` is a way to generate a _specification_ of a
#' regularized discriminant analysis (RDA) model before fitting.
#'
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param frac_common_cov,frac_identity Numeric values between zero and one.
#' @details
#'
#' The model is from Friedman (1989) and can create LDA models, QDA models,
#'  and regularized mixtures of the two. It does _not_ conduct feature
#'  selection. The main arguments for the model are:
#' \itemize{
#'   \item \code{frac_common_cov}: The fraction of the regularized covariance
#'   matrix that is based on the LDA model (i.e., computed from all classes). A
#'   value of 1 is the linear discriminant analysis assumption while a value
#'   near zero assumes that there should be separate covariance matrices for
#'   each class.
#'   \item \code{frac_identity}: The fraction of the final, class-specific
#'   covariance matrix that is the identity matrix.
#' }
#'
#' See `klaR::rda()` for the equations that define these parameters.
#'
#' These arguments are converted to their specific names at the time that the
#'  model is fit. Other options and argument can be set using `set_engine()`. If
#'  left to their defaults here (`NULL`), the values are taken from the
#'  underlying model functions. If parameters need to be modified, `update()`
#'  can be used in lieu of recreating the object from scratch.

#'
#' For `discrim_regularized()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the following
#'  _engines_:

#' \itemize{
#' \item \pkg{R}:  `"klaR"`(the default)
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the model fit
#'  call. For this type of model, the template of the fit calls are:
#'
#' \pkg{klaR} engine:
#'
#' \preformatted{
#' klaR::rda(x = missing_arg(), grouping = missing_arg(), lambda = frac_common_cov,
#'           gamma = frac_identity, crossval = FALSE)
#' }
#'
#'
#' @references Friedman, J.H. (1989). Regularized Discriminant Analysis. _Journal
#' of the American Statistical Association_ 84, 165-175.
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
  function(mode = "classification", frac_common_cov = NULL, frac_identity = NULL) {

    args <- list(
      frac_common_cov = rlang::enquo(frac_common_cov),
      frac_identity = rlang::enquo(frac_identity)
    )

    new_model_spec(
      "discrim_regularized",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
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

    new_model_spec(
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


