#' General Interface for Regularized Discriminant Models
#'
#' `discrim_regularized()` is a way to generate a _specification_ of a
#' regularized discriminant analysis (RDA) model before fitting.
#'
#' @inheritParams discrim_linear
#' @param frac_common_cov,frac_identity Numeric values between zero and one.
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


