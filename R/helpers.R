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
      shrink_mean = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "lda_shrink_mean",
        !!!basic_opts
      ),
      shrink_cov = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "lda_shrink_cov",
        !!!basic_opts
      ),
      diagonal = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "lda_diag",
        !!!basic_opts
      ),
      min_distance = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "lda_emp_bayes_eigen",
        !!!basic_opts
      )
    )
  linear_calls[[method]]
}

discrim_regularized_call_quad <- function(method, ...) {
  basic_opts <- list(x = rlang::expr(x), y = rlang::expr(y), ...)
  mass_opts <- list(x = rlang::expr(x), grouping = rlang::expr(y), ...)

  quad_calls <-
    list(
      shrink_mean = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "qda_shrink_mean",
        !!!basic_opts
      ),
      shrink_cov = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "qda_shrink_cov",
        !!!basic_opts
      ),
      diagonal = rlang::call2(
        .ns = "sparsediscrim",
        .fn = "qda_diag",
        !!!basic_opts
      )
    )

  quad_calls[[method]]
}

#' Wrapper for sparsediscrim models
#' @param x A matrix or data frame.
#' @param y A factor column.
#' @param regularization_method A character string.
#' @param ... Not currently used.
#' @return A sparsediscrim object
#' @keywords internal
#' @export
fit_regularized_linear <- function(
  x,
  y,
  regularization_method = "diagonal",
  ...,
  call = rlang::caller_env()
) {
  rlang::arg_match(
    regularization_method,
    lda_regularization_method_vals,
    error_call = call
  )
  cl <- discrim_regularized_call_linear(regularization_method, ...)
  rlang::eval_tidy(cl)
}

#' @rdname fit_regularized_linear
#' @export
fit_regularized_quad <- function(
  x,
  y,
  regularization_method = "diagonal",
  ...,
  call = rlang::caller_env()
) {
  rlang::arg_match(
    regularization_method,
    qda_regularization_method_vals,
    error_call = call
  )
  cl <- discrim_regularized_call_quad(regularization_method, ...)
  rlang::eval_tidy(cl)
}
