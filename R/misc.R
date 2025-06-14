# ------------------------------------------------------------------------------
# post-processing helpers

post_to_tibble <- function(x, object) {
  probs <- x$posterior
  probs <- tibble::as_tibble(probs)
}

get_class <- function(x, object) {
  x$class
}

prob_matrix_to_tibble <- function(x, object) {
  tibble::as_tibble(x)
}

# ------------------------------------------------------------------------------

# This is needed since parnsip is looking for a funciton like `foo(x, y)` but
# we have `NaiveBayes(x, grouping)`. It just maps y -> grouping
#' Internal wrapper functions
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

# For earth models, you cannot pass na.action to predict()

#' @keywords internal
#' @rdname discrim_helpers
#' @export
pred_wrapper <- function(object, new_data, ...) {
  withr::with_options(
    list(na.action = "na.pass"),
    predict(object, new_data, ...)
  )
}
