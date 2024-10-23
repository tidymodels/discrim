# These functions define the discriminant analysis models. They are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_naive_Bayes_naivebayes <- function() {
  parsnip::set_model_engine("naive_Bayes", "classification", "naivebayes")
  parsnip::set_dependency("naive_Bayes", "naivebayes", "naivebayes")
  parsnip::set_dependency("naive_Bayes", "naivebayes", "discrim")

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "naivebayes",
    parsnip = "smoothness",
    original = "adjust",
    func = list(pkg = "dials", fun = "smoothness"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "naivebayes",
    parsnip = "Laplace",
    original = "laplace",
    func = list(pkg = "dials", fun = "Laplace"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "naive_Bayes",
    eng = "naivebayes",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "naivebayes", fun = "naive_bayes"),
      defaults = list(usekernel = TRUE)
    )
  )

  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "naivebayes",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "naivebayes",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "naivebayes",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = prob_matrix_to_tibble,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "prob"
        )
    )
  )
}

make_naive_Bayes_klaR <- function() {
  parsnip::set_model_engine("naive_Bayes", "classification", "klaR")
  parsnip::set_dependency("naive_Bayes", "klaR", "klaR")
  parsnip::set_dependency("naive_Bayes", "klaR", "discrim")

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "klaR",
    parsnip = "smoothness",
    original = "adjust",
    func = list(pkg = "dials", fun = "smoothness"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "klaR",
    parsnip = "Laplace",
    original = "fL",
    func = list(pkg = "dials", fun = "Laplace"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "naive_Bayes",
    eng = "klaR",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "discrim", fun = "klar_bayes_wrapper"),
      defaults = list(usekernel = TRUE)
    )
  )

  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "klaR",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "klaR",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = get_class,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "klaR",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = post_to_tibble,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "klaR",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
}

# nocov end
