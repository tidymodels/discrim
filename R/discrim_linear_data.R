# These functions define the discriminant analysis models. They are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_discrim_linear_MASS <- function() {
  parsnip::set_model_engine("discrim_linear", "classification", "MASS")
  parsnip::set_dependency("discrim_linear", "MASS", "MASS")
  parsnip::set_dependency("discrim_linear", "MASS", "discrim")

  parsnip::set_fit(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "MASS", fun = "lda"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = get_class,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = post_to_tibble,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
}

# ------------------------------------------------------------------------------

make_discrim_linear_mda <- function() {
  parsnip::set_model_engine("discrim_linear", "classification", "mda")
  parsnip::set_dependency("discrim_linear", eng = "mda", pkg = "mda")
  parsnip::set_dependency("discrim_linear", eng = "mda", pkg = "discrim")

  parsnip::set_model_arg(
    model = "discrim_linear",
    eng = "mda",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "discrim_linear",
    eng = "mda",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "mda", fun = "fda"),
      defaults = list(method = quote(mda::gen.ridge), keep.fitted = FALSE)
    )
  )

  parsnip::set_encoding(
    model = "discrim_linear",
    eng = "mda",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "mda",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "discrim", fun = "pred_wrapper"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "mda",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = prob_matrix_to_tibble,
      func = c(pkg = "discrim", fun = "pred_wrapper"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "posterior"
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "mda",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
}

# ------------------------------------------------------------------------------

make_discrim_linear_sda <- function() {
  parsnip::set_model_engine("discrim_linear", "classification", "sda")
  parsnip::set_dependency("discrim_linear", "sda", "sda")
  parsnip::set_dependency("discrim_linear", "sda", "discrim")

  parsnip::set_fit(
    model = "discrim_linear",
    eng = "sda",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "Xtrain", y = "L"),
      protect = c("Xtrain", "L"),
      func = c(pkg = "sda", fun = "sda"),
      defaults = list(
        verbose = FALSE
      )
    )
  )

  parsnip::set_encoding(
    model = "discrim_linear",
    eng = "sda",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sda",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = get_class,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        Xtest = quote(as.matrix(new_data)),
        verbose = FALSE
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sda",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = post_to_tibble,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        Xtest = quote(as.matrix(new_data)),
        verbose = FALSE
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sda",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        Xtest = quote(as.matrix(new_data))
      )
    )
  )
}

# ------------------------------------------------------------------------------

make_discrim_linear_sparsediscrim <- function() {
  parsnip::set_model_engine("discrim_linear", "classification", "sparsediscrim")
  parsnip::set_dependency("discrim_linear", "sparsediscrim", "sparsediscrim")
  parsnip::set_dependency("discrim_linear", "sparsediscrim", "discrim")

  parsnip::set_fit(
    model = "discrim_linear",
    eng = "sparsediscrim",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "discrim", fun = "fit_regularized_linear"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "discrim_linear",
    eng = "sparsediscrim",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_model_arg(
    model = "discrim_linear",
    eng = "sparsediscrim",
    parsnip = "regularization_method",
    original = "regularization_method",
    func = list(pkg = "dials", fun = "regularization_method"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sparsediscrim",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sparsediscrim",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "prob"
      )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "sparsediscrim",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(as.matrix(new_data))
      )
    )
  )
}

# nocov end
