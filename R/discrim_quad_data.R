# These functions define the discriminant analysis models. They are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_discrim_quad_MASS <- function() {
  parsnip::set_new_model("discrim_quad")

  parsnip::set_model_mode("discrim_quad", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("discrim_quad", "classification", "MASS")
  parsnip::set_dependency("discrim_quad", "MASS", "MASS")
  parsnip::set_dependency("discrim_quad", "MASS", "discrim")

  parsnip::set_fit(
    model = "discrim_quad",
    eng = "MASS",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "MASS", fun = "qda"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "discrim_quad",
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
    model = "discrim_quad",
    eng = "MASS",
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
    model = "discrim_quad",
    eng = "MASS",
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
    model = "discrim_quad",
    eng = "MASS",
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

# ------------------------------------------------------------------------------

make_discrim_quad_sparsediscrim <- function() {

  parsnip::set_model_engine("discrim_quad", "classification", "sparsediscrim")
  parsnip::set_dependency("discrim_quad", "sparsediscrim", "sparsediscrim")
  parsnip::set_dependency("discrim_quad", "sparsediscrim", "discrim")

  parsnip::set_fit(
    model = "discrim_quad",
    eng = "sparsediscrim",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "discrim", fun = "fit_regularized_quad"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "discrim_quad",
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
    model = "discrim_quad",
    eng = "sparsediscrim",
    parsnip = "regularization_method",
    original = "method",
    func = list(pkg = "dials", fun = "regularization_method"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "discrim_quad",
    eng = "sparsediscrim",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    )
  )

  parsnip::set_pred(
    model = "discrim_quad",
    eng = "sparsediscrim",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "prob"
        )
    )
  )

  parsnip::set_pred(
    model = "discrim_quad",
    eng = "sparsediscrim",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(as.matrix(new_data))
        )
    )
  )

}

# nocov end

