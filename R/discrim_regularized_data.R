# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov
make_discrim_regularized <- function() {
  parsnip::set_new_model("discrim_regularized")

  parsnip::set_model_mode("discrim_regularized", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("discrim_regularized", "classification", "klaR")
  parsnip::set_dependency("discrim_regularized", eng = "klaR", pkg = "klaR")
  parsnip::set_dependency("discrim_regularized", eng = "klaR", pkg = "discrim")

  parsnip::set_model_arg(
    model = "discrim_regularized",
    eng = "klaR",
    parsnip = "frac_common_cov",
    original = "lambda",
    func = list(pkg = "discrim", fun = "frac_common_cov"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "discrim_regularized",
    eng = "klaR",
    parsnip = "frac_identity",
    original = "gamma",
    func = list(pkg = "discrim", fun = "frac_identity"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "discrim_regularized",
    eng = "klaR",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "klaR", fun = "rda"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "discrim_regularized",
    eng = "klaR",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "discrim_regularized",
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
    model = "discrim_regularized",
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
    model = "discrim_regularized",
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
