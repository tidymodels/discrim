# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov
make_discrim_flexible <- function() {
  parsnip::set_new_model("discrim_flexible")

  parsnip::set_model_mode("discrim_flexible", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("discrim_flexible", "classification", "earth")
  parsnip::set_dependency("discrim_flexible", eng = "earth", pkg = "mda")
  parsnip::set_dependency("discrim_flexible", eng = "earth", pkg = "earth")

  parsnip::set_model_arg(
    model = "discrim_flexible",
    eng = "earth",
    parsnip = "num_terms",
    original = "nprune",
    func = list(pkg = "dials", fun = "num_terms"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "discrim_flexible",
    eng = "earth",
    parsnip = "prod_degree",
    original = "degree",
    func = list(pkg = "dials", fun = "prod_degree"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "discrim_flexible",
    eng = "earth",
    parsnip = "prune_method",
    original = "pmethod",
    func = list(pkg = "dials", fun = "prune_method"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "discrim_flexible",
    eng = "earth",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "mda", fun = "fda"),
      defaults = list(method = quote(earth::earth))
    )
  )

  parsnip::set_encoding(
    model = "discrim_flexible",
    eng = "earth",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE
    )
  )

  parsnip::set_pred(
    model = "discrim_flexible",
    eng = "earth",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "discrim", fun = "pred_wrapper"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "discrim_flexible",
    eng = "earth",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = prob_matrix_to_tibble,
      func = c(pkg = "discrim", fun = "pred_wrapper"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "posterior"
        )
    )
  )

  parsnip::set_pred(
    model = "discrim_flexible",
    eng = "earth",
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
