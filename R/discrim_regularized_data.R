
make_discrim_regularized <- function() {
  set_new_model("discrim_regularized")

  set_model_mode("discrim_regularized", "classification")

  # ------------------------------------------------------------------------------

  set_model_engine("discrim_regularized", "classification", "rda")
  set_dependency("discrim_regularized", eng = "rda", pkg = "klaR")


  set_model_arg(
    model = "discrim_regularized",
    eng = "rda",
    parsnip = "frac_common_cov",
    original = "lambda",
    func = list(pkg = "dials", fun = "fraction"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "discrim_regularized",
    eng = "rda",
    parsnip = "frac_identity",
    original = "gamma",
    func = list(pkg = "dials", fun = "fraction"),
    has_submodel = FALSE
  )

  set_fit(
    model = "discrim_regularized",
    eng = "rda",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(fun = "rda_wrapper"),
      defaults = list()
    )
  )

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
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

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
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

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
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

make_discrim_regularized_rda <- function() {

  set_model_engine("discrim_regularized", "classification", "rda")
  set_dependency("discrim_regularized", eng = "rda", pkg = "mda")

  set_model_arg(
    model = "discrim_regularized",
    eng = "rda",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  set_fit(
    model = "discrim_regularized",
    eng = "rda",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "grouping"),
      func = c(pkg = "discrim", fun = "mass_rda_wrapper"),
      defaults = list(crossval = FALSE)
    )
  )

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
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

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = post_to_tibble,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          posterior = TRUE
        )
    )
  )

  set_pred(
    model = "discrim_regularized",
    eng = "rda",
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

