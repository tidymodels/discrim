
make_discrim_regularized <- function() {
  parsnip::set_new_model("discrim_regularized")

  parsnip::set_model_mode("discrim_regularized", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("discrim_regularized", "classification", "rda")
  parsnip::set_dependency("discrim_regularized", eng = "rda", pkg = "klaR")


  parsnip::set_model_arg(
    model = "discrim_regularized",
    eng = "rda",
    parsnip = "frac_common_cov",
    original = "lambda",
    func = list(pkg = "dials", fun = "fraction"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "discrim_regularized",
    eng = "rda",
    parsnip = "frac_identity",
    original = "gamma",
    func = list(pkg = "dials", fun = "fraction"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
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

  parsnip::set_pred(
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

  parsnip::set_pred(
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

  parsnip::set_pred(
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
