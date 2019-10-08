# These functions define the discriminant analysis models. They are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

make_discrim_linear_MASS <- function() {
  parsnip::set_new_model("discrim_linear")

  parsnip::set_model_mode("discrim_linear", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("discrim_linear", "classification", "MASS")
  parsnip::set_dependency("discrim_linear", "MASS", "MASS")

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

  parsnip::set_pred(
    model = "discrim_linear",
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
    model = "discrim_linear",
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
    model = "discrim_linear",
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

make_discrim_linear_FDA <- function() {

  parsnip::set_model_engine("discrim_linear", "classification", "FDA")
  parsnip::set_dependency("discrim_linear", eng = "FDA", pkg = "mda")

  parsnip::set_model_arg(
    model = "discrim_linear",
    eng = "FDA",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "discrim_linear",
    eng = "FDA",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "mda", fun = "fda"),
      defaults = list(method = quote(mda::gen.ridge), keep.fitted = FALSE)
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "FDA",
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
    model = "discrim_linear",
    eng = "FDA",
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
          type = "posterior"
        )
    )
  )

  parsnip::set_pred(
    model = "discrim_linear",
    eng = "FDA",
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

