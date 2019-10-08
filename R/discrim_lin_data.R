# These functions define the discriminant analysis models. The are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

make_discrim_linear_MASS <- function() {
  set_new_model("discrim_linear")

  set_model_mode("discrim_linear", "classification")

  # ------------------------------------------------------------------------------

  set_model_engine("discrim_linear", "classification", "MASS")
  set_dependency("discrim_linear", "MASS", "MASS")

  set_fit(
    model = "discrim_linear",
    eng = "MASS",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(fun = "mass_lda_wrapper"),
      defaults = list()
    )
  )

  set_pred(
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

  set_pred(
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

  set_pred(
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

  set_model_engine("discrim_linear", "classification", "FDA")
  set_dependency("discrim_linear", eng = "FDA", pkg = "mda")

  set_model_arg(
    model = "discrim_linear",
    eng = "FDA",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  set_fit(
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

  set_pred(
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

  set_pred(
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

  set_pred(
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

