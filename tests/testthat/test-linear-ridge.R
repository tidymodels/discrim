test_that('mda::fda/gen.ridge model object', {
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(lda_fda_spec, Type ~ ., data = glass_tr))
  expect_equal(f_fit$fit$theta.mod, exp_f_fit_lda_fda$theta.mod)
  expect_equal(f_fit$fit$fit$coefficients, exp_f_fit_lda_fda$fit$coefficients)

  expect_no_error(
    f_wts_fit <- fit(
      lda_fda_spec,
      Type ~ .,
      data = glass_tr,
      case_weights = wts
    )
  )
  expect_equal(f_wts_fit$fit$theta.mod, exp_f_wts_fit_lda_fda$theta.mod)
  expect_equal(
    f_wts_fit$fit$fit$coefficients,
    exp_f_wts_fit_lda_fda$fit$coefficients
  )

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(lda_fda_spec, x = glass_tr[, -10], y = glass_tr$Type)
  )
  expect_equal(xy_fit$fit$theta.mod, exp_f_fit_lda_fda$theta.mod)
  expect_equal(xy_fit$fit$fit$coefficients, exp_f_fit_lda_fda$fit$coefficients)

  expect_no_error(
    xy_wts_fit <- fit_xy(
      lda_fda_spec,
      x = glass_tr[, -10],
      y = glass_tr$Type,
      case_weights = wts
    )
  )
  expect_equal(xy_wts_fit$fit$theta.mod, exp_f_wts_fit_lda_fda$theta.mod)
  expect_equal(
    xy_wts_fit$fit$fit$coefficients,
    exp_f_wts_fit_lda_fda$fit$coefficients
  )
})

# ------------------------------------------------------------------------------

test_that('mda::fda/gen.ridge class predictions', {
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(lda_fda_spec, Type ~ ., data = glass_tr))
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit_lda_fda, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred)

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(lda_fda_spec, x = glass_tr[, -10], y = glass_tr$Type)
  )
  xy_pred <- predict(xy_fit, glass_te)
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, glass_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that("mda::fda/gen.ridge prob predictions", {
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(lda_fda_spec, Type ~ ., data = glass_tr))
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(
    exp_f_fit_lda_fda,
    glass_te,
    type = "posterior"
  ))

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(lda_fda_spec, x = glass_tr[, -10], y = glass_tr$Type)
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), glass_prob_names)
  expect_equal(xy_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that("mda::fda/gen.ridge missing data", {
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  expect_no_error(f_fit <- fit(lda_fda_spec, Type ~ ., data = glass_tr))
  f_pred <- predict(f_fit, glass_na, type = "prob")

  opt <- getOption("na.action")
  options(na.action = "na.pass")
  exp_f_pred <- probs_to_tibble(predict(
    exp_f_fit_lda_fda,
    glass_na,
    type = "posterior"
  ))
  options(na.action = opt)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that("mda::fda/gen.ridge updating", {
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  lda_spec_2 <- discrim_linear(penalty = .1) |> set_engine("mda")
  lda_spec_3 <- update(lda_fda_spec, penalty = .1)
  expect_equal(lda_spec_2, lda_spec_3)

  prior_spec_2 <- discrim_linear(penalty = .1) |>
    set_engine("mda", prior = rep(1 / 6, 6))
  prior_spec_3 <- update(prior_lda_fda_spec, penalty = .1)
  expect_equal(
    prior_spec_2,
    prior_spec_3,
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )
})
