test_that("mda::fda/earth model object", {
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(fda_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(f_fit$fit$theta.mod, exp_f_fit_fda$theta.mod)
  expect_equal(f_fit$fit$fit$cuts, exp_f_fit_fda$fit$cuts)

  expect_error(
    f_wts_fit <- fit(fda_spec, Type ~ ., case_weights = wts,
                     data = glass_tr),
    NA
  )
  expect_equal(f_wts_fit$fit$theta.mod, exp_f_wts_fit_fda$theta.mod)
  expect_equal(f_wts_fit$fit$fit$cuts, exp_f_wts_fit_fda$fit$cuts)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(fda_spec, x = glass_tr[, -10], y = glass_tr$Type),
    NA
  )
  expect_equal(xy_fit$fit$theta.mod, exp_f_fit_fda$theta.mod)
  expect_equal(xy_fit$fit$fit$cuts, exp_f_fit_fda$fit$cuts)

  expect_error(
    xy_wts_fit <- fit_xy(fda_spec, x = glass_tr[, -10], y = glass_tr$Type,
                         case_weights = wts),
    NA
  )
  expect_equal(xy_wts_fit$fit$theta.mod, exp_f_wts_fit_fda$theta.mod)
  expect_equal(xy_wts_fit$fit$fit$cuts, exp_f_wts_fit_fda$fit$cuts)
})

# ------------------------------------------------------------------------------


test_that("mda::fda/earth class predictions", {
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(fda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit_fda, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(fda_spec, x = glass_tr[, -10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te)
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, glass_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_f_pred)
})

# ------------------------------------------------------------------------------


test_that("mda::fda/earth prob predictions", {
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(fda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit_fda, glass_te, type = "posterior"))

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(fda_spec, x = glass_tr[, -10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), glass_prob_names)
  expect_equal(xy_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------


test_that("mda::fda/earth missing data", {
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  expect_error(f_fit <- fit(fda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_na, type = "prob")

  opt <- getOption("na.action")
  options(na.action = "na.pass")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit_fda, glass_na, type = "posterior"))
  options(na.action = opt)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that("mda::fda/earth updating", {
  fda_spec_2 <- discrim_flexible(num_terms = 6) %>% set_engine("earth")
  fda_spec_3 <- update(fda_spec, num_terms = 6)
  expect_equal(fda_spec_2, fda_spec_3)
})

test_that('mda::fda/earth check_args() works', {
  skip_if_not_installed("earth")
  skip_if_not_installed("parsnip", "1.2.1.9001")
  skip_if_not_installed("mda")
  skip_if_not_installed("mlbench")

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_flexible(prod_degree = 0) %>%
        set_engine("earth") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_flexible(num_terms = 0) %>%
        set_engine("earth") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_flexible(prune_method = 2) %>%
        set_engine("earth") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
})
