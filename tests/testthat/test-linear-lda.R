test_that('MASS::lda model object', {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(f_fit$fit$scaling, exp_f_fit_lda$scaling)
  expect_equal(f_fit$fit$means, exp_f_fit_lda$means)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  # `MASS::lda()` doesn't throw an error despite a factor predictor. It converts
  # the factor to integers. Reported to MASS@stats.ox.ac.uk on 2019-10-08. We
  # now use the formula method in the parsnip model to avoid the bug.
  # expect_error(xy_fit$fit$scaling, exp_xy_fit_lda$scaling)
  # expect_error(xy_fit$fit$means, exp_xy_fit$means)

  # pass an extra argument

  expect_error(prior_fit <- fit(prior_spec_lda, Type ~ ., data = glass_tr), NA)
  expect_equal(prior_fit$fit$scaling, exp_prior_fit_lda$scaling)
  expect_equal(prior_fit$fit$means, exp_prior_fit_lda$means)
})

# ------------------------------------------------------------------------------


test_that("MASS::lda class predictions", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit_lda, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred$class)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[, -10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te)
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit_lda, glass_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_f_pred$class)

  # added argument
  expect_error(prior_fit <- fit(prior_spec_lda, Type ~ ., data = glass_tr), NA)
  prior_pred <- predict(prior_fit, glass_te)
  exp_prior_pred <- predict(exp_prior_fit_lda, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred$class)
})

# ------------------------------------------------------------------------------


test_that("MASS::lda prob predictions", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit_lda, glass_te)$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[, -10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit_lda, glass_te)

  expect_s3_class(xy_pred, "tbl_df")
  expect_equal(names(xy_pred), glass_prob_names)
  expect_equal(xy_pred, exp_f_pred)

  # added argument
  expect_error(prior_fit <- fit(prior_spec_lda, Type ~ ., data = glass_tr), NA)
  prior_pred <- predict(prior_fit, glass_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(exp_prior_fit_lda, glass_te)$posterior)

  expect_true(inherits(prior_pred, "tbl_df"))
  expect_equal(names(prior_pred), glass_prob_names)
  expect_equal(prior_pred, exp_prior_pred)
})

# ------------------------------------------------------------------------------


test_that("MASS::lda missing data", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  expect_snapshot(f_pred <- predict(f_fit, glass_na, type = "prob"))
  expect_snapshot(exp_f_pred <- probs_to_tibble(predict(exp_f_fit_lda, glass_na)$posterior))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), glass_prob_names)
  expect_equal(f_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that("sda fit and prediction", {
  skip_if_not_installed("sda")
  skip_if_not_installed("mlbench")

  sda_fit <- sda::sda(
    glass_tr %>% dplyr::select(-factor, -Type) %>% as.matrix(),
    glass_tr$Type,
    verbose = FALSE
  )
  sda_pred <-
    predict(
      sda_fit,
      glass_te %>% dplyr::select(-factor) %>% as.matrix(),
      verbose = FALSE
    )
  expect_error(
    d_fit <-
      discrim_linear() %>%
      set_engine("sda") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(-factor)),
    NA
  )
  expect_error(
    d_pred <- predict(
      d_fit, glass_te %>% dplyr::select(-factor),
      type = "class"
    ),
    NA
  )
  expect_error(
    d_prob <- predict(
      d_fit, glass_te %>% dplyr::select(-factor),
      type = "prob"
    ),
    NA
  )
  expect_equal(
    sda_pred$class,
    d_pred$.pred_class
  )

  expect_equal(
    sda_pred$posterior %>% tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )
})
