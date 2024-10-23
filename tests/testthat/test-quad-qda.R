test_that("MASS::qda model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr))
  expect_equal(f_fit$fit$scaling, exp_f_fit_qda$scaling)
  expect_equal(f_fit$fit$means, exp_f_fit_qda$means)

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[, -1], y = penguin_tr$species)
  )

  # pass an extra argument

  expect_no_error(prior_fit <- fit(prior_spec_qda, species ~ ., data = penguin_tr))
  expect_equal(prior_fit$fit$scaling, exp_prior_fit_qda$scaling)
  expect_equal(prior_fit$fit$means, exp_prior_fit_qda$means)
})

# ------------------------------------------------------------------------------


test_that("MASS::qda class predictions", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr))
  f_pred <- predict(f_fit, penguin_te)
  exp_f_pred <- predict(exp_f_fit_qda, penguin_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred$class)

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[, -1], y = penguin_tr$species)
  )
  xy_pred <- predict(xy_fit, penguin_te)
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, penguin_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_f_pred$class)

  # added argument
  expect_no_error(prior_fit <- fit(prior_spec_qda, species ~ ., data = penguin_tr))
  prior_pred <- predict(prior_fit, penguin_te)
  exp_prior_pred <- predict(exp_prior_fit_qda, penguin_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred$class)
})

# ------------------------------------------------------------------------------


test_that("MASS::qda prob predictions", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  # formula method
  expect_no_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr))
  f_pred <- predict(f_fit, penguin_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit_qda, penguin_te)$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), pen_prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_no_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[, -1], y = penguin_tr$species)
  )
  xy_pred <- predict(xy_fit, penguin_te, type = "prob")
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, penguin_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), pen_prob_names)
  expect_equal(xy_pred, exp_f_pred)

  # added argument
  expect_no_error(prior_fit <- fit(prior_spec_qda, species ~ ., data = penguin_tr))
  prior_pred <- predict(prior_fit, penguin_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(exp_prior_fit_qda, penguin_te)$posterior)

  expect_true(inherits(prior_pred, "tbl_df"))
  expect_equal(names(prior_pred), pen_prob_names)
  expect_equal(prior_pred, exp_prior_pred)
})

# ------------------------------------------------------------------------------

test_that("MASS::qda missing data", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  exp_f_fit_miss <- MASS::qda(species ~ ., data = penguins_miss)
  expect_no_error(f_fit <- fit(qda_spec, species ~ ., data = penguins_miss))
  expect_snapshot(f_pred <- predict(f_fit, penguins_miss, type = "prob"))
  expect_snapshot(
    # exp_f_pred <- probs_to_tibble(predict(exp_f_fit_miss, penguins_miss)$posterior)
    exp_f_pred <- predict(exp_f_fit_miss, penguins_miss)$posterior
  )
  exp_f_pred <- probs_to_tibble(exp_f_pred)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(nrow(f_pred) == nrow(penguins_miss))
  expect_equal(names(f_pred), pen_prob_names)
  expect_equal(f_pred, exp_f_pred)
})
