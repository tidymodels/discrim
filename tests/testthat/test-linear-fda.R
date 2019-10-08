context("linear discrim - fda")

# ------------------------------------------------------------------------------

source("helper-objects.R")

# ------------------------------------------------------------------------------

lda_spec   <- discrim_linear(penalty = 1) %>% set_engine("FDA")
prior_spec <- discrim_linear() %>% set_engine("FDA", prior = rep(1/6, 6))

exp_f_fit     <- mda::fda(Type ~ ., data = glass_tr, method = mda::gen.ridge, lambda = 1)

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(f_fit$fit$theta.mod, exp_f_fit$theta.mod)
  expect_equal(f_fit$fit$fit$coefficients, exp_f_fit$fit$coefficients)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  expect_equal(xy_fit$fit$theta.mod, exp_f_fit$theta.mod)
  expect_equal(xy_fit$fit$fit$coefficients, exp_f_fit$fit$coefficients)

})

# ------------------------------------------------------------------------------


test_that('class predictions', {
  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
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


test_that('prob predictions', {
  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_te, type = "posterior"))

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_f_pred)

})

# ------------------------------------------------------------------------------


test_that('missing data', {
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_na, type = "prob")

  opt <- getOption("na.action")
  options(na.action = "na.pass")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na, type = "posterior"))
  options(na.action = opt)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that('api errors', {
  expect_error(
    discrim_linear() %>% set_engine("monday"),
    "engine 'monday' is not availble"
  )
})



