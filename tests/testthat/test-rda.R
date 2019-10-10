context("regularized discrim")

# ------------------------------------------------------------------------------

source("helper-objects.R")

# ------------------------------------------------------------------------------

rda_spec <-
  discrim_regularized(frac_common_cov = .1, frac_identity = 1) %>%
  set_engine("klaR")

prior_spec <- discrim_regularized() %>% set_engine("klaR", prior = rep(1/6, 6))

exp_f_fit <- klaR::rda(Type ~ ., data = glass_tr, lambda = .1, gamma = 1)

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(rda_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(f_fit$fit$covpooled, exp_f_fit$covpooled)
  expect_equal(f_fit$fit$means, exp_f_fit$means)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(rda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  expect_equal(xy_fit$fit$covpooled, exp_f_fit$covpooled)
  expect_equal(xy_fit$fit$means, exp_f_fit$means)

})

# ------------------------------------------------------------------------------


test_that('class predictions', {
  # formula method
  expect_error(f_fit <- fit(rda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit, glass_te)$class

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(rda_spec, x = glass_tr[,-10], y = glass_tr$Type),
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
  expect_error(f_fit <- fit(rda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_te, type = "posterior")$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(rda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_f_pred)

})

# ------------------------------------------------------------------------------


test_that('missing data', {
  expect_error(f_fit <- fit(rda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_na, type = "prob")

  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na, type = "posterior")$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)
})


# ------------------------------------------------------------------------------

test_that('api errors', {
  expect_error(
    discrim_regularized() %>% set_engine("monday"),
    "engine 'monday' is not availble"
  )
})

# ------------------------------------------------------------------------------

test_that('printing', {
  expect_output(
    print(rda_spec),
    "Regularized Discriminant Model Specification"
  )
})

# ------------------------------------------------------------------------------

test_that('updating', {
  rda_spec_2 <-
    discrim_regularized(frac_common_cov = 1, frac_identity = 1) %>%
    set_engine("klaR")
  rda_spec_3 <- update(rda_spec, frac_common_cov = 1, frac_identity = 1)
  expect_equal(rda_spec_2, rda_spec_3)

  prior_spec_2 <- discrim_regularized(frac_common_cov = 1) %>%
    set_engine("klaR", prior = rep(1/6, 6))
  prior_spec_3 <- update(prior_spec, frac_common_cov = 1)
  expect_equal(prior_spec_2, prior_spec_3)

})
