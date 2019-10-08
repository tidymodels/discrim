context("linear discrim - lda")

# ------------------------------------------------------------------------------

library(mlbench)
data(Glass)

set.seed(55822)
in_samp <- sample.int(nrow(Glass), 5)

# Add a random factor predictor to test dummy variables
Glass$factor <- factor(sample(letters[1:4], nrow(Glass), replace = TRUE))

glass_tr <- Glass[-in_samp,]
glass_te <- Glass[ in_samp, -10]
glass_na <- glass_te
glass_na$RI[1] <- NA
glass_na$Na[2] <- NA

glass_lvl <- levels(Glass$Type)
prob_names <- paste0(".pred_", glass_lvl)

# ------------------------------------------------------------------------------

lda_spec   <- discrim_linear() %>% set_engine("MASS")
prior_spec <- discrim_linear() %>% set_engine("MASS", prior = rep(1/6, 6))

exp_f_fit     <- MASS::lda(Type ~ ., data = glass_tr)
exp_xy_fit    <- MASS::lda(x = glass_tr[,-10], grouping = glass_tr$Type)
exp_prior_fit <- MASS::lda(Type ~ ., data = glass_tr, prior = rep(1/6, 6))

probs_to_tibble <- function(x) {
  x <- tibble::as_tibble(x)
  names(x) <- paste0(".pred_", names(x))
  x
}

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(f_fit$fit$scaling, exp_f_fit$scaling)
  expect_equal(f_fit$fit$means, exp_f_fit$means)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  # `MASS::lda()` doesn't throw an error despite a factor predictor. It converts
  # the factor to integers. Reported to MASS@stats.ox.ac.uk on 2019-10-08. We
  # now use the formula method in the parsnip model to avoid the bug.
  # expect_error(xy_fit$fit$scaling, exp_xy_fit$scaling)
  # expect_error(xy_fit$fit$means, exp_xy_fit$means)

  # pass an extra argument

  expect_error(prior_fit <- fit(prior_spec, Type ~ ., data = glass_tr), NA)
  expect_equal(prior_fit$fit$scaling, exp_prior_fit$scaling)
  expect_equal(prior_fit$fit$means, exp_prior_fit$means)
})

# ------------------------------------------------------------------------------


test_that('class predictions', {
  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred$class)

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
  expect_equal(xy_pred$.pred_class, exp_f_pred$class)

  # added argument
  expect_error(prior_fit <- fit(prior_spec, Type ~ ., data = glass_tr), NA)
  prior_pred <- predict(prior_fit, glass_te)
  exp_prior_pred <- predict(exp_prior_fit, glass_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred$class)
})

# ------------------------------------------------------------------------------


test_that('prob predictions', {
  # formula method
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_te)$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(lda_spec, x = glass_tr[,-10], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, glass_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_f_pred)

  # added argument
  expect_error(prior_fit <- fit(prior_spec, Type ~ ., data = glass_tr), NA)
  prior_pred <- predict(prior_fit, glass_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(exp_prior_fit, glass_te)$posterior)

  expect_true(inherits(prior_pred, "tbl_df"))
  expect_equal(names(prior_pred), prob_names)
  expect_equal(prior_pred, exp_prior_pred)
})

# ------------------------------------------------------------------------------


test_that('missing data', {
  expect_error(f_fit <- fit(lda_spec, Type ~ ., data = glass_tr), NA)
  expect_warning(f_pred <- predict(f_fit, glass_na, type = "prob"))
  expect_warning(exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na)$posterior))

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)
})

# ------------------------------------------------------------------------------

test_that('api errors', {
  expect_error(
    discrim_linear() %>% set_engine("lda"),
    "engine 'lda' is not availble"
  )
})



