context("quadratic discrim - qda")

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

# ------------------------------------------------------------------------------

data(penguins, package = "modeldata")
penguins$island <- NULL
penguins_miss <- penguins
penguins <- na.omit(penguins)
in_train <- seq(1, nrow(penguins), by = 2)
penguin_tr <- penguins[ in_train, ]
penguin_te <- penguins[-in_train, ]

qda_spec   <- discrim_quad() %>% set_engine("MASS")
prior_spec <- discrim_quad() %>% set_engine("MASS", prior = rep(1/3, 3))

exp_f_fit     <- MASS::qda(species ~ ., data = penguin_tr)
exp_xy_fit    <- MASS::qda(x = penguin_tr[,-1], grouping = penguin_tr$species)
exp_prior_fit <- MASS::qda(species ~ ., data = penguin_tr, prior = rep(1/3, 3))

prob_names <- c(".pred_Adelie", ".pred_Chinstrap", ".pred_Gentoo")

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr), NA)
  expect_equal(f_fit$fit$scaling, exp_f_fit$scaling)
  expect_equal(f_fit$fit$means, exp_f_fit$means)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[,-1], y = penguin_tr$species),
    NA
  )
  # `MASS::qda()` doesn't throw an error despite a factor predictor. It converts
  # the factor to integers. Reported to MASS@stats.ox.ac.uk on 2019-10-08. We
  # now use the formula method in the parsnip model to avoid the bug.
  # expect_error(xy_fit$fit$scaling, exp_xy_fit$scaling)
  # expect_error(xy_fit$fit$means, exp_xy_fit$means)

  # pass an extra argument

  expect_error(prior_fit <- fit(prior_spec, species ~ ., data = penguin_tr), NA)
  expect_equal(prior_fit$fit$scaling, exp_prior_fit$scaling)
  expect_equal(prior_fit$fit$means, exp_prior_fit$means)
})

# ------------------------------------------------------------------------------


test_that('class predictions', {
  # formula method
  expect_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr), NA)
  f_pred <- predict(f_fit, penguin_te)
  exp_f_pred <- predict(exp_f_fit, penguin_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred$class)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[,-1], y = penguin_tr$species),
    NA
  )
  xy_pred <- predict(xy_fit, penguin_te)
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, penguin_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_f_pred$class)

  # added argument
  expect_error(prior_fit <- fit(prior_spec, species ~ ., data = penguin_tr), NA)
  prior_pred <- predict(prior_fit, penguin_te)
  exp_prior_pred <- predict(exp_prior_fit, penguin_te)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred$class)
})

# ------------------------------------------------------------------------------


test_that('prob predictions', {
  # formula method
  expect_error(f_fit <- fit(qda_spec, species ~ ., data = penguin_tr), NA)
  f_pred <- predict(f_fit, penguin_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, penguin_te)$posterior)

  expect_true(inherits(f_pred, "tbl_df"))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(qda_spec, x = penguin_tr[,-1], y = penguin_tr$species),
    NA
  )
  xy_pred <- predict(xy_fit, penguin_te, type = "prob")
  # See bug note above
  # exp_xy_pred <- predict(exp_xy_fit, penguin_te)

  expect_true(inherits(xy_pred, "tbl_df"))
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_f_pred)

  # added argument
  expect_error(prior_fit <- fit(prior_spec, species ~ ., data = penguin_tr), NA)
  prior_pred <- predict(prior_fit, penguin_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(exp_prior_fit, penguin_te)$posterior)

  expect_true(inherits(prior_pred, "tbl_df"))
  expect_equal(names(prior_pred), prob_names)
  expect_equal(prior_pred, exp_prior_pred)
})

# ------------------------------------------------------------------------------


test_that('missing data', {
  exp_f_fit     <- MASS::qda(species ~ ., data = penguins_miss)
  expect_error(f_fit <- fit(qda_spec, species ~ ., data = penguins_miss), NA)
  expect_warning(f_pred <- predict(f_fit, penguins_miss, type = "prob"))
  expect_warning(exp_f_pred <- probs_to_tibble(predict(exp_f_fit, penguins_miss)$posterior))

  expect_true(inherits(f_pred, "tbl_df"))
  expect_true(nrow(f_pred) == nrow(penguins_miss))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("discrim_quad", "_pkgs")) %>%
      dplyr::filter(engine == "MASS", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("MASS", "discrim"))
  )

  expect_identical(
    get_from_env(paste0("discrim_quad", "_pkgs")) %>%
      dplyr::filter(engine == "MASS", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
