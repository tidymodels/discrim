test_that("naivebayes::naive_bayes", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("naivebayes")

  # ------------------------------------------------------------------------------

  data(Glass)

  # Naive Bayes doesn't like zero-variance predictors within a class
  Glass <- Glass[, !(names(Glass) %in% c("K", "Ba", "Fe"))]

  set.seed(55822)
  in_samp <- sample.int(nrow(Glass), 5)

  # Add a random factor predictor to test dummy variables
  Glass$factor <- factor(sample(letters[1:3], nrow(Glass), replace = TRUE))

  glass_tr <- Glass[-in_samp, ]
  glass_te <- Glass[in_samp, -7]
  glass_na <- glass_te
  glass_na$RI[1] <- NA
  glass_na$Na[2] <- NA

  glass_lvl <- levels(Glass$Type)
  prob_names <- paste0(".pred_", glass_lvl)

  # ------------------------------------------------------------------------------

  nb_spec <- naive_Bayes(smoothness = 1.2) %>% set_engine("naivebayes")
  prior_spec <- naive_Bayes() %>% set_engine("naivebayes", prior = rep(1 / 6, 6))

  exp_f_fit <- naivebayes::naive_bayes(
    Type ~ .,
    data = glass_tr,
    usekernel = TRUE, adjust = 1.2
  )

  exp_xy_fit <- naivebayes::naive_bayes(
    x = glass_tr[, -7], y = glass_tr$Type,
    usekernel = TRUE, adjust = 1.2
  )

  exp_prior_fit <- naivebayes::naive_bayes(
    x = glass_tr[, -7], y = glass_tr$Type,
    prior = rep(1 / 6, 6), usekernel = TRUE
  )

  # ------------------------------------------------------------------------------

  # formula method
  expect_error(f_fit <- fit(nb_spec, Type ~ ., data = glass_tr), NA)
  # The calls are embedded and different so check the numbers
  for (x in setdiff(names(f_fit$fit$tables), "factor")) {
    x_dat <- f_fit$fit$tables[[x]]
    for (val in seq(along = x_dat)) {
      expect_equal(x_dat[[val]]$x, exp_f_fit$tables[[x]][[val]]$x)
      expect_equal(x_dat[[val]]$y, exp_f_fit$tables[[x]][[val]]$y)
    }
  }

  expect_equal(f_fit$fit$tables[["factor"]], exp_f_fit$tables[["factor"]])

  # x/y method
  expect_error(
    xy_fit <- fit_xy(nb_spec, x = glass_tr[, -7], y = glass_tr$Type),
    NA
  )
  for (x in setdiff(names(xy_fit$fit$tables), "factor")) {
    x_dat <- xy_fit$fit$tables[[x]]

    if (!is.table(x_dat)) {
      for (val in seq(along = x_dat)) {
        expect_equal(x_dat[[val]]$x, exp_xy_fit$tables[[x]][[val]]$x)
        expect_equal(x_dat[[val]]$y, exp_xy_fit$tables[[x]][[val]]$y)
      }
    } else {
      expect_equal(x_dat, exp_xy_fit$tables[[x]])
    }
  }
  expect_equal(xy_fit$fit$tables[["factor"]], exp_xy_fit$tables[["factor"]])

  # pass an extra argument
  expect_error(prior_fit <- fit_xy(prior_spec, x = glass_tr[, -7], y = glass_tr$Type), NA)
  for (x in setdiff(names(prior_fit$fit$tables), "factor")) {
    x_dat <- prior_fit$fit$tables[[x]]

    if (!is.table(x_dat)) {
      for (val in seq(along = x_dat)) {
        expect_equal(x_dat[[val]]$x, exp_prior_fit$tables[[x]][[val]]$x)
        expect_equal(x_dat[[val]]$y, exp_prior_fit$tables[[x]][[val]]$y)
      }
    } else {
      expect_equal(x_dat, exp_prior_fit$tables[[x]], check.attributes = FALSE)
    }
  }
  expect_equal(prior_fit$fit$tables[["factor"]], exp_prior_fit$tables[["factor"]])

  # ------------------------------------------------------------------------------

  # formula method
  expect_error(f_fit <- fit(nb_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te)
  exp_f_pred <- predict(exp_f_fit, glass_te)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_f_pred, ignore_attr = TRUE)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(nb_spec, x = glass_tr[, -7], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te)
  exp_xy_pred <- predict(exp_xy_fit, glass_te)

  expect_s3_class(xy_pred, "tbl_df")
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_xy_pred, ignore_attr = TRUE)

  # added argument
  expect_error(prior_fit <- fit_xy(prior_spec, x = glass_tr[, -7], y = glass_tr$Type), NA)
  prior_pred <- predict(prior_fit, glass_te)
  exp_prior_pred <- predict(exp_prior_fit, glass_te)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred, ignore_attr = TRUE)

  # ------------------------------------------------------------------------------
  # prob predictions

  # formula method
  expect_error(f_fit <- fit(nb_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_te, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_te, type = "prob"))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)

  # x/y method
  expect_error(
    xy_fit <- fit_xy(nb_spec, x = glass_tr[, -7], y = glass_tr$Type),
    NA
  )
  xy_pred <- predict(xy_fit, glass_te, type = "prob")
  exp_xy_pred <- probs_to_tibble(predict(exp_xy_fit, glass_te, type = "prob"))

  expect_s3_class(xy_pred, "tbl_df")
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_xy_pred)

  # added argument
  expect_error(prior_fit <- fit_xy(prior_spec, x = glass_tr[, -7], y = glass_tr$Type), NA)
  prior_pred <- predict(prior_fit, glass_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(exp_prior_fit, glass_te, type = "prob"))

  expect_s3_class(prior_pred, "tbl_df")
  expect_equal(names(prior_pred), prob_names)
  expect_equal(prior_pred, exp_prior_pred)

  # ------------------------------------------------------------------------------
  # missing data

  expect_error(f_fit <- fit(nb_spec, Type ~ ., data = glass_tr), NA)
  f_pred <- predict(f_fit, glass_na, type = "prob")
  exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na, type = "prob"))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(nrow(f_pred) == nrow(glass_te))
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_f_pred)
})

