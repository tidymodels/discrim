test_that("naivebayes::multinomial_naive_bayes", {
  skip_if_not_installed("naivebayes")

  # ------------------------------------------------------------------------------

  # Simulate a small document-term matrix of word counts
  set.seed(1234)
  counts <- matrix(
    rpois(60 * 8, lambda = 2),
    nrow = 60,
    dimnames = list(NULL, paste0("term_", 1:8))
  )
  topic <- factor(sample(c("sports", "politics", "science"), 60, TRUE))

  in_samp <- sample.int(60, 5)

  counts_tr <- counts[-in_samp, ]
  counts_te <- counts[in_samp, ]
  topic_tr <- topic[-in_samp]

  df_tr <- data.frame(counts_tr, topic = topic_tr)
  df_te <- data.frame(counts_te)

  topic_lvl <- levels(topic)
  prob_names <- paste0(".pred_", topic_lvl)

  # ------------------------------------------------------------------------------

  nb_spec <- naive_Bayes(Laplace = 1) |> set_engine("multinomial_naive_bayes")
  prior_spec <- naive_Bayes() |>
    set_engine("multinomial_naive_bayes", prior = rep(1 / 3, 3))

  exp_fit <- naivebayes::multinomial_naive_bayes(
    x = counts_tr,
    y = topic_tr,
    laplace = 1
  )

  exp_prior_fit <- naivebayes::multinomial_naive_bayes(
    x = counts_tr,
    y = topic_tr,
    prior = rep(1 / 3, 3)
  )

  # ------------------------------------------------------------------------------
  # class predictions

  # formula method
  expect_no_error(f_fit <- fit(nb_spec, topic ~ ., data = df_tr))
  f_pred <- predict(f_fit, df_te)
  exp_pred <- predict(exp_fit, counts_te)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_class"))
  expect_equal(f_pred$.pred_class, exp_pred, ignore_attr = TRUE)

  # x/y method
  expect_no_error(xy_fit <- fit_xy(nb_spec, x = counts_tr, y = topic_tr))
  xy_pred <- predict(xy_fit, counts_te)

  expect_s3_class(xy_pred, "tbl_df")
  expect_true(all(names(xy_pred) == ".pred_class"))
  expect_equal(xy_pred$.pred_class, exp_pred, ignore_attr = TRUE)

  # added argument
  expect_no_error(
    prior_fit <- fit_xy(prior_spec, x = counts_tr, y = topic_tr)
  )
  prior_pred <- predict(prior_fit, counts_te)
  exp_prior_pred <- predict(exp_prior_fit, counts_te)

  expect_s3_class(prior_pred, "tbl_df")
  expect_true(all(names(prior_pred) == ".pred_class"))
  expect_equal(prior_pred$.pred_class, exp_prior_pred, ignore_attr = TRUE)

  # ------------------------------------------------------------------------------
  # prob predictions

  # formula method
  f_pred <- predict(f_fit, df_te, type = "prob")
  exp_pred <- probs_to_tibble(predict(exp_fit, counts_te, type = "prob"))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), prob_names)
  expect_equal(f_pred, exp_pred)

  # x/y method
  xy_pred <- predict(xy_fit, counts_te, type = "prob")

  expect_s3_class(xy_pred, "tbl_df")
  expect_equal(names(xy_pred), prob_names)
  expect_equal(xy_pred, exp_pred)

  # added argument
  prior_pred <- predict(prior_fit, counts_te, type = "prob")
  exp_prior_pred <- probs_to_tibble(predict(
    exp_prior_fit,
    counts_te,
    type = "prob"
  ))

  expect_s3_class(prior_pred, "tbl_df")
  expect_equal(names(prior_pred), prob_names)
  expect_equal(prior_pred, exp_prior_pred)

  # ------------------------------------------------------------------------------
  # sparse matrices

  skip_if_not_installed("Matrix")

  sparse_tr <- Matrix::Matrix(counts_tr, sparse = TRUE)
  sparse_te <- Matrix::Matrix(counts_te, sparse = TRUE)

  expect_no_error(sp_fit <- fit_xy(nb_spec, x = sparse_tr, y = topic_tr))
  sp_pred <- predict(sp_fit, sparse_te)
  exp_sp_pred <- predict(exp_fit, sparse_te)

  expect_s3_class(sp_pred, "tbl_df")
  expect_true(all(names(sp_pred) == ".pred_class"))
  expect_equal(sp_pred$.pred_class, exp_sp_pred, ignore_attr = TRUE)

  sp_pred <- predict(sp_fit, sparse_te, type = "prob")
  exp_sp_pred <- probs_to_tibble(predict(exp_fit, sparse_te, type = "prob"))

  expect_s3_class(sp_pred, "tbl_df")
  expect_equal(names(sp_pred), prob_names)
  expect_equal(sp_pred, exp_sp_pred)
})
