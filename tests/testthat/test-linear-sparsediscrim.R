test_that("sparsediscrim::lda_diag fit and prediction", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("mlbench")

  spd_fit <- sparsediscrim::lda_diag(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  spd_prob <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  expect_no_error(
    d_fit <-
      discrim_linear(regularization_method = "diagonal") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type))
  )
  expect_no_error(
    d_pred <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  )
  expect_no_error(
    d_prob <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equal(
    spd_prob %>% tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )
})


# ------------------------------------------------------------------------------

test_that("sparsediscrim::lda_shrink_mean fit and prediction", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("mlbench")

  spd_fit <- sparsediscrim::lda_shrink_mean(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  spd_prob <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  expect_no_error(
    d_fit <-
      discrim_linear(regularization_method = "shrink_mean") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type))
  )
  expect_no_error(
    d_pred <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  )
  expect_no_error(
    d_prob <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equal(
    spd_prob %>% tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("sparsediscrim::lda_shrink_cov fit and prediction", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("mlbench")

  spd_fit <- sparsediscrim::lda_shrink_cov(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  spd_prob <-
    predict(
      spd_fit,
      glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  expect_no_error(
    d_fit <-
      discrim_linear(regularization_method = "shrink_cov") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type))
  )
  expect_no_error(
    d_pred <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "class"
    )
  )
  expect_no_error(
    d_prob <- predict(
      d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
      type = "prob"
    )
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equal(
    spd_prob %>% tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that('sparsediscrim lda_emp_bayes_eigen fit and prediction', {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("mlbench")

  data(cells, package = "modeldata")
  cell_tr <- cells %>% dplyr::filter(case == "Train") %>% dplyr::select(-case)
  cell_te <- cells %>% dplyr::filter(case == "Test")  %>% dplyr::select(-case, -class)


  spd_fit <- sparsediscrim::lda_emp_bayes_eigen(
    cell_tr %>% dplyr::select(-class),
    cell_tr$class
  )
  spd_pred <-
    predict(spd_fit,
            cell_te,
            type = "class")
  spd_prob <- predict(spd_fit, cell_te, type = "prob")
  expect_no_error(
    d_fit <-
      discrim_linear(regularization_method = "min_distance") %>%
      set_engine("sparsediscrim") %>%
      fit(class ~ ., data = cell_tr)
  )
  expect_no_error(
    d_pred <- predict(d_fit, cell_te, type = "class")
  )
  expect_no_error(
    d_prob <- predict(d_fit, cell_te, type = "prob")
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equal(
    spd_prob %>% tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )

  expect_snapshot_error(
    d_fit <-
      discrim_linear(regularization_method = "min_dist") %>%
      set_engine("sparsediscrim") %>%
      fit(class ~ ., data = cell_tr)
  )
})
