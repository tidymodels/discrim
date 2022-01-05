context("quadratic discrim - sparsediscrim")

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('qda_diag fit and prediction', {

  spd_fit <- sparsediscrim::qda_diag(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "class")
  spd_prob <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "prob")
  expect_error(
    d_fit <-
      discrim_quad(regularization_method = "diagonal") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type)),
    NA
  )
  expect_error(
    d_pred <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "class"),
    NA
  )
  expect_error(
    d_prob <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "prob"),
    NA
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equivalent(
    spd_prob %>% tibble::as_tibble(),
    d_prob
  )
})


# ------------------------------------------------------------------------------

test_that('qda_shrink_mean fit and prediction', {

  spd_fit <- sparsediscrim::qda_shrink_mean(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "class")
  spd_prob <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "prob")
  expect_error(
    d_fit <-
      discrim_quad(regularization_method = "shrink_mean") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type)),
    NA
  )
  expect_error(
    d_pred <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "class"),
    NA
  )
  expect_error(
    d_prob <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "prob"),
    NA
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equivalent(
    spd_prob %>% tibble::as_tibble(),
    d_prob
  )
})

# ------------------------------------------------------------------------------

test_that('qda_shrink_cov fit and prediction', {

  spd_fit <- sparsediscrim::qda_shrink_cov(
    glass_tr %>% dplyr::select(RI, Na, Mg),
    glass_tr$Type
  )
  spd_pred <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "class")
  spd_prob <-
    predict(spd_fit,
            glass_te %>% dplyr::select(RI, Na, Mg),
            type = "prob")
  expect_error(
    d_fit <-
      discrim_quad(regularization_method = "shrink_cov") %>%
      set_engine("sparsediscrim") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(RI, Na, Mg, Type)),
    NA
  )
  expect_error(
    d_pred <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "class"),
    NA
  )
  expect_error(
    d_prob <- predict(d_fit, glass_te %>% dplyr::select(RI, Na, Mg),
                      type = "prob"),
    NA
  )
  expect_equal(
    spd_pred,
    d_pred$.pred_class
  )

  expect_equivalent(
    spd_prob %>% tibble::as_tibble(),
    d_prob
  )
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("discrim_quad", "_pkgs")) %>%
      dplyr::filter(engine == "sparsediscrim", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("sparsediscrim", "discrim"))
  )

  expect_identical(
    get_from_env(paste0("discrim_quad", "_pkgs")) %>%
      dplyr::filter(engine == "sparsediscrim", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
