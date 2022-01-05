context("linear discrim - sda")

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('sda fit and prediction', {

  sda_fit <- sda::sda(
    glass_tr %>% dplyr::select(-factor, -Type) %>% as.matrix(),
    glass_tr$Type,
    verbose = FALSE
  )
  sda_pred <-
    predict(sda_fit,
            glass_te %>% dplyr::select(-factor) %>% as.matrix(),
            verbose = FALSE)
  expect_error(
    d_fit <-
      discrim_linear() %>%
      set_engine("sda") %>%
      fit(Type ~ ., data = glass_tr %>% dplyr::select(-factor)),
    NA
  )
  expect_error(
    d_pred <- predict(d_fit, glass_te %>% dplyr::select(-factor),
                      type = "class"),
    NA
  )
  expect_error(
    d_prob <- predict(d_fit, glass_te %>% dplyr::select(-factor),
                      type = "prob"),
    NA
  )
  expect_equal(
    sda_pred$class,
    d_pred$.pred_class
  )

  expect_equivalent(
    sda_pred$posterior %>% tibble::as_tibble(),
    d_prob
  )
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("discrim_linear", "_pkgs")) %>%
      dplyr::filter(engine == "sda", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("sda", "discrim"))
  )

  expect_identical(
    get_from_env(paste0("discrim_linear", "_pkgs")) %>%
      dplyr::filter(engine == "sda", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
