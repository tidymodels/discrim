test_that("sda fit and prediction", {
  skip_if_not_installed("sda")
  skip_if_not_installed("mlbench")
  # exp_* objects in helper-object.R

  sda_fit <- sda::sda(
    glass_tr |> dplyr::select(-factor, -Type) |> as.matrix(),
    glass_tr$Type,
    verbose = FALSE
  )
  sda_pred <-
    predict(
      sda_fit,
      glass_te |> dplyr::select(-factor) |> as.matrix(),
      verbose = FALSE
    )
  expect_no_error(
    d_fit <-
      discrim_linear() |>
      set_engine("sda") |>
      fit(Type ~ ., data = glass_tr |> dplyr::select(-factor))
  )
  expect_no_error(
    d_pred <- predict(
      d_fit,
      glass_te |> dplyr::select(-factor),
      type = "class"
    )
  )
  expect_no_error(
    d_prob <- predict(
      d_fit,
      glass_te |> dplyr::select(-factor),
      type = "prob"
    )
  )
  expect_equal(
    sda_pred$class,
    d_pred$.pred_class
  )

  expect_equal(
    sda_pred$posterior |> tibble::as_tibble(),
    d_prob,
    ignore_attr = TRUE
  )
})

test_that("sda engine arguments are tunable", {
  res <- discrim_linear() |>
    set_engine("sda") |>
    generics::tunable()
  eng_args <- c("lambda", "lambda.var", "lambda.freqs", "diagonal")
  res <- res[res$name %in% eng_args, ]
  expect_identical(res$name, eng_args)
  expect_identical(
    vapply(res$call_info, \(x) x$fun, character(1)),
    c(
      "shrinkage_correlation",
      "shrinkage_variance",
      "shrinkage_frequencies",
      "diagonal_covariance"
    )
  )
  expect_identical(
    vapply(res$call_info, \(x) x$pkg, character(1)),
    rep("dials", 4)
  )
})
