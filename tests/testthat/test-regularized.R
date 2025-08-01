test_that('check_args() works', {
  skip_if_not_installed("parsnip", "1.2.1.9001")
  skip_if_not_installed("mlbench")
  skip_if_not_installed("klaR")

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_regularized(frac_common_cov = -1) |>
        set_engine("klaR") |>
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_regularized(frac_identity = -1) |>
        set_engine("klaR") |>
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
})
