test_that('check_args() works', {
  skip_if_not_installed("parsnip", "1.2.1.9001")

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_linear(penalty = -1) |>
        set_engine("MASS") |>
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
})
