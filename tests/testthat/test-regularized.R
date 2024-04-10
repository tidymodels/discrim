test_that('check_args() works', {
  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_regularized(frac_common_cov = -1) %>% 
        set_engine("klaR") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_regularized(frac_identity = -1) %>% 
        set_engine("klaR") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
})