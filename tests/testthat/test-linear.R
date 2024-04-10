test_that('check_args() works', {
  expect_snapshot(
    error = TRUE,
    {
      spec <- discrim_linear(penalty = -1) %>% 
        set_engine("MASS") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    }
  )
})