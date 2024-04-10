# check_args() works

    Code
      spec <- discrim_linear(penalty = -1) %>% set_engine("MASS") %>% set_mode(
        "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! The amount of regularization, `penalty`, should be `>= 0`.

