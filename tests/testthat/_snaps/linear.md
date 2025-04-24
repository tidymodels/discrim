# check_args() works

    Code
      spec <- set_mode(set_engine(discrim_linear(penalty = -1), "MASS"),
      "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

