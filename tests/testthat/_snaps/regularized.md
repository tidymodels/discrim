# check_args() works

    Code
      spec <- set_mode(set_engine(discrim_regularized(frac_common_cov = -1), "klaR"),
      "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `frac_common_cov` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- set_mode(set_engine(discrim_regularized(frac_identity = -1), "klaR"),
      "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `frac_identity` must be a number between 0 and 1 or `NULL`, not the number -1.

