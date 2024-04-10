# check_args() works

    Code
      spec <- discrim_regularized(frac_common_cov = -1) %>% set_engine("klaR") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `frac_common_cov` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- discrim_regularized(frac_identity = -1) %>% set_engine("klaR") %>%
        set_mode("classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `frac_identity` must be a number between 0 and 1 or `NULL`, not the number -1.

