# mda::fda/earth check_args() works

    Code
      spec <- discrim_flexible(prod_degree = 0) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `prod_degree` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- discrim_flexible(num_terms = 0) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `num_terms` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- discrim_flexible(prune_method = 2) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, factor ~ ., glass_tr)
    Condition
      Error in `fit()`:
      ! `prune_method` must be a single string or `NULL`, not the number 2.

