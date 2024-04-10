# missing data

    Code
      f_pred <- predict(f_fit, glass_na, type = "prob")
    Condition
      Warning in `FUN()`:
      no non-missing arguments to min; returning Inf
      Warning in `FUN()`:
      no non-missing arguments to min; returning Inf

---

    Code
      exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na)$posterior)
    Condition
      Warning in `FUN()`:
      no non-missing arguments to min; returning Inf
      Warning in `FUN()`:
      no non-missing arguments to min; returning Inf

