# missing data

    Code
      f_pred <- predict(f_fit, glass_na, type = "prob")
    Warning <simpleWarning>
      no non-missing arguments to min; returning Inf
      no non-missing arguments to min; returning Inf

---

    Code
      exp_f_pred <- probs_to_tibble(predict(exp_f_fit, glass_na)$posterior)
    Warning <simpleWarning>
      no non-missing arguments to min; returning Inf
      no non-missing arguments to min; returning Inf

