library(mlbench)
data(Glass)

set.seed(55822)
in_samp <- sample.int(nrow(Glass), 5)

# Add a random factor predictor to test dummy variables
Glass$factor <- factor(sample(letters[1:4], nrow(Glass), replace = TRUE))

glass_tr <- Glass[-in_samp, ]
glass_te <- Glass[in_samp, -10]
glass_na <- glass_te
glass_na$RI[1] <- NA
glass_na$Na[2] <- NA

glass_lvl <- levels(Glass$Type)
prob_names <- paste0(".pred_", glass_lvl)

# ------------------------------------------------------------------------------

probs_to_tibble <- function(x) {
  x <- tibble::as_tibble(x)
  names(x) <- paste0(".pred_", names(x))
  x
}
