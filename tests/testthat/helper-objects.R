# ------------------------------------------------------------------------------
# Glass data

if (rlang::is_installed("mlbench")) {
  library(mlbench)
  data(Glass, package = "mlbench", envir = environment())

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
  glass_prob_names <- paste0(".pred_", glass_lvl)

  wts <- ifelse(runif(nrow(glass_tr)) < .1, 0, 1)
  wts <- hardhat::importance_weights(wts)
}

# ------------------------------------------------------------------------------
# Penguin data

if (rlang::is_installed("modeldata")) {
  data(penguins, package = "modeldata", envir = environment())
  penguins$island <- NULL
  penguins_miss <- penguins
  penguins <- na.omit(penguins)
  in_train <- seq(1, nrow(penguins), by = 2)
  penguin_tr <- penguins[in_train, ]
  penguin_te <- penguins[-in_train, ]

  pen_prob_names <- c(".pred_Adelie", ".pred_Chinstrap", ".pred_Gentoo")
}

# ------------------------------------------------------------------------------
# LDA/QDA fits

if (rlang::is_installed(c("mlbench", "MASS"))) {
  lda_spec <- discrim_linear() |> set_engine("MASS")
  prior_spec_lda <- discrim_linear() |>
    set_engine("MASS", prior = rep(1 / 6, 6))

  exp_f_fit_lda <- MASS::lda(Type ~ ., data = glass_tr)
  exp_xy_fit_lda <- MASS::lda(x = glass_tr[, -10], grouping = glass_tr$Type)
  exp_prior_fit_lda <- MASS::lda(
    Type ~ .,
    data = glass_tr,
    prior = rep(1 / 6, 6)
  )

  ###

  qda_spec <- discrim_quad() |> set_engine("MASS")
  prior_spec_qda <- discrim_quad() |> set_engine("MASS", prior = rep(1 / 3, 3))

  exp_f_fit_qda <- MASS::qda(species ~ ., data = penguin_tr)
  exp_xy_fit_qda <- MASS::qda(
    x = penguin_tr[, -1],
    grouping = penguin_tr$species
  )
  exp_prior_fit_qda <- MASS::qda(
    species ~ .,
    data = penguin_tr,
    prior = rep(1 / 3, 3)
  )
}

# ------------------------------------------------------------------------------
# RDA fits

if (rlang::is_installed(c("mlbench", "klaR"))) {
  rda_spec <-
    discrim_regularized(frac_common_cov = .1, frac_identity = 1) |>
    set_engine("klaR")

  prior_spec_rda <- discrim_regularized() |>
    set_engine("klaR", prior = rep(1 / 6, 6))

  exp_f_fit_rda <- klaR::rda(Type ~ ., data = glass_tr, lambda = .1, gamma = 1)
}


# ------------------------------------------------------------------------------
# FDA fits

if (rlang::is_installed(c("mlbench", "mda", "earth"))) {
  fda_spec <- discrim_flexible(num_terms = 7) |> set_engine("earth")

  exp_f_fit_fda <- mda::fda(
    Type ~ .,
    data = glass_tr,
    method = earth::earth,
    nprune = 7
  )

  exp_f_wts_fit_fda <- mda::fda(
    Type ~ .,
    data = glass_tr,
    weights = as.double(wts),
    method = earth::earth,
    nprune = 7
  )
}

if (rlang::is_installed(c("mlbench", "mda"))) {
  lda_fda_spec <- discrim_linear(penalty = 1) |> set_engine("mda")
  prior_lda_fda_spec <- discrim_linear() |>
    set_engine("mda", prior = rep(1 / 6, 6))

  exp_f_fit_lda_fda <- mda::fda(
    Type ~ .,
    data = glass_tr,
    method = mda::gen.ridge,
    lambda = 1
  )

  exp_f_wts_fit_lda_fda <- mda::fda(
    Type ~ .,
    data = glass_tr,
    weights = as.double(wts),
    method = mda::gen.ridge,
    lambda = 1
  )
}

# ------------------------------------------------------------------------------

probs_to_tibble <- function(x) {
  x <- tibble::as_tibble(x)
  names(x) <- paste0(".pred_", names(x))
  x
}
