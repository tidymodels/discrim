test_that('param ranges', {
  expect_equal(frac_common_cov(c(.1, .5))$range, list(lower = .1, upper = .5))
  expect_equal(frac_identity(c(.1, .5))$range, list(lower = .1, upper = .5))
  expect_equal(smoothness(c(.1, .5))$range, list(lower = .1, upper = .5))
})
