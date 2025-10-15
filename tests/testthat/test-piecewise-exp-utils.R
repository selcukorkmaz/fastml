test_that("fastml_piecewise_rates handles scalar inputs", {
  skip_if_not_installed("flexsurv")
  base_log_rate <- log(0.1)
  log_ratios <- log(c(2, 3))
  res <- fastml:::fastml_piecewise_rates(base_log_rate, log_ratios)
  expect_length(res, length(log_ratios) + 1L)
  expect_equal(res, exp(base_log_rate) * c(1, exp(log_ratios)))
})

test_that("fastml_piecewise_rates accepts vectorised log_rate from flexsurv", {
  skip_if_not_installed("flexsurv")
  base_log_rate <- log(0.2)
  extra_params <- log(c(0.3, 0.4))
  res <- fastml:::fastml_piecewise_rates(c(base_log_rate, extra_params), numeric(0))
  expect_length(res, length(extra_params) + 1L)
  expect_equal(res[1], exp(base_log_rate))
  expect_equal(res[-1], exp(base_log_rate) * exp(extra_params))
})
