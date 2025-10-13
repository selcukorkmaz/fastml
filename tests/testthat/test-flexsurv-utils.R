library(testthat)


test_that("fastml_flexsurv_survival_matrix preserves factor structure", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")

  data(lung, package = "survival")
  lung <- stats::na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog")])
  lung$sex <- factor(lung$sex, levels = c(1, 2), labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog)
  set.seed(123)
  lung$group <- factor(sample(c("A", "B", "C"), nrow(lung), replace = TRUE),
                       levels = c("A", "B", "C", "D"))

  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex + ph.ecog + group,
    data = lung,
    dist = "weibull"
  )

  newdata <- lung[seq_len(min(6L, nrow(lung))), c("age", "sex", "ph.ecog", "group")]
  levels(newdata$group) <- levels(lung$group)
  times <- c(30, 90, 180, 365)

  surv_mat <- fastml:::fastml_flexsurv_survival_matrix(fit, newdata, times)

  expect_true(is.matrix(surv_mat))
  expect_identical(dim(surv_mat), c(nrow(newdata), length(times)))
  expect_true(all(rowSums(is.finite(surv_mat)) > 0))
  expect_true(all(surv_mat[is.finite(surv_mat)] >= 0))
  expect_true(all(surv_mat[is.finite(surv_mat)] <= 1))
})


test_that("fastml_flexsurv_survival_matrix handles empty newdata", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")

  data(lung, package = "survival")
  lung <- stats::na.omit(lung[, c("time", "status", "age", "sex")])
  lung$sex <- factor(lung$sex, levels = c(1, 2), labels = c("male", "female"))

  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex,
    data = lung,
    dist = "weibull"
  )

  times <- c(30, 90, 180)
  empty_newdata <- lung[0, c("age", "sex")]

  surv_mat <- fastml:::fastml_flexsurv_survival_matrix(fit, empty_newdata, times)

  expect_true(is.matrix(surv_mat))
  expect_identical(dim(surv_mat), c(0L, length(times)))
})
