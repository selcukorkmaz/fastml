# yardstick's metric_set() always calls its members with `na_rm` and
# `case_weights`, and passes `event_level` to metrics that accept it. A metric
# wrapper that does not declare those as formals receives them in `...`, where
# fastml's probability metrics read `...` as a selection of probability columns.
# The metric then returns NA rather than erroring.
#
# The consequence was silent and wide. Every probability metric in the
# classification set (roc_auc, logloss, brier_score, ece) came back empty
# together while the six class metrics, whose wrappers already declared these
# arguments, were unaffected. Because roc_auc is the default selection metric,
# tuning could not choose a configuration, which is what disabled nested
# cross-validation whenever tuning was requested.

skip_if_not_installed("yardstick")

preds <- function(n = 120) {
  set.seed(11)
  d <- tibble::tibble(
    y = factor(sample(c("neg", "pos"), n, replace = TRUE), levels = c("neg", "pos")),
    .pred_pos = runif(n)
  )
  d$.pred_neg <- 1 - d$.pred_pos
  d$.pred_class <- factor(ifelse(d$.pred_pos > 0.5, "pos", "neg"),
                          levels = c("neg", "pos"))
  d
}

prob_metrics <- function(event_class = "first") {
  list(
    roc_auc     = fastml_configured_roc_auc("macro", event_class = event_class),
    logloss     = fastml_configured_logloss(event_class = event_class),
    brier_score = fastml_configured_brier_score(event_class = event_class),
    ece         = fastml_configured_ece(event_class = event_class)
  )
}

test_that("probability metrics declare the arguments metric_set passes", {
  for (m in prob_metrics()) {
    nms <- names(formals(m))
    expect_true("na_rm" %in% nms)
    expect_true("case_weights" %in% nms)
    expect_true("event_level" %in% nms)
  }
})

test_that("probability metrics survive the arguments metric_set passes", {
  d <- preds()
  for (nm in names(prob_metrics())) {
    m <- prob_metrics()[[nm]]
    # This is the call shape metric_set() uses. Before the fix, supplying
    # case_weights alone was enough to turn the result into NA.
    res <- m(d, truth = y, .pred_neg, na_rm = TRUE, case_weights = NULL)
    expect_true(is.finite(res$.estimate), info = nm)
  }
})

test_that("a mixed class and probability metric set returns no empty metrics", {
  d <- preds()
  ms <- yardstick::metric_set(
    fastml_wrap_basic_metric(yardstick::accuracy),
    fastml_wrap_event_level_metric(yardstick::sens, "first"),
    fastml_configured_roc_auc("macro", event_class = "first"),
    logloss     = fastml_configured_logloss(event_class = "first"),
    brier_score = fastml_configured_brier_score(event_class = "first"),
    ece         = fastml_configured_ece(event_class = "first")
  )
  out <- ms(d, truth = y, estimate = .pred_class, .pred_neg, .pred_pos)
  expect_equal(nrow(out), 6L)
  expect_true(all(is.finite(out$.estimate)))
})

test_that("the configured event_class governs, not the forwarded event_level", {
  # metric_set() forwards event_level = "first" whether or not the caller asked
  # for it. Honouring that would silently override an event_class of "second"
  # on every metric computed through a set, so the configured value governs and
  # the forwarded one is absorbed and ignored.
  d <- preds()
  m_second <- fastml_configured_roc_auc("macro", event_class = "second")
  configured <- m_second(d, truth = y, .pred_pos)$.estimate
  forwarded  <- m_second(d, truth = y, .pred_pos, event_level = "first")$.estimate
  expect_equal(configured, forwarded, tolerance = 1e-12)
  # And the two event_class settings genuinely differ on this data, so the
  # assertion above is not vacuous.
  m_first <- fastml_configured_roc_auc("macro", event_class = "first")
  expect_false(isTRUE(all.equal(
    m_first(d, truth = y, .pred_pos)$.estimate, configured
  )))
})
