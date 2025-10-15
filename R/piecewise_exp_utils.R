#' Internal helpers for piecewise exponential survival models
#'
#' These utilities construct a custom piecewise exponential distribution for
#' flexsurv::flexsurvreg() along with supporting density, distribution,
#' quantile, hazard, and simulation routines. They are internal to fastml.
#'
#' @noRd

fastml_piecewise_make_distribution <- function(breaks = NULL) {
  cuts <- fastml_piecewise_normalize_breaks(breaks)
  n_intervals <- length(cuts) + 1L
  param_names <- c("log_rate", if (n_intervals > 1L) {
    paste0("log_ratio_", seq_len(n_intervals - 1L))
  })
  transforms <- replicate(length(param_names), identity, simplify = FALSE)
  inv_transforms <- transforms

  inits_fun <- function(t) {
    tt <- as.numeric(t)
    tt <- tt[is.finite(tt) & tt > 0]
    base_log_rate <- if (length(tt) == 0) 0 else log(1 / mean(tt))
    c(base_log_rate, rep(0, length(param_names) - 1L))
  }

  dlist <- list(
    name = "fastml_piecewise_exponential",
    pars = param_names,
    location = "log_rate",
    transforms = transforms,
    inv.transforms = inv_transforms,
    inits = inits_fun
  )

  dfns <- fastml_piecewise_dfns_factory(cuts, param_names)

  aux <- if (length(cuts) > 0) list(cuts = cuts) else list()

  list(dlist = dlist, dfns = dfns, aux = aux)
}

fastml_piecewise_normalize_breaks <- function(breaks) {
  if (is.null(breaks) || length(breaks) == 0) {
    return(numeric(0))
  }
  vals <- as.numeric(breaks)
  vals <- vals[is.finite(vals) & vals > 0]
  vals <- sort(unique(vals))
  vals
}

fastml_piecewise_dfns_factory <- function(cuts, param_names) {
  delta_names <- if (length(param_names) > 1L) param_names[-1L] else character(0)

  list(
    d = fastml_piecewise_density_fn(cuts, delta_names),
    p = fastml_piecewise_cdf_fn(cuts, delta_names),
    q = fastml_piecewise_quantile_fn(cuts, delta_names),
    r = fastml_piecewise_random_fn(cuts, delta_names),
    h = fastml_piecewise_hazard_fn(cuts, delta_names),
    H = fastml_piecewise_cumhaz_fn(cuts, delta_names)
  )
}

fastml_piecewise_extract_deltas <- function(extra, delta_names) {
  if (length(delta_names) == 0L) {
    return(numeric(0))
  }
  vapply(delta_names, function(nm) {
    val <- extra[[nm]]
    if (is.null(val)) {
      0
    } else {
      as.numeric(val)[1]
    }
  }, numeric(1))
}

fastml_piecewise_rates <- function(log_rate, log_ratios) {
  pars <- c(log_rate, log_ratios)
  if (any(!is.finite(pars))) {
    return(rep(NA_real_, length(log_ratios) + 1L))
  }
  base_rate <- exp(log_rate)
  c(base_rate, base_rate * exp(log_ratios))
}

fastml_piecewise_interval_starts <- function(cuts) {
  if (length(cuts) == 0L) {
    return(0)
  }
  c(0, cuts)
}

fastml_piecewise_cumhaz_starts <- function(rates, cuts) {
  if (length(rates) <= 1L || length(cuts) == 0L) {
    return(rep(0, length(rates)))
  }
  seg_lengths <- diff(c(0, cuts))
  if (length(seg_lengths) >= length(rates)) {
    seg_lengths <- seg_lengths[seq_len(length(rates) - 1L)]
  }
  accum <- cumsum(rates[seq_len(length(seg_lengths))] * seg_lengths)
  c(0, accum)
}

fastml_piecewise_cumhaz_core <- function(times, rates, cuts) {
  times <- as.numeric(times)
  res <- rep(NA_real_, length(times))
  if (length(times) == 0L) {
    return(res)
  }
  if (any(is.na(rates))) {
    return(res)
  }
  idx_valid <- which(is.finite(times) & times >= 0)
  if (length(idx_valid) > 0L) {
    tvals <- times[idx_valid]
    interval_idx <- findInterval(tvals, cuts) + 1L
    starts <- fastml_piecewise_interval_starts(cuts)
    cumhaz_starts <- fastml_piecewise_cumhaz_starts(rates, cuts)
    contrib <- rates[interval_idx] * (tvals - starts[interval_idx])
    res[idx_valid] <- cumhaz_starts[interval_idx] + contrib
  }
  res[is.finite(times) & times < 0] <- 0
  res
}

fastml_piecewise_survival_core <- function(times, rates, cuts) {
  H <- fastml_piecewise_cumhaz_core(times, rates, cuts)
  ifelse(is.na(H), NA_real_, exp(-pmax(H, 0)))
}

fastml_piecewise_density_core <- function(times, rates, cuts) {
  surv <- fastml_piecewise_survival_core(times, rates, cuts)
  times <- as.numeric(times)
  dens <- rep(NA_real_, length(times))
  idx_valid <- which(is.finite(times) & times >= 0)
  if (length(idx_valid) > 0L && !any(is.na(rates))) {
    tvals <- times[idx_valid]
    interval_idx <- findInterval(tvals, cuts) + 1L
    dens[idx_valid] <- rates[interval_idx] * surv[idx_valid]
  }
  dens[is.finite(times) & times < 0] <- 0
  dens
}

fastml_piecewise_cdf_core <- function(times, rates, cuts) {
  surv <- fastml_piecewise_survival_core(times, rates, cuts)
  ifelse(is.na(surv), NA_real_, pmin(pmax(1 - surv, 0), 1))
}

fastml_piecewise_hazard_core <- function(times, rates, cuts) {
  times <- as.numeric(times)
  haz <- rep(NA_real_, length(times))
  idx_valid <- which(is.finite(times) & times >= 0)
  if (length(idx_valid) > 0L && !any(is.na(rates))) {
    tvals <- times[idx_valid]
    interval_idx <- findInterval(tvals, cuts) + 1L
    haz[idx_valid] <- rates[interval_idx]
  }
  haz[is.finite(times) & times < 0] <- 0
  haz
}

fastml_piecewise_quantile_core <- function(probs, rates, cuts) {
  probs <- as.numeric(probs)
  res <- rep(NA_real_, length(probs))
  if (length(probs) == 0L || any(is.na(rates))) {
    return(res)
  }
  invalid <- probs < 0 | probs > 1
  res[invalid] <- NaN
  zero_idx <- which(!invalid & probs <= 0)
  if (length(zero_idx) > 0L) {
    res[zero_idx] <- 0
  }
  one_idx <- which(!invalid & probs >= 1)
  if (length(one_idx) > 0L) {
    res[one_idx] <- Inf
  }
  idx <- which(!invalid & probs > 0 & probs < 1)
  if (length(idx) > 0L) {
    target <- -log1p(-probs[idx])
    cumhaz_starts <- fastml_piecewise_cumhaz_starts(rates, cuts)
    cumhaz_ends <- if (length(rates) > 1L && length(cuts) > 0L) {
      cumhaz_starts[-1L]
    } else {
      numeric(0)
    }
    cumhaz_bounds <- c(cumhaz_ends, Inf)
    interval_idx <- findInterval(target, cumhaz_bounds) + 1L
    starts <- fastml_piecewise_interval_starts(cuts)
    res[idx] <- starts[interval_idx] +
      (target - cumhaz_starts[interval_idx]) / rates[interval_idx]
  }
  res
}

fastml_piecewise_random_core <- function(n, rates, cuts) {
  if (n <= 0 || any(is.na(rates))) {
    return(numeric(0))
  }
  u <- stats::runif(n)
  fastml_piecewise_quantile_core(u, rates, cuts)
}

fastml_piecewise_density_fn <- function(cuts, delta_names) {
  function(x, log_rate, ..., log = FALSE) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    dens <- fastml_piecewise_density_core(x, rates, cuts)
    if (log) {
      log(dens)
    } else {
      dens
    }
  }
}

fastml_piecewise_cdf_fn <- function(cuts, delta_names) {
  function(q, log_rate, ..., lower.tail = TRUE, log.p = FALSE) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    cdf <- fastml_piecewise_cdf_core(q, rates, cuts)
    if (!lower.tail) {
      cdf <- 1 - cdf
    }
    cdf <- pmin(pmax(cdf, 0), 1)
    if (log.p) {
      log(cdf)
    } else {
      cdf
    }
  }
}

fastml_piecewise_quantile_fn <- function(cuts, delta_names) {
  function(p, log_rate, ..., lower.tail = TRUE, log.p = FALSE) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    probs <- as.numeric(p)
    if (log.p) {
      probs <- exp(probs)
    }
    if (!lower.tail) {
      probs <- 1 - probs
    }
    fastml_piecewise_quantile_core(probs, rates, cuts)
  }
}

fastml_piecewise_random_fn <- function(cuts, delta_names) {
  function(n, log_rate, ...) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    n_val <- as.integer(n[1])
    if (is.na(n_val) || n_val <= 0) {
      return(numeric(0))
    }
    fastml_piecewise_random_core(n_val, rates, cuts)
  }
}

fastml_piecewise_hazard_fn <- function(cuts, delta_names) {
  function(x, log_rate, ...) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    fastml_piecewise_hazard_core(x, rates, cuts)
  }
}

fastml_piecewise_cumhaz_fn <- function(cuts, delta_names) {
  function(x, log_rate, ...) {
    extra <- list(...)
    deltas <- fastml_piecewise_extract_deltas(extra, delta_names)
    rates <- fastml_piecewise_rates(log_rate, deltas)
    fastml_piecewise_cumhaz_core(x, rates, cuts)
  }
}
