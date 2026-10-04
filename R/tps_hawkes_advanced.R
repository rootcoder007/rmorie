# SPDX-License-Identifier: AGPL-3.0-or-later
#' Non-stationary Hawkes with non-exponential kernels (R port)
#'
#' R port of \code{morie.tps_hawkes_advanced}. Implements the Kwan,
#' Chen and Dunsmuir (2024, arXiv:2408.09710v1) methodology for Hawkes
#' process likelihood inference when the baseline intensity is
#' time-varying \emph{and} the excitation kernel is non-exponential
#' (so the intensity process is non-Markovian).
#'
#' The complete intensity is
#' \deqn{\lambda(t) = \mu(t) + \int_0^{t-} g(t - s) \, dN_s,}{lambda(t) = u(t) + int_0^t-
#' g(t - s) dN_s,}
#' with kernel decomposition \eqn{g(u) = \eta \cdot \tilde g(u)}{g(u) = eta * tilde g(u)} where
#' \eqn{\eta \in (0, 1)}{eta in (0, 1)} is the branching ratio (mean offspring per
#' event) and \eqn{\tilde g}{tilde g} is a probability density on
#' \eqn{[0, \infty)}{[0, inf)}. Stationarity requires \eqn{\eta < 1}{eta < 1}.
#'
#' Supported kernels: exponential, gamma, Weibull, Lomax (Pareto-II).
#' Supported baselines: constant and sinusoidal-with-trend
#' \deqn{\mu(t) = \exp\bigl(a_0 + a_1 (t/T) + a_2 \sin(2\pi t / 365.25)
#'        + a_3 \cos(2\pi t / 365.25)\bigr).}{u(t) = expbigl(a_0 + a_1 (t/T) + a_2
#' sin(2pi t / 365.25) + a_3 cos(2pi t / 365.25)bigr).}
#'
#' Companion to \code{morie_tps_hawkes_temporal_fit} (exponential /
#' constant Markovian special case) in \code{morie.tps_stochastic}.
#'
#' Goodness-of-fit uses time-rescaling residuals (Brown \emph{et al.}
#' 2002 \emph{Neural Comput.} 14: 325-346) and a Kolmogorov-Smirnov
#' test against Uniform(0,1).
#'
#' If the optional R package \pkg{hawkes} or \pkg{emhawkes} is
#' installed it is consulted for the exponential-kernel constant-
#' baseline fast path; otherwise the negative log-likelihood is
#' computed in base R via direct O(n^2) summation. The non-Markovian
#' kernels (gamma, Weibull, Lomax) always use the base-R path -- those
#' kernels lack the memorylessness required for O(n) recursion.
#'
#' Functions
#'
#' \itemize{
#'   \item \code{\link{morie_tps_hawkes_advanced_fit}} -- fit one
#'     (kernel, baseline) combination and produce a rich result with
#'     time-rescaling KS diagnostics.
#'   \item \code{\link{morie_tps_compare_hawkes_kernels}} -- 8-way AIC
#'     comparison across (kernel, baseline) combinations.
#'   \item \code{\link{morie_tps_hawkes_markovian_vs_nonmarkovian}} --
#'     focused 2x2 comparison: classical exp/const vs gamma/
#'     sinusoidal.
#' }
#'
#' @references
#' Kwan TKJ, Chen F, Dunsmuir WTM (2024). Likelihood inference for
#' non-stationary Hawkes processes. arXiv:2408.09710v1.
#'
#' Brown EN, Barbieri R, Ventura V, Kass RE, Frank LM (2002). The
#' time-rescaling theorem and its application to neural spike train
#' data analysis. \emph{Neural Computation} 14: 325-346.
#'
#' Mohler GO, Short MB, Brantingham PJ, Schoenberg FP, Tita GE (2011).
#' Self-exciting point process modeling of crime. \emph{Journal of
#' the American Statistical Association} 106: 100-108.
#'
#' @name morie_tps_hawkes_advanced
NULL


# ---------------------------------------------------------------------------
# Constants and helpers
# ---------------------------------------------------------------------------

.TPS_HAWKES_KERNELS   <- c("exponential", "gamma", "weibull", "lomax")
.TPS_HAWKES_BASELINES <- c("constant", "sinusoidal")

#' Internal helper: Tps Hwka Result
#' @noRd
.tps_hwka_result <- function(title, summary_lines = list(),
                              warnings = character(0),
                              interpretation = "",
                              payload = list()) {
  out <- list(
    title          = title,
    summary_lines  = summary_lines,
    warnings       = warnings,
    interpretation = interpretation,
    payload        = payload
  )
  class(out) <- c("morie_tps_hawkes_advanced_result",
                   "morie_rich_result", "list")
  out
}

#' Internal helper: Tps Hwka N Kernel Params
#' @noRd
.tps_hwka_n_kernel_params <- function(kind) {
  if (identical(kind, "exponential")) 1L else 2L
}

#' Internal helper: Tps Hwka N Baseline Params
#' @noRd
.tps_hwka_n_baseline_params <- function(kind) {
  if (identical(kind, "constant")) 1L else 4L
}


# ---------------------------------------------------------------------------
# Kernel densities and CDFs (vectorised; mirrors the Python branch table)
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Cpp Ok
#' @noRd
.tps_hwka_cpp_ok <- function() {
  exists("morie_hawkes_pair_excitation_sum_cpp",
         envir = asNamespace("rmorie"), inherits = FALSE)
}

#' Internal helper: Tps Hwka Kernel Density
#' @noRd
.tps_hwka_kernel_density <- function(u, kind, psi) {
  if (.tps_hwka_cpp_ok()) {
    return(morie_hawkes_kernel_density_cpp(as.numeric(u), kind,
                                            as.numeric(psi)))
  }
  u <- as.numeric(u)
  switch(kind,
    exponential = {
      beta <- psi[1]
      beta * exp(-beta * u)
    },
    gamma = {
      alpha <- psi[1]
      beta <- psi[2]
      log_d <- alpha * log(beta) +
               (alpha - 1) * log(pmax(u, 1e-300)) -
               beta * u - lgamma(alpha)
      exp(log_d)
    },
    weibull = {
      alpha <- psi[1]
      lam <- psi[2]
      x <- u / lam
      (alpha / lam) * pmax(x, 1e-300) ^ (alpha - 1) *
        exp(-x ^ alpha)
    },
    lomax = {
      alpha <- psi[1]
      c_ <- psi[2]
      # v0.9.5.6+: scipy.stats.lomax convention.
      # log-space: alpha * c^alpha * (u + c)^{-(alpha+1)}
      log_d <- log(alpha) + alpha * log(c_) -
               (alpha + 1) * log(u + c_)
      exp(log_d)
    },
    stop(sprintf("unknown kernel kind: %s", kind))
  )
}

#' Internal helper: Tps Hwka Kernel Cdf
#' @noRd
.tps_hwka_kernel_cdf <- function(u, kind, psi) {
  if (.tps_hwka_cpp_ok()) {
    return(morie_hawkes_kernel_cdf_cpp(as.numeric(u), kind,
                                        as.numeric(psi)))
  }
  u <- as.numeric(u)
  switch(kind,
    exponential = 1 - exp(-psi[1] * u),
    gamma = stats::pgamma(u, shape = psi[1], rate = psi[2]),
    weibull = 1 - exp(-(u / psi[2]) ^ psi[1]),
    lomax = 1 - (psi[2] / (u + psi[2])) ^ psi[1],  # v0.9.5.6+: scipy
    stop(sprintf("unknown kernel kind: %s", kind))
  )
}


# ---------------------------------------------------------------------------
# Baseline nu(t) and its integral
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Baseline
#' @noRd
.tps_hwka_baseline <- function(t, kind, alpha, T_) {
  t <- as.numeric(t)
  switch(kind,
    constant = rep(exp(alpha[1]), length(t)),
    sinusoidal = exp(alpha[1] +
                     alpha[2] * (t / max(T_, 1)) +
                     alpha[3] * sin(2 * pi * t / 365.25) +
                     alpha[4] * cos(2 * pi * t / 365.25)),
    stop(sprintf("unknown baseline kind: %s", kind))
  )
}

#' Internal helper: Tps Hwka Baseline Integral
#' @noRd
.tps_hwka_baseline_integral <- function(T_, kind, alpha) {
  if (identical(kind, "constant")) return(exp(alpha[1]) * T_)
  if (identical(kind, "sinusoidal") && .tps_hwka_cpp_ok()) {
    n_grid <- max(64L, as.integer(T_) + 1L)
    return(morie_hawkes_baseline_integral_cpp(as.numeric(T_),
                                               as.numeric(alpha),
                                               n_grid))
  }
  grid <- seq(0, T_, length.out = max(64L, as.integer(T_) + 1L))
  vals <- .tps_hwka_baseline(grid, kind, alpha, T_)
  # Trapezoidal rule.
  sum(0.5 * (vals[-1] + vals[-length(vals)]) * diff(grid))
}


# ---------------------------------------------------------------------------
# Negative log-likelihood (general, base-R O(n^2))
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Split Theta
#' @noRd
.tps_hwka_split_theta <- function(theta, kernel_kind, baseline_kind) {
  nb <- .tps_hwka_n_baseline_params(baseline_kind)
  nk <- .tps_hwka_n_kernel_params(kernel_kind)
  if (length(theta) != nb + 1L + nk) {
    stop(sprintf("expected %d params, got %d",
                 nb + 1L + nk, length(theta)))
  }
  list(a   = theta[seq_len(nb)],
       eta = theta[nb + 1L],
       psi = theta[(nb + 2L):length(theta)])
}

#' Internal helper: Tps Hwka Neg Loglik General
#' @noRd
.tps_hwka_neg_loglik_general <- function(theta, t, T_,
                                          kernel_kind,
                                          baseline_kind) {
  # Module 21: the native base-R/C++ path below IS the estimator for
  # every kernel/baseline combination (the former hawkes/emhawkes
  # exponential fast path was removed; tests/cross validates the
  # exponential-kernel log-likelihood against hawkes::likelihoodHawkes
  # where installed).
  nb <- .tps_hwka_n_baseline_params(baseline_kind)
  nk <- .tps_hwka_n_kernel_params(kernel_kind)
  a   <- theta[seq_len(nb)]
  eta <- theta[nb + 1L]
  psi <- theta[(nb + 2L):length(theta)]

  if (eta <= 1e-6 || eta >= 0.999) return(1e12)
  if (any(psi <= 1e-6)) return(1e12)
  if (identical(kernel_kind, "lomax") && psi[1] <= 1.001) {
    return(1e12)  # need alpha > 1 for finite mean (scipy convention)
  }

  n <- length(t)
  nu_at_t <- .tps_hwka_baseline(t, baseline_kind, a, T_)
  if (.tps_hwka_cpp_ok()) {
    excite <- morie_hawkes_pair_excitation_sum_cpp(as.numeric(t),
                                                    as.numeric(eta),
                                                    kernel_kind,
                                                    as.numeric(psi))
    lam <- nu_at_t + excite
    if (any(lam <= 0)) return(1e12)
    log_sum <- sum(log(lam))
  } else {
    log_sum <- 0
    for (i in seq_len(n)) {
      if (i == 1L) {
        lam_i <- nu_at_t[1]
      } else {
        lags <- t[i] - t[seq_len(i - 1L)]
        lam_i <- nu_at_t[i] +
                 eta * sum(.tps_hwka_kernel_density(lags, kernel_kind, psi))
      }
      if (lam_i <= 0) return(1e12)
      log_sum <- log_sum + log(lam_i)
    }
  }
  integral <- .tps_hwka_baseline_integral(T_, baseline_kind, a) +
              eta * sum(.tps_hwka_kernel_cdf(T_ - t, kernel_kind, psi))
  -(log_sum - integral)
}


# ---------------------------------------------------------------------------
# Initial-guess heuristic
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka X0
#' @noRd
.tps_hwka_x0 <- function(kernel_kind, baseline_kind, n, T_, mean_dt) {
  rate <- max(n / T_, 1e-3)
  a <- if (identical(baseline_kind, "constant")) {
    log(rate * 0.6)
  } else {
    c(log(rate * 0.6), 0, 0, 0)
  }
  eta <- 0.4
  psi <- switch(kernel_kind,
    exponential = 1 / max(mean_dt, 1e-3),
    gamma = c(1.5, 1 / max(mean_dt, 1e-3)),
    weibull = c(1.5, max(mean_dt, 1e-3) * 1.2),
    lomax = c(2.5, max(mean_dt, 1e-3) * 5)
  )
  c(a, eta, psi)
}


# ---------------------------------------------------------------------------
# Time-rescaling residuals (Brown et al. 2002)
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Time Rescaling
#' @noRd
.tps_hwka_time_rescaling <- function(theta, t, T_,
                                      kernel_kind, baseline_kind) {
  nb <- .tps_hwka_n_baseline_params(baseline_kind)
  a   <- theta[seq_len(nb)]
  eta <- theta[nb + 1L]
  psi <- theta[(nb + 2L):length(theta)]
  n <- length(t)
  bl_grid <- seq(0, T_, length.out = max(256L, as.integer(T_) + 1L))
  bl_vals <- .tps_hwka_baseline(bl_grid, baseline_kind, a, T_)
  cum_baseline <- c(0,
                     cumsum(0.5 * (bl_vals[-1] + bl_vals[-length(bl_vals)]) *
                              diff(bl_grid)))
  Lambda_at <- function(ti) {
    bl <- stats::approx(bl_grid, cum_baseline, xout = ti,
                         rule = 2)$y
    prior <- t[t < ti]
    excite <- eta *
              sum(.tps_hwka_kernel_cdf(ti - prior, kernel_kind, psi))
    bl + excite
  }
  inc <- numeric(n)
  prev <- 0
  for (i in seq_len(n)) {
    cur <- Lambda_at(as.numeric(t[i]))
    inc[i] <- max(cur - prev, 1e-12)
    prev <- cur
  }
  1 - exp(-inc)
}


# ---------------------------------------------------------------------------
# Public single-fit driver
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Fit One
#' @noRd
.tps_hwka_fit_one <- function(t, T_, kernel_kind, baseline_kind, method = "auto", eps = 1e-9) {
  # the shared C++ fit (rmoriebricklayer::core_hawkes_fit: analytic gradient, projected BFGS, the
  # exact / sum-of-exponentials / truncated / EM / INAR routes) -- the routine morie's Python
  # fit_hawkes_general calls, so both arms return the same estimate from the same events
  fit <- rmoriebricklayer::core_hawkes_fit(as.numeric(t), T_, kernel_kind, baseline_kind, method = method, eps = eps)
  t <- as.numeric(t)
  t <- t[t >= 0 & t < T_]
  u <- rmoriebricklayer::core_hawkes_residuals(t, T_, kernel_kind, fit$theta, baseline = baseline_kind)
  list(
    theta              = as.numeric(fit$theta),
    baseline_params    = as.numeric(fit$baseline_params),
    branching_ratio    = as.numeric(fit$branching_ratio),
    kernel_params      = as.numeric(fit$kernel_params),
    nll                = fit$nll,
    aic                = fit$aic,
    bic                = fit$bic,
    n                  = fit$n,
    T_days             = as.numeric(T_),
    k_params           = as.integer(fit$k_params),
    ks_stat            = as.numeric(fit$ks_stat),
    ks_pvalue          = as.numeric(fit$ks_pvalue),
    rescaled_uniforms  = utils::head(u, 1000L),
    kernel_kind        = kernel_kind,
    baseline_kind      = baseline_kind,
    method             = fit$method,
    eps                = fit$eps,
    converged          = isTRUE(fit$converged)
  )
}


# ---------------------------------------------------------------------------
# Event-to-days conversion (TPS daily resolution + ties jitter)
# ---------------------------------------------------------------------------

#' Internal helper: Tps Hwka Events To Days
#' @noRd
.tps_hwka_events_to_days <- function(df, max_n = NULL, min_year = 2014L) {
  # as morie's Python _events_to_days / _date_series, step for step, so both arms fit the same
  # vector: local dates from OCC_YEAR / OCC_MONTH / OCC_DAY when present (the ArcGIS feed turns
  # OCC_DATE into UTC, moving late-evening events to the next day), else OCC_DATE / REPORT_DATE
  # (epoch milliseconds or text); years before min_year dropped; an optional subsample of the
  # max_n events with the smallest splitmix keys (seed 43); a stable sort; the splitmix64
  # within-day jitter (seed 42) -- rmoriebricklayer::core_uniforms, the same numbers as Python's
  dt <- NULL
  if (all(c("OCC_YEAR", "OCC_MONTH", "OCC_DAY") %in% colnames(df))) {
    mon <- as.character(df$OCC_MONTH)
    mnum <- suppressWarnings(as.integer(mon))
    by_name <- match(tolower(trimws(mon)), tolower(month.name))
    by_abb <- match(tolower(substr(trimws(mon), 1L, 3L)), tolower(month.abb))
    mnum <- ifelse(is.na(mnum), ifelse(is.na(by_name), by_abb, by_name), mnum)
    d <- suppressWarnings(as.POSIXct(sprintf("%04d-%02d-%02d", as.integer(df$OCC_YEAR), mnum,
                                             as.integer(df$OCC_DAY)), tz = "UTC", format = "%Y-%m-%d"))
    d <- d[!is.na(d)]
    if (length(d)) dt <- d
  }
  if (is.null(dt)) {
    date_col <- intersect(c("OCC_DATE", "REPORT_DATE"), colnames(df))[1]
    if (is.na(date_col)) stop("no OCC_DATE or REPORT_DATE column found", call. = FALSE)
    x <- df[[date_col]]
    dt <- if (is.numeric(x)) as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC") else .morie_tps_parse_datetime(x)
    dt <- dt[!is.na(dt)]
  }
  if (!is.null(min_year)) dt <- dt[as.integer(format(dt, "%Y", tz = "UTC")) >= min_year]
  if (!is.null(max_n) && is.finite(max_n) && length(dt) > max_n) {
    keys <- rmoriebricklayer::core_uniforms(length(dt), 43)
    dt <- dt[sort(order(keys)[seq_len(max_n)])]
  }
  dt <- dt[order(as.numeric(dt), method = "radix")]
  t <- as.numeric(difftime(dt, dt[[1]], units = "days")) + rmoriebricklayer::core_uniforms(length(dt), 42)
  t <- sort(t)
  list(t = t, T_ = as.numeric(t[length(t)]))
}


# ---------------------------------------------------------------------------
# Public: fit one (kernel, baseline) combination
# ---------------------------------------------------------------------------

#' Fit a single (kernel, baseline) Hawkes specification
#'
#' Companion to the Markovian exponential / constant fit in
#' \code{morie_tps_hawkes_temporal_fit}. Supports the four kernels
#' (exponential, gamma, Weibull, Lomax) and two baselines (constant,
#' sinusoidal) of Kwan-Chen-Dunsmuir (2024).
#'
#' The fit is rmoriebricklayer's \code{core_hawkes_fit()} (the C++ routine morie's Python
#' \code{fit_hawkes_general} calls): maximum likelihood with the analytic gradient by projected
#' BFGS under box constraints, from the default start and from the exponential fit. \code{method}
#' chooses how the likelihood is evaluated: \code{"exact"} (Ozaki's O(n) recursion for the
#' exponential kernel; for Weibull and gamma the double sum stops where the kernel underflows to
#' 0), \code{"soe"} (Lomax, and gamma with shape < 1, as a sum of exponentials with relative error
#' \code{eps}), \code{"truncate"} (kernel tail mass below \code{eps} left out), \code{"em"} (the
#' EM algorithm) or \code{"inar"} (Kirchner's INAR estimator, constant baseline); \code{"auto"} is
#' \code{"exact"} for the exponential kernel, \code{"truncate"} for Weibull and \code{"soe"} for
#' Lomax and gamma.
#'
#' Goodness-of-fit is reported via time-rescaling residuals (Brown
#' \emph{et al.} 2002) and a Kolmogorov-Smirnov test against
#' Uniform(0, 1).
#'
#' @param df A data frame with an \code{OCC_DATE} or \code{REPORT_DATE}
#'   column.
#' @param kernel Excitation kernel: one of \code{"exponential"},
#'   \code{"gamma"}, \code{"weibull"}, \code{"lomax"}.
#' @param baseline Baseline kind: \code{"constant"} or
#'   \code{"sinusoidal"}.
#' @param ds_name Dataset name used in titles and warnings.
#' @param max_n Maximum number of events to retain; \code{NULL} (the default) keeps every event.
#'   A subsample thins the process (and its clustering), so it is for quick looks only; it is
#'   the same subsample as morie's Python (splitmix64 keys).
#' @param method Likelihood route: \code{"auto"}, \code{"exact"}, \code{"soe"},
#'   \code{"truncate"}, \code{"em"} or \code{"inar"} (see Details).
#' @param eps Error level of \code{"soe"} and \code{"truncate"}.
#'
#' @return A \code{morie_rich_result} with branching ratio,
#'   stationarity verdict, kernel and baseline parameters,
#'   negative log-likelihood, AIC, BIC, and time-rescaling KS
#'   statistic.
#'
#' @references
#' Kwan TKJ, Chen F, Dunsmuir WTM (2024). Likelihood inference for
#' non-stationary Hawkes processes. arXiv:2408.09710v1.
#'
#' @examples
#' \donttest{
#'   df <- morie_tps_load_tps_dataset("Assault", nrows = 400)
#'   rr <- morie_tps_hawkes_advanced_fit(df, kernel = "gamma",
#'                                         baseline = "sinusoidal",
#'                                         ds_name = "Assault")
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_hawkes_advanced_fit <- function(df,
                                            kernel = "gamma",
                                            baseline = "sinusoidal",
                                            ds_name = "?",
                                            max_n = NULL,
                                            method = "auto",
                                            eps = 1e-9) {
  if (!(kernel %in% .TPS_HAWKES_KERNELS)) {
    stop(sprintf("unknown kernel: %s", kernel))
  }
  if (!(baseline %in% .TPS_HAWKES_BASELINES)) {
    stop(sprintf("unknown baseline: %s", baseline))
  }
  if (!any(c("OCC_DATE", "REPORT_DATE") %in% colnames(df))) {
    return(.tps_hwka_result(
      title = sprintf("Hawkes-%s/%s -- %s", kernel, baseline, ds_name),
      warnings = "no OCC_DATE or REPORT_DATE column"))
  }
  td <- .tps_hwka_events_to_days(df, max_n)
  t  <- td$t
  T_ <- td$T_
  if (length(t) < 100L) {
    return(.tps_hwka_result(
      title = sprintf("Hawkes-%s/%s -- %s", kernel, baseline, ds_name),
      warnings = sprintf("only %d timestamps", length(t))))
  }

  fit <- tryCatch(
    .tps_hwka_fit_one(t, T_, kernel_kind = kernel,
                       baseline_kind = baseline, method = method, eps = eps),
    error = function(e) {
      list(error = conditionMessage(e))
    })
  if (!is.null(fit$error)) {
    return(.tps_hwka_result(
      title = sprintf("Hawkes-%s/%s -- %s", kernel, baseline, ds_name),
      warnings = fit$error))
  }

  eta <- fit$branching_ratio
  summary_lines <- list(
    events_fitted       = fit$n,
    time_window_days    = round(fit$T_days, 1),
    kernel              = kernel,
    baseline            = baseline,
    branching_ratio     = round(eta, 4),
    stationary          = if (eta < 1) "Yes (eta<1)" else "EXPLOSIVE",
    kernel_params       = round(fit$kernel_params, 4),
    baseline_params     = round(fit$baseline_params, 4),
    neg_log_likelihood  = round(fit$nll, 1),
    aic                 = round(fit$aic, 1),
    bic                 = round(fit$bic, 1),
    ks_stat             = round(fit$ks_stat, 4),
    ks_pvalue           = round(fit$ks_pvalue, 4)
  )
  interp <- sprintf(
    paste0("Branching ratio eta = %.3f -> mean %.2f offspring per ",
           "event. %s Time-rescaling KS p-value = %.4f -- residuals ",
           "%s Uniform(0,1) under correct specification (Brown et ",
           "al. 2002)."),
    eta, eta,
    if (eta < 1) "Process is stationary (eta < 1)."
    else "Process is EXPLOSIVE (eta >= 1).",
    fit$ks_pvalue,
    if (fit$ks_pvalue >= 0.05) "consistent with"
    else "depart from")
  .tps_hwka_result(
    title = sprintf("Hawkes [%s kernel, %s baseline] -- %s",
                     kernel, baseline, ds_name),
    summary_lines = summary_lines,
    interpretation = interp,
    payload = fit)
}


# ---------------------------------------------------------------------------
# Public: 8-way (kernel x baseline) AIC comparison
# ---------------------------------------------------------------------------

#' Compare Hawkes models across kernel x baseline combinations
#'
#' Fits every supplied (kernel, baseline) combination and ranks by
#' AIC. Mirrors Section 5 of Kwan-Chen-Dunsmuir (2024): the
#' Markovian classical Hawkes is the (exponential, constant) row;
#' the non-Markovian non-stationary models are everything else.
#'
#' Combinations that fail to converge are recorded with an error
#' message rather than aborting the whole comparison.
#'
#' @param df Data frame with \code{OCC_DATE} or \code{REPORT_DATE}.
#' @param ds_name Dataset name used in titles.
#' @param max_n Maximum events to fit; \code{NULL} (the default) keeps every event.
#' @param baselines Baseline kinds to sweep over.
#' @param kernels Kernel kinds to sweep over.
#' @param method Likelihood route, as in \code{\link{morie_tps_hawkes_advanced_fit}}.
#' @param eps Error level of \code{"soe"} and \code{"truncate"}.
#'
#' @return A \code{morie_rich_result} with a per-combination summary
#'   table, the best (lowest-AIC) combination, and the AIC gap
#'   between the classical Markovian model and the winner.
#'
#' @references Kwan TKJ, Chen F, Dunsmuir WTM (2024). arXiv:2408.09710.
#'
#' @examples
#' \donttest{
#'   df <- morie_tps_load_tps_dataset("Assault", nrows = 500)
#'   # A bounded grid (300 events, constant baseline, one kernel pair)
#'   # keeps the example fast; the defaults sweep the full
#'   # baseline x kernel grid over up to 4000 events.
#'   rr <- morie_tps_compare_hawkes_kernels(
#'     df, ds_name = "Assault", max_n = 300L,
#'     baselines = "constant", kernels = c("exponential", "gamma")
#'   )
#' }
#'
#' @export
morie_tps_compare_hawkes_kernels <- function(df,
                                               ds_name = "?",
                                               max_n = NULL,
                                               baselines = .TPS_HAWKES_BASELINES,
                                               kernels = .TPS_HAWKES_KERNELS,
                                               method = "auto",
                                               eps = 1e-9) {
  if (!any(c("OCC_DATE", "REPORT_DATE") %in% colnames(df))) {
    return(.tps_hwka_result(
      title = sprintf("Hawkes comparison -- %s", ds_name),
      warnings = "no OCC_DATE or REPORT_DATE column"))
  }
  td <- .tps_hwka_events_to_days(df, max_n)
  t  <- td$t
  T_ <- td$T_
  if (length(t) < 100L) {
    return(.tps_hwka_result(
      title = sprintf("Hawkes comparison -- %s", ds_name),
      warnings = sprintf("only %d timestamps", length(t))))
  }
  rows <- list()
  for (k in kernels) {
    for (b in baselines) {
      fit <- tryCatch(.tps_hwka_fit_one(t, T_, k, b, method = method, eps = eps),
                       error = function(e)
                         list(error = conditionMessage(e)))
      row <- if (!is.null(fit$error)) {
        list(kernel = k, baseline = b, error = fit$error)
      } else {
        list(kernel = k, baseline = b,
             k_params = fit$k_params,
             nll = round(fit$nll, 1),
             aic = round(fit$aic, 1),
             bic = round(fit$bic, 1),
             branching_ratio = round(fit$branching_ratio, 3),
             ks_pvalue = round(fit$ks_pvalue, 4),
             markovian = identical(k, "exponential"),
             stationary_baseline = identical(b, "constant"))
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  fitted <- Filter(function(r) is.null(r$error), rows)
  fitted <- fitted[order(vapply(fitted,
                                 function(r) r$aic,
                                 numeric(1)))]
  best <- if (length(fitted) > 0L) fitted[[1]] else NULL
  summary_lines <- list(
    combinations_fitted = length(fitted),
    combinations_failed = length(rows) - length(fitted)
  )
  if (!is.null(best)) {
    markov_row <- Filter(function(r)
                          identical(r$kernel, "exponential") &&
                          identical(r$baseline, "constant"),
                          fitted)
    delta <- if (length(markov_row) > 0L) {
      round(markov_row[[1]]$aic - best$aic, 1)
    } else NA_real_
    summary_lines$best_lowest_aic <-
      sprintf("%s / %s", best$kernel, best$baseline)
    summary_lines$delta_aic_vs_markovian <- delta
  }
  interp <- paste0(
    "Comparison of the (kernel x baseline) combinations. The ",
    "classical Markovian Hawkes (exponential, constant) is the ",
    "special case where the bivariate process (N_t, lambda_t) is ",
    "Markov. All non-exponential kernels and the time-varying ",
    "sinusoidal baseline yield non-Markovian intensity processes; ",
    "their large-sample theory is the contribution of Kwan, Chen & ",
    "Dunsmuir (2024).")
  .tps_hwka_result(
    title = sprintf("Markovian vs non-Markovian Hawkes -- %s",
                     ds_name),
    summary_lines = summary_lines,
    interpretation = interp,
    payload = list(rows = rows, best = best))
}


# ---------------------------------------------------------------------------
# Public: focused 2x2 comparison
# ---------------------------------------------------------------------------

#' Focused 2x2 Markovian vs non-Markovian Hawkes comparison
#'
#' Fits the four (kernel, baseline) combinations corresponding to the
#' two endpoints of the Kwan-Chen-Dunsmuir framework: classical
#' exponential / constant Markovian, against gamma / sinusoidal
#' non-Markovian. Faster to run than the full 8-way comparison and
#' suitable for dashboard surfaces.
#'
#' @param df Data frame with \code{OCC_DATE} or \code{REPORT_DATE}.
#' @param ds_name Dataset name used in titles.
#' @param max_n Maximum events to fit; \code{NULL} (the default) keeps every event.
#' @param method Likelihood route, as in \code{\link{morie_tps_hawkes_advanced_fit}}.
#' @param eps Error level of \code{"soe"} and \code{"truncate"}.
#'
#' @return A \code{morie_rich_result} from
#'   \code{morie_tps_compare_hawkes_kernels} restricted to the 2x2
#'   sub-grid.
#'
#' @references Kwan TKJ, Chen F, Dunsmuir WTM (2024). arXiv:2408.09710.
#'
#' @examples
#' \donttest{
#'   df <- morie_tps_load_tps_dataset("Assault", nrows = 500)
#'   rr <- morie_tps_hawkes_markovian_vs_nonmarkovian(df,
#'                                                     ds_name = "Assault")
#' }
#'
#' @export
morie_tps_hawkes_markovian_vs_nonmarkovian <- function(df,
                                                         ds_name = "?",
                                                         max_n = NULL,
                                                         method = "auto",
                                                         eps = 1e-9) {
  morie_tps_compare_hawkes_kernels(
    df, ds_name = ds_name, max_n = max_n,
    kernels = c("exponential", "gamma"),
    baselines = c("constant", "sinusoidal"), method = method, eps = eps)
}
