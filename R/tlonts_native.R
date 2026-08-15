# morie.fn -- function file (rootcoder007/morie)
# Online targeted learning for a single time series.
# 
# At each time we observe, in order, a covariate vector, a treatment and
# an outcome. The conditional law of that triple given the past depends
# on the past only through a fixed-dimensional summary measure, and
# the mechanism producing it is constant in time. That is the whole
# statistical model, and its generality is the point: an empty summary
# measure gives ordinary i.i.d. targeted learning; a parametric
# conditional density gives a classical time series model; a
# data-dependent randomisation gives a group sequential adaptive design.
# 
# Effects are defined by stochastic interventions on future treatment
# nodes. With a single time series there is no population to average
# over, so a static "set A_t = 1 for everyone" has no referent.
# The causal quantity is instead the mean of a future outcome under a
# stochastic intervention on a subset of the treatment nodes, and the
# chapter establishes that these are identifiable from the observed data
# distribution.
# 
# Where the sample size comes from. Not from independent units --
# there is one series. It comes from time: the fixed-dimensional
# summary and the time-invariant mechanism mean each new time point is
# another draw from the same conditional law. So asymptotics are in
# t, and the influence curve is a martingale difference sequence rather
# than an i.i.d. sum. Its variance is estimated by the sum of
# conditional variances, and the martingale central limit theorem
# supplies the normal limit.
# 
# Which makes one check essential. If the summary measure is too
# small, the "past" left out is still influencing the present, the
# martingale property fails, and the reported interval is wrong for
# reasons no amount of data fixes. martingale_check regresses the
# influence terms on the past and reports the dependence that should
# not be there.
# 
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 19 (van der
# Laan, Chambaz & Lendle): a time series in which one observes in
# chronological order a covariate vector, a treatment and an outcome;
# the conditional distribution given the past depending on the past
# through a fixed-dimensional summary measure, described by a
# time-invariant mechanism in a model space that may be unspecified; a
# compatible causal model with a family of causal effects defined by
# STOCHASTIC INTERVENTIONS on a subset of the treatment nodes on a
# future outcome, and their identifiability from the observed data
# distribution; and the observation that empty summary measures recover
# i.i.d. targeted learning, parametric conditional densities recover
# classical time series models, and group sequential adaptive designs
# are included.
# 
# Chambaz, A., Zheng, W. & van der Laan, M. J. (2017) "Targeted
# sequential design for targeted learning inference of the optimal
# treatment rule and its mean reward", Annals of Statistics 45(6),
# 2537-2564, doi:10.1214/16-AOS1534.
# 
# van der Laan, M. J., Rose, S. & Lendle, S. (2018) "Online Targeted
# Learning for Time Series", in Targeted Learning in Data Science,
# Springer, doi:10.1007/978-3-319-65304-4_19.

.TLONTS_EPS <- 1e-12

lag_summary <- function(series, t, lags = 2) {
  v <- as.numeric(series)
  L <- as.integer(lags)
  if (L < 0) stop("tlonts: lags must be non-negative")
  if (t <= 0) return(rep(0.0, L))
  # Python: past = v[max(0, t - L):t]  (0-based, end exclusive)
  # We need the last min(L, t) elements up to time t (0-based).
  n_past <- as.integer(min(L, t))
  if (n_past == 0L) return(rep(0.0, L))
  start_idx <- t - n_past + 1
  if (start_idx < 1) start_idx <- 1L
  end_idx <- t
  if (end_idx > length(v)) end_idx <- length(v)
  if (start_idx > end_idx) {
    past <- numeric(0)
  } else {
    past <- v[seq.int(start_idx, end_idx)]
  }
  pad_len <- L - length(past)
  c(rep(0.0, pad_len), past)
}

stochastic_intervention <- function(A, nodes, shift = NULL, prob = NULL) {
  a <- as.numeric(A)
  idx <- as.integer(nodes)
  if (any(idx < 0L | idx >= length(a))) {
    stop("tlonts: an intervention node is outside the series")
  }
  if (is.null(shift) == is.null(prob)) {
    stop("tlonts: give exactly one of shift or prob")
  }
  out <- a
  if (!is.null(shift)) {
    for (i in idx) {
      out[i + 1L] <- a[i + 1L] + as.numeric(shift)
    }
    kind <- "shift"
  } else {
    for (i in idx) {
      out[i + 1L] <- as.numeric(prob)
    }
    kind <- "bernoulli"
  }
  list(
    intervened = out,
    nodes = sort(idx),
    n_intervened = length(idx),
    kind = kind
  )
}

martingale_variance <- function(D) {
  v <- as.numeric(D)
  T_len <- length(v)
  if (T_len < 2L) stop("tlonts: at least 2 time points are needed")
  s2 <- sum(v * v) / T_len
  se <- sqrt(s2 / T_len)
  list(
    variance = s2,
    se = se,
    T = T_len,
    note = "asymptotics are in TIME, not in independent units"
  )
}

martingale_check <- function(D, past, tol = 0.2) {
  d <- as.numeric(D)
  p <- as.numeric(past)
  if (length(d) != length(p)) {
    stop(sprintf("tlonts: %d influence terms but %d past values",
                 length(d), length(p)))
  }
  n <- length(d)
  md <- sum(d) / n
  mp <- sum(p) / n
  num <- sum((d - md) * (p - mp))
  den <- sqrt(sum((d - md)^2) * sum((p - mp)^2))
  r_val <- if (den > .TLONTS_EPS) num / den else 0.0
  list(
    correlation = r_val,
    is_martingale = abs(r_val) < as.numeric(tol),
    note = "a non-zero correlation says the summary measure omits something the present still depends on"
  )
}

online_tmle_series <- function(Y, A, Z, Q_fn, g_fn, target_prob, burn_in = 10) {
  y <- as.numeric(Y)
  a <- as.numeric(A)
  if (!is.matrix(Z)) Z <- as.matrix(Z)
  T_len <- length(y)
  if (!(length(a) == T_len && nrow(Z) == T_len)) {
    stop("tlonts: the series differ in length")
  }
  b <- as.integer(burn_in)
  if (b < 1L || b >= T_len) {
    stop(sprintf("tlonts: burn_in must lie in 1..%d", T_len - 1L))
  }
  est <- numeric(0)
  D <- numeric(0)
  running <- 0.0
  # t is 0-based to match Python's range(b, T)
  for (t in seq.int(b, T_len - 1L)) {
    g <- as.numeric(g_fn(Z[t + 1L, ]))
    if (g <= 0.0 || g >= 1.0) {
      stop(sprintf("tlonts: the treatment probability left (0,1) at time %d", t))
    }
    gs <- as.numeric(target_prob)
    if (a[t + 1L] == 1.0) {
      h <- gs / g
    } else {
      h <- (1.0 - gs) / (1.0 - g)
    }
    q1 <- as.numeric(Q_fn(1.0, Z[t + 1L, ]))
    q0 <- as.numeric(Q_fn(0.0, Z[t + 1L, ]))
    qa <- if (a[t + 1L] == 1.0) q1 else q0
    psi_t <- gs * q1 + (1.0 - gs) * q0
    running <- running + psi_t
    cur <- running / (t - b + 1L)
    est <- c(est, cur)
    D <- c(D, h * (y[t + 1L] - qa) + psi_t - cur)
  }
  mv <- martingale_variance(D)
  last_est <- est[length(est)]
  list(
    estimate = last_est,
    psi = last_est,
    path = est,
    se = mv$se,
    ci = c(last_est - 1.96 * mv$se, last_est + 1.96 * mv$se),
    T_scored = mv$T,
    method = "online TMLE for a time series under a stochastic intervention; van der Laan & Rose (2018) Chap. 19",
    note = "one series, no independent units -- the sample size is TIME, and the influence terms form a martingale difference sequence"
  )
}

cheatsheet <- function() {
  "tlonts: ONE time series -- covariate, treatment, outcome at each step -- with the conditional law depending on the past only through a FIXED-DIMENSIONAL summary and a time-invariant mechanism. Effects are defined by STOCHASTIC interventions on a SUBSET of future treatment nodes, since with a single series there is no population to set treatment for. Sample size comes from TIME: the influence terms are a MARTINGALE difference sequence, so variance is the sum of squares and the CLT is the martingale one. If the summary is too small the martingale property fails and the interval is simply wrong."
}

# compact alias per ledger/NAMING.md
onlinetimeseriestmle <- online_tmle_series

# Module entry point
morie_tlonts <- online_tmle_series
