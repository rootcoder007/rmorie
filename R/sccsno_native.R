```r
# morie.fn -- function file (rootcoder007/morie)
# Self-controlled case series: cases only, each its own control.
#
# A cohort study of a rare vaccine reaction needs the whole cohort. A
# case-control study needs matched controls. The self-controlled case
# series needs neither -- only the people who had the event, and for
# each of them the dates of exposure and of the event. Everything else
# cancels.
#
# Why it cancels. Events arise in an age-dependent Poisson process whose
# rate for individual i is
#
#   lambda_i(t | v_i, x_i) = lambda_{i0}(t) * exp(gamma^T x_i + sum_r beta_r X_{ir}(t))
#
# with X_{ir}(t) = 1 when t falls in the r-th risk interval
# (v_i + a_r, v_i + b_r] after vaccination at age v_i, and lambda_{i0}
# piecewise constant on age bands. Writing the log baseline as
# phi_i + alpha_j splits it into an individual effect and an age effect.
# Conditioning on the number of events a person had -- and on their
# exposure history -- removes phi_i and gamma^T x_i exactly, because
# both are constants that multiply every interval of that person's
# follow-up alike. The conditional likelihood is multinomial:
#
#   L = prod_i prod_{k=1}^{n_i}
#       exp(alpha_{j(t_ik)} + beta_{r(t_ik)})
#       / (sum_j e_ij * exp(alpha_j + beta_{r(j)}))
#
# where e_ij is the time individual i spent in interval j.
#
# What does NOT cancel. Age does, and only because it is modelled.
# Anything that varies within a person over time is a live confounder:
# if the age bands are too coarse, age leaks into beta-hat.
#
# The assumption in this module's name. Farrington's derivation
# requires that the event does not alter subsequent observation -- no
# event-dependent censoring (the event must not be fatal or
# observation-terminating) and no event-dependent exposure (having the
# event must not change whether or when you get vaccinated). This
# module implements the case where those hold.
#
# A pre-exposure window is a diagnostic, not decoration. If vaccination
# is deferred because a child is unwell, event rates dip just before
# exposure. Fitting an explicit pre-exposure interval makes that
# visible.
#
# References
# ----------
# Farrington, C. P. (1995) "Relative Incidence Estimation from Case
# Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
# JSTOR stable URL https://www.jstor.org/stable/2533328.
#
# Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P. (2006)
# "Tutorial in biostatistics: The self-controlled case series method",
# Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.

.sccsno_EPS <- 1e-12

.sccsno_cholsolve <- function(A, b) {
  p <- nrow(A)
  L <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:i) {
      idx <- seq_len(j - 1)
      s <- if (length(idx) > 0) sum(L[i, idx] * L[j, idx]) else 0
      if (i == j) {
        v <- A[i, i] - s
        if (v <= 0) stop("singular matrix")
        L[i, j] <- sqrt(v)
      } else {
        L[i, j] <- (A[i, j] - s) / L[j, j]
      }
    }
  }
  # Forward: L y = b
  y <- numeric(p)
  for (i in 1:p) {
    idx <- seq_len(i - 1)
    s <- if (length(idx) > 0) sum(L[i, idx] * y[idx]) else 0
    y[i] <- (b[i] - s) / L[i, i]
  }
  # Backward: L^T x = y
  x <- numeric(p)
  for (i in p:1) {
    idx <- if (i < p) (i + 1):p else integer(0)
    s <- if (length(idx) > 0) sum(L[idx, i] * x[idx]) else 0
    x[i] <- (y[i] - s) / L[i, i]
  }
  x
}

.sccsno_cuts <- function(start, end, exposure, risk_periods, age_breaks) {
  s <- as.numeric(start); e <- as.numeric(end)
  pts <- unique(c(s, e))
  for (b in age_breaks) {
    bb <- as.numeric(b)
    if (s < bb && bb < e) pts <- c(pts, bb)
  }
  if (!is.null(exposure)) {
    exv <- as.numeric(exposure)
    for (rb in risk_periods) {
      a <- as.numeric(rb[[1]]); b <- as.numeric(rb[[2]])
      for (pp in c(exv + a, exv + b)) {
        if (s < pp && pp < e) pts <- c(pts, pp)
      }
    }
  }
  sort(pts)
}

.sccsno_band <- function(t, age_breaks) {
  j <- 0
  for (b in age_breaks) {
    if (t >= as.numeric(b)) j <- j + 1 else break
  }
  j
}

.sccsno_risk <- function(t, exposure, risk_periods) {
  if (is.null(exposure)) return(0)
  exv <- as.numeric(exposure)
  for (r in seq_along(risk_periods)) {
    rb <- risk_periods[[r]]
    a <- as.numeric(rb[[1]]); b <- as.numeric(rb[[2]])
    if (exv + a < t && t <= exv + b) return(r)
  }
  0
}

build_intervals <- function(start, end, exposure, event_times, risk_periods,
                            age_breaks) {
  s <- as.numeric(start); e <- as.numeric(end)
  if (!(e > s)) stop(sprintf("sccsno: the observation period must have positive length, got [%g, %
