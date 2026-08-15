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
  y <- numeric(p)
  for (i in 1:p) {
    idx <- seq_len(i - 1)
    s <- if (length(idx) > 0) sum(L[i, idx] * y[idx]) else 0
    y[i] <- (b[i] - s) / L[i, i]
  }
  x <- numeric(p)
  for (i in p:1) {
    idx <- if (i < p) (i + 1):p else integer(0)
    s <- if (length(idx) > 0) sum(L[idx, i] * x[idx]) else 0
    x[i] <- (y[i] - s) / L[i, i]
  }
  x
}

.sccsno_qnorm <- function(p) {
  a1 <- -3.969683028665376e+01
  a2 <-  2.209460984245205e+02
  a3 <- -2.759285104469687e+02
  a4 <-  1.383577518672690e+02
  a5 <- -3.066479806614716e+01
  a6 <-  2.506628277459239e+00
  b1 <- -5.447609879822406e+01
  b2 <-  1.615858368580409e+02
  b3 <- -1.556989798598866e+02
  b4 <-  6.680131188771972e+01
  b5 <- -1.328068155288572e+01
  c1 <- -7.784894002430293e-03
  c2 <- -3.223964580411365e-01
  c3 <- -2.400758277161838e+00
  c4 <- -2.549732539343734e+00
  c5 <-  4.374664141464968e+00
  c6 <-  2.938163982698783e+00
  d1 <-  7.784695709041462e-03
  d2 <-  3.224671290700398e-01
  d3 <-  2.445134137142996e+00
  d4 <-  3.754408661907416e+00
  plow <- 0.02425
  phigh <- 1 - plow
  if (p < plow) {
    q <- sqrt(-2 * log(p))
    -(((((c1*q + c2)*q + c3)*q + c4)*q + c5)*q + c6) /
     (((((d1*q + d2)*q + d3)*q + d4)*q + 1)
  } else if (p <= phigh) {
    q <- p - 0.5
    r <- q * q
    (((((a1*r + a2)*r + a3)*r + a4)*r + a5)*r + a6) * q /
    (((((b1*r + b2)*r + b3)*r + b4)*r + b5)*r + 1)
  } else {
    q <- sqrt(-2 * log(1 - p))
    -(((((c1*q + c2)*q + c3)*q + c4)*q + c5)*q + c6) /
     (((((d1*q + d2)*q + d3)*q + d4)*q + 1)
  }
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
  if (!(e > s)) {
    stop(sprintf("sccsno: the observation period must have positive length, got [%g, %g]", s, e))
  }
  for (rb in risk_periods) {
    a <- as.numeric(rb[[1]]); b <- as.numeric(rb[[2]])
    if (!(b > a)) {
      stop(sprintf("sccsno: a risk period must satisfy b > a, got (%g, %g]", a, b))
    }
  }
  if (!is.null(exposure)) {
    exv <- as.numeric(exposure)
    if (!(s <= exv && exv <= e)) {
      stop(sprintf("sccsno: the exposure at %g lies outside the observation period [%g, %g]", exv, s, e))
    }
  }
  cuts <- .sccsno_cuts(s, e, exposure, risk_periods, age_breaks)
  nc <- length(cuts)
  if (nc < 2) {
    stop(sprintf("sccsno: the observation period must have positive length, got [%g, %g]", s, e))
  }
  cells <- vector("list", nc - 1)
  for (q in 1:(nc - 1)) {
    lo <- cuts[q]; hi <- cuts[q + 1]
    mid <- 0.5 * (lo + hi)
    cells[[q]] <- c(.sccsno_band(mid, age_breaks),
                    .sccsno_risk(mid, exposure, risk_periods),
                    hi - lo, 0)
  }
  for (t in event_times) {
    tv <- as.numeric(t)
    if (!(s <= tv && tv <= e)) {
      stop(sprintf("sccsno: an event at %g lies outside the observation period [%g, %g]", tv, s, e))
    }
    placed <- FALSE
    for (q in 1:(nc - 1)) {
      if ((cuts[q] < tv && tv <= cuts[q + 1]) || (q == 1 && tv == cuts[1])) {
        cells[[q]][4] <- cells[[q]][4] + 1
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      cells[[nc - 1]][4] <- cells[[nc - 1]][4] + 1
    }
  }
  cells
}

sccs_loglik <- function(params, cells_by_person, n_risk, n_age) {
  beta <- c(0, as.numeric(params[1:n_risk]))
  alpha <- c(0, as.numeric(params[(n_risk + 1):(n_risk + n_age - 1)]))
  ll <- 0
  for (cells in cells_by_person) {
    nc <- length(cells)
    tot <- 0
    for (ci in 1:nc) tot <- tot + cells[[ci]][4]
    if (tot == 0) next
    den <- 0
    for (ci in 1:nc) {
      c <- cells[[ci]]
      j <- c[1]; r <- c[2]; e <- c[3]
      den <- den + e * exp(alpha[j + 1] + beta[r + 1])
    }
    if (den <= .sccsno_EPS) {
      stop("sccsno: an individual has no observation time")
    }
    for (ci in 1:nc) {
      c <- cells[[ci]]
      j <- c[1]; r <- c[2]; n <- c[4]
      if (n > 0) ll <- ll + n * (alpha[j + 1] + beta[r + 1])
    }
    ll <- ll - tot * log(den)
  }
  ll
}

.sccsno_grad_hess <- function(params, cells_by_person, n_risk, n_age) {
  p <- n_risk + n_age - 1
  g <- numeric(p)
  H <- matrix(0, p, p)
  beta <- c(0, as.numeric(params[1:n_risk]))
  alpha <- c(0, as.numeric(params[(n_risk + 1):p]))

  idx <- function(j, r) {
    row <- numeric(p)
    if (r > 0) row[r] <- 1
    if (j > 0) row[n_risk + j] <- 1
    row
  }

  for (cells in cells_by_person) {
    nc <- length(cells)
    tot <- 0
    for (ci in 1:nc) tot <- tot + cells[[ci]][4]
    if (tot == 0) next

    w <- numeric(nc)
    rows <- matrix(0, nc, p)
    den <- 0
    for (ci in 1:nc) {
      c <- cells[[ci]]
      j <- c[1]; r <- c[2]; e <- c[3]
      v <- e * exp(alpha[j + 1] + beta[r + 1])
      den <- den + v
      w[ci] <- v
      rows[ci, ] <- idx(j, r)
    }
    pr <- w / den

    for (ci in 1:nc) {
      c <- cells[[ci]]
      n <- c[4]
      if (n > 0) {
        g <- g + n * rows[ci, ]
      }
    }

    mean_design <- as.numeric(crossprod(rows, pr))
    g <- g - tot * mean_design

    sec <- crossprod(rows, pr * rows)
    H <- H - tot * (sec - mean_design %o% mean_design)
  }
  list(g, H)
}

sccs_fit <- function(cases, risk_periods, age_breaks = numeric(0),
                     iters = 100, tol = 1e-10, ridge = 1e-10) {
  rp <- lapply(risk_periods, function(rb) c(as.numeric(rb[[1]]), as.numeric(rb[[2]])))
  ab <- as.numeric(age_breaks)
  if (length(ab) > 0 && !identical(ab, sort(ab))) {
    stop("sccsno: age_breaks must be increasing")
  }
  n_risk <- length(rp)
  n_age <- length(ab) + 1
  if (n_risk < 1) {
    stop("sccsno: at least one risk period is needed")
  }

  cells_by_person <- list()
  used <- 0
  for (c in cases) {
    ev <- c$events
    if (is.null(ev)) ev <- numeric(0)
    if (length(ev) == 0) next
    cells <- build_intervals(c$start, c$end, c$exposure, ev, rp, ab)
    cells_by_person <- c(cells_by_person, list(cells))
    used <- used + 1
  }
  if (used == 0) {
    stop("sccsno: no case contributed an event")
  }

  p <- n_risk + n_age - 1
  par <- numeric(p)
  conv <- FALSE
  it <- 0
  for (it in 1:iters) {
    gh <- .sccsno_grad_hess(par, cells_by_person, n_risk, n_age)
    g <- gh[[1]]; H <- gh[[2]]
    A <- -H
    diag(A) <- diag(A) + ridge
    step <- tryCatch(
      .sccsno_cholsolve(A, g),
      error = function(e) {
        stop("sccsno: the information matrix is singular -- some interval carries no events or no exposure time")
      }
    )
    par <- par + step
    mx <- max(abs(step))
    if (mx < tol) {
      conv <- TRUE
      break
    }
  }

  gh <- .sccsno_grad_hess(par, cells_by_person, n_risk, n_age)
  g <- gh[[1]]; H <- gh[[2]]
  A <- -H
  diag(A) <- diag(A) + ridge
  cols <- list()
  for (a in 1:p) {
    e_vec <- numeric(p)
    e_vec[a] <- 1
    cols[[a]] <- tryCatch(
      .sccsno_cholsolve(A, e_vec),
      error = function(e) {
        stop("sccsno: the information matrix is singular -- some interval carries no events or no exposure time")
      }
    )
  }
  se <- numeric(p)
  for (a in 1:p) {
    v <- cols[[a]][a]
    se[a] <- if (v > 0) sqrt(v) else NaN
  }
  beta <- par[1:n_risk]
  list(
    estimate = exp(beta),
    relative_incidence = exp(beta),
    log_ri = beta,
    se_log_ri = se[1:n_risk],
    age_effects = par[(n_risk + 1):p],
    se_age = se[(n_risk + 1):p],
    coef = par,
    se = se,
    loglik = sccs_loglik(par, cells_by_person, n_risk, n_age),
    n_cases = used,
    converged = conv,
    iterations = it,
    n_risk_periods = n_risk,
    n_age_bands = n_age,
    method = "self-controlled case series, conditional likelihood of Farrington (1995) Sec. 3",
    conditions_out = "individual frailty and every time-invariant covariate"
  )
}

relative_incidence <- function(fit, level = 0.95) {
  z <- .sccsno_qnorm(0.5 + level / 2.0)
  out <- list()
  for (i in seq_along(fit$log_ri)) {
    b <- fit$log_ri[i]
    s <- fit$se_log_ri[i]
    out[[i]] <- list(
      ri = exp(b),
      lower = exp(b - z * s),
      upper = exp(b + z * s),
      log_ri = b,
      se = s
    )
  }
  list(intervals = out, level = level)
}

check_assumptions <- function(fit_with_pre, pre_index = 0, tol = 0.25) {
  ri <- fit_with_pre$relative_incidence[pre_index + 1]
  ok <- abs(log(ri)) <= tol
  list(
    pre_exposure_ri = ri,
    consistent_with_design = ok,
    tolerance_log = tol,
    interpretation = "a pre-exposure RI near 1 is consistent with event-independent exposure; far from 1 indicates the event affected exposure, which invalidates the design rather than biasing it"
  )
}

cheatsheet <- function() {
  "sccsno: SCCS. Cases ONLY. Conditioning on each person's event count cancels phi_i exactly, so every time-INVARIANT confounder -- measured or not -- is gone by construction. What does NOT cancel is anything varying WITHIN a person: age must be modelled with bands or it leaks into beta. Requires no event-dependent censoring and no event-dependent exposure; a pre-exposure window with RI far from 1 says the latter failed."
}

sccsnoevent <- sccs_fit
sccs_no_replacement <- sccs_fit
morie_sccsno <- sccs_fit
