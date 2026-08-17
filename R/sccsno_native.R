# morie.fn -- function file (rootcoder007/morie)
# Self-controlled case series: cases only, each its own control.
#
# Events arise in an age-dependent Poisson process whose rate for
# individual i is
#   lambda_i(t | v_i, x_i) = lambda_i0(t) exp(gamma'x_i +
#                            sum_r beta_r X_ir(t)),
# with X_ir(t) = 1 when t falls in the r-th risk interval
# (v_i + a_r, v_i + b_r] after vaccination at age v_i, and lambda_i0
# piecewise constant on age bands. Writing the log baseline as
# phi_i + alpha_j splits it into an individual effect and an age
# effect. Conditioning on the number of events a person had -- and on
# their exposure history -- removes phi_i and gamma'x_i EXACTLY. The
# conditional likelihood is multinomial:
#   L = prod_i prod_k exp(alpha_j(t_ik) + beta_r(t_ik))
#                     / sum_j e_ij exp(alpha_j + beta_r(j)),
# where e_ij is the time individual i spent in interval j. Any fixed
# characteristic is inside phi_i and therefore cannot confound.
#
# What does NOT cancel: anything that varies within a person over
# time. If the age bands are too coarse, age leaks into beta-hat.
#
# Farrington's derivation requires that the event does not alter
# subsequent observation -- no event-dependent censoring and no
# event-dependent exposure. A pre-exposure window is a diagnostic: a
# pre-exposure relative incidence far from 1 is evidence the exposure
# was event-dependent, which invalidates the design.
#
# References
# ----------
# Farrington, C. P. (1995) "Relative Incidence Estimation from Case
# Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
# JSTOR stable URL https://www.jstor.org/stable/2533328. Secs. 2-3.
#
# Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
# (2006) "Tutorial in biostatistics: The self-controlled case series
# method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.

.sccsno_EPS <- 1e-12

#' Ordered distinct cutpoints for one individual (Fig. 1)
#'
#' A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param start Coerced to numeric by the body, with \code{as.numeric}.
#' @param end Coerced to numeric by the body, with \code{as.numeric}.
#' @param exposure Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param risk_periods A matrix; indexed by row and column.
#' @param age_breaks See Usage.
#' @return A vector, from \code{sort}.
#' @export
.sccsno_cuts <- function(start, end, exposure, risk_periods, age_breaks) {
  # Ordered distinct cutpoints for one individual (Fig. 1).
  pts <- c(as.numeric(start), as.numeric(end))
  for (b in age_breaks) {
    if (as.numeric(start) < as.numeric(b) &&
        as.numeric(b) < as.numeric(end)) {
      pts <- c(pts, as.numeric(b))
    }
  }
  if (!is.null(exposure)) {
    for (i in seq_len(nrow(risk_periods))) {
      for (p in c(as.numeric(exposure) + risk_periods[i, 1L],
                  as.numeric(exposure) + risk_periods[i, 2L])) {
        if (as.numeric(start) < p && p < as.numeric(end)) {
          pts <- c(pts, p)
        }
      }
    }
  }
  sort(unique(pts))
}

#' Index of the age band containing t (0-based, matching alpha[j+1])
#'
#' A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{>=}.
#' @param age_breaks See Usage.
#' @return The value of \code{j}, as built in the body.
#' @export
.sccsno_band <- function(t, age_breaks) {
  # Index of the age band containing t (0-based, matching alpha[j+1]).
  j <- 0L
  for (b in age_breaks) {
    if (t >= as.numeric(b)) {
      j <- j + 1L
    } else {
      break
    }
  }
  j
}

#' Index of the risk period containing t; 0 is the control period
#'
#' A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{<}.
#' @param exposure Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param risk_periods A matrix; indexed by row and column.
#' @return A numeric value.
#' @export
.sccsno_risk <- function(t, exposure, risk_periods) {
  # Index of the risk period containing t; 0 is the control period.
  if (is.null(exposure)) {
    return(0L)
  }
  for (r in seq_len(nrow(risk_periods))) {
    if (as.numeric(exposure) + risk_periods[r, 1L] < t &&
        t <= as.numeric(exposure) + risk_periods[r, 2L]) {
      return(r)
    }
  }
  0L
}

#' morie_sccsno_build_intervals
#'
#' A step of the sccsno_native implementation. Called by \code{.smatch_build_intervals}, \code{morie_sccsno_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param start Coerced to numeric by the body, with \code{as.numeric}.
#' @param end Coerced to numeric by the body, with \code{as.numeric}.
#' @param exposure Optional; may be \code{NULL}. Passed to \code{.sccsno_cuts}.
#' @param event_times See Usage.
#' @param risk_periods Passed to \code{.sccsno_rp}.
#' @param age_breaks Passed to \code{.sccsno_cuts}.
#' @return The value of \code{cells}, as built in the body.
#' @export
morie_sccsno_build_intervals <- function(start, end, exposure, event_times,
                                         risk_periods, age_breaks) {
  # One individual's follow-up, cut into (age band, risk period) cells
  # with their exposure times and event counts. Returns a matrix with
  # columns (age_band, risk_period, e_ij, n_ij). risk_periods is a
  # 2-column matrix (or list of pairs) of (a, b] offsets.
  rp <- .sccsno_rp(risk_periods)
  s <- as.numeric(start)
  e <- as.numeric(end)
  if (!(e > s)) {
    stop(sprintf(paste0("sccsno: the observation period must have ",
                        "positive length, got [%g, %g]"), s, e))
  }
  for (i in seq_len(nrow(rp))) {
    if (!(rp[i, 2L] > rp[i, 1L])) {
      stop(sprintf("sccsno: a risk period must satisfy b > a, got (%g, %g]",
                   rp[i, 1L], rp[i, 2L]))
    }
  }
  if (!is.null(exposure) &&
      !(s <= as.numeric(exposure) && as.numeric(exposure) <= e)) {
    stop(sprintf(paste0("sccsno: the exposure at %g lies outside the ",
                        "observation period [%g, %g]"),
                 as.numeric(exposure), s, e))
  }
  cuts <- .sccsno_cuts(s, e, exposure, rp, age_breaks)
  m <- length(cuts) - 1L
  cells <- matrix(0.0, nrow=m, ncol=4L)
  colnames(cells) <- c("age_band", "risk_period", "e", "n")
  for (q in seq_len(m)) {
    lo <- cuts[q]
    hi <- cuts[q + 1L]
    mid <- 0.5 * (lo + hi)
    cells[q, 1L] <- .sccsno_band(mid, age_breaks)
    cells[q, 2L] <- .sccsno_risk(mid, exposure, rp)
    cells[q, 3L] <- hi - lo
  }
  for (t in event_times) {
    tv <- as.numeric(t)
    if (!(s <= tv && tv <= e)) {
      stop(sprintf(paste0("sccsno: an event at %g lies outside the ",
                          "observation period [%g, %g]"), tv, s, e))
    }
    placed <- FALSE
    for (q in seq_len(m)) {
      if ((cuts[q] < tv && tv <= cuts[q + 1L]) ||
          (q == 1L && tv == cuts[1L])) {
        cells[q, 4L] <- cells[q, 4L] + 1
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      cells[m, 4L] <- cells[m, 4L] + 1
    }
  }
  cells
}

#' .sccsno_rp
#'
#' A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}, \code{morie_sccsno_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param risk_periods A matrix; the body checks with \code{is.matrix}.
#' @return The value of \code{rp}, as built in the body.
#' @export
.sccsno_rp <- function(risk_periods) {
  if (is.matrix(risk_periods)) {
    rp <- risk_periods
  } else {
    rp <- do.call(rbind, lapply(risk_periods,
                                function(p) as.numeric(unlist(p))))
  }
  storage.mode(rp) <- "double"
  rp
}

#' The conditional log-likelihood of Sec. 3. params is
#'
#' (beta_1..beta_s, alpha_1..alpha_{m-1}) with beta_0 = alpha_0 = 0. The
#' individual effects phi_i do not appear -- that is the point.
#'
#' @param params A vector; indexed elementwise.
#' @param cells_by_person See Usage.
#' @param n_risk A count; the body uses it as \code{seq_len(...)}.
#' @param n_age Numeric; combined arithmetically in the body.
#' @return The value of \code{ll}, as built in the body.
#' @export
morie_sccsno_loglik <- function(params, cells_by_person, n_risk, n_age) {
  # The conditional log-likelihood of Sec. 3. params is
  # (beta_1..beta_s, alpha_1..alpha_{m-1}) with beta_0 = alpha_0 = 0.
  # The individual effects phi_i do not appear -- that is the point.
  beta <- c(0.0, as.numeric(params[seq_len(n_risk)]))
  if (n_age > 1L) {
    alpha <- c(0.0, as.numeric(params[n_risk + seq_len(n_age - 1L)]))
  } else {
    alpha <- 0.0
  }
  ll <- 0.0
  for (cells in cells_by_person) {
    tot <- sum(cells[, 4L])
    if (tot == 0) {
      next
    }
    den <- 0.0
    for (q in seq_len(nrow(cells))) {
      j <- as.integer(cells[q, 1L])
      r <- as.integer(cells[q, 2L])
      den <- den + cells[q, 3L] * exp(alpha[j + 1L] + beta[r + 1L])
    }
    if (den <= .sccsno_EPS) {
      stop("sccsno: an individual has no observation time")
    }
    for (q in seq_len(nrow(cells))) {
      n <- cells[q, 4L]
      if (n > 0) {
        j <- as.integer(cells[q, 1L])
        r <- as.integer(cells[q, 2L])
        ll <- ll + n * (alpha[j + 1L] + beta[r + 1L])
      }
    }
    ll <- ll - tot * log(den)
  }
  ll
}

#' .sccsno_grad_hess
#'
#' A step of the sccsno_native implementation. Called by \code{morie_sccsno_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param params A vector; indexed elementwise.
#' @param cells_by_person See Usage.
#' @param n_risk A count; the body uses it as \code{seq_len(...)}.
#' @param n_age Numeric; combined arithmetically in the body.
#' @return A list with \code{g}, \code{H}.
#' @export
.sccsno_grad_hess <- function(params, cells_by_person, n_risk, n_age) {
  p <- n_risk + n_age - 1L
  g <- rep(0.0, p)
  H <- matrix(0.0, p, p)
  beta <- c(0.0, as.numeric(params[seq_len(n_risk)]))
  if (n_age > 1L) {
    alpha <- c(0.0, as.numeric(params[n_risk + seq_len(n_age - 1L)]))
  } else {
    alpha <- 0.0
  }
  idx <- function(j, r) {
    # Design row: risk dummies then age dummies, both baseline 0.
    row <- rep(0.0, p)
    if (r > 0L) {
      row[r] <- 1.0
    }
    if (j > 0L) {
      row[n_risk + j] <- 1.0
    }
    row
  }
  for (cells in cells_by_person) {
    tot <- sum(cells[, 4L])
    if (tot == 0) {
      next
    }
    m <- nrow(cells)
    w <- numeric(m)
    rows <- matrix(0.0, m, p)
    den <- 0.0
    for (q in seq_len(m)) {
      j <- as.integer(cells[q, 1L])
      r <- as.integer(cells[q, 2L])
      v <- cells[q, 3L] * exp(alpha[j + 1L] + beta[r + 1L])
      den <- den + v
      w[q] <- v
      rows[q, ] <- idx(j, r)
    }
    pr <- w / den
    for (q in seq_len(m)) {
      n <- cells[q, 4L]
      if (n > 0) {
        g <- g + n * rows[q, ]
      }
    }
    mean_ <- as.numeric(crossprod(rows, pr))
    g <- g - tot * mean_
    sec <- crossprod(rows, rows * pr)
    H <- H - tot * (sec - outer(mean_, mean_))
  }
  list(g=g, H=H)
}

#' morie_sccsno_fit
#'
#' A step of the sccsno_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cases See Usage.
#' @param risk_periods Passed to \code{.sccsno_rp}.
#' @param age_breaks Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{c()}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @param ridge A matrix; passed to \code{diag}. Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{relative_incidence}, \code{log_ri}, \code{se_log_ri}, \code{age_effects}, \code{se_age}, \code{coef}, \code{se}, \code{loglik}, \code{n_cases}, \code{converged}, \code{iterations}, \code{n_risk_periods}, \code{n_age_bands}, \code{method}, \code{conditions_out}.
#' @export
morie_sccsno_fit <- function(cases, risk_periods, age_breaks=c(),
                             iters=100, tol=1e-10, ridge=1e-10) {
  # Maximise the conditional likelihood by Newton-Raphson. cases is a
  # list of lists with names start, end, exposure (or NULL) and
  # events. Only individuals with at least one event contribute.
  rp <- .sccsno_rp(risk_periods)
  ab <- as.numeric(age_breaks)
  if (is.unsorted(ab, strictly=FALSE)) {
    stop("sccsno: age_breaks must be increasing")
  }
  n_risk <- nrow(rp)
  n_age <- length(ab) + 1L
  if (n_risk < 1L) {
    stop("sccsno: at least one risk period is needed")
  }
  cells_by_person <- list()
  used <- 0L
  for (cse in cases) {
    ev <- cse[["events"]]
    if (is.null(ev) || length(ev) == 0L) {
      next
    }
    cells <- morie_sccsno_build_intervals(cse[["start"]], cse[["end"]],
                                          cse[["exposure"]], ev, rp, ab)
    cells_by_person[[used + 1L]] <- cells
    used <- used + 1L
  }
  if (used == 0L) {
    stop("sccsno: no case contributed an event")
  }
  p <- n_risk + n_age - 1L
  par <- rep(0.0, p)
  conv <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(iters))) {
    gh <- .sccsno_grad_hess(par, cells_by_person, n_risk, n_age)
    A <- -gh$H + diag(ridge, p)
    step <- tryCatch(
      backsolve(chol(A), forwardsolve(t(chol(A)), gh$g)),
      error=function(e) NULL)
    if (is.null(step)) {
      stop(paste0("sccsno: the information matrix is singular -- some ",
                  "interval carries no events or no exposure time"))
    }
    # step-halving safeguard: the undamped update oscillates and
    # overflows on strong-effect fixtures (H -> 0, par -> Inf)
    ll0 <- morie_sccsno_loglik(par, cells_by_person, n_risk, n_age)
    lam <- 1.0
    for (h in seq_len(30L)) {
      cand <- par + lam * step
      llc <- morie_sccsno_loglik(cand, cells_by_person, n_risk, n_age)
      if (is.finite(llc) && llc >= ll0 - 1e-12) break
      lam <- lam / 2
    }
    par <- par + lam * step
    if (max(abs(lam * step)) < tol) {
      conv <- TRUE
      break
    }
  }
  gh <- .sccsno_grad_hess(par, cells_by_person, n_risk, n_age)
  A <- -gh$H + diag(ridge, p)
  cov_ <- tryCatch(chol2inv(chol(A)), error=function(e) solve(A))
  se <- ifelse(diag(cov_) > 0, sqrt(diag(cov_)), NaN)
  beta <- par[seq_len(n_risk)]
  age_idx <- if (p > n_risk) seq.int(n_risk + 1L, p) else integer(0)
  list(
    estimate=exp(beta),
    relative_incidence=exp(beta),
    log_ri=beta, se_log_ri=se[seq_len(n_risk)],
    age_effects=par[age_idx], se_age=se[age_idx],
    coef=par, se=se,
    loglik=morie_sccsno_loglik(par, cells_by_person, n_risk, n_age),
    n_cases=used, converged=conv, iterations=it,
    n_risk_periods=n_risk, n_age_bands=n_age,
    method=paste0("self-controlled case series, conditional ",
                  "likelihood of Farrington (1995) Sec. 3"),
    conditions_out=paste0("individual frailty and every ",
                          "time-invariant covariate")
  )
}

#' Point estimates and Wald intervals on the incidence scale
#'
#' A step of the sccsno_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$log_ri}, \code{$se_log_ri} from it.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @return A list with \code{intervals}, \code{level}.
#' @export
morie_sccsno_relative_incidence <- function(fit, level=0.95) {
  # Point estimates and Wald intervals on the incidence scale.
  z <- stats::qnorm(0.5 + as.numeric(level) / 2.0)
  out <- list()
  for (i in seq_along(fit[["log_ri"]])) {
    b <- fit[["log_ri"]][i]
    s <- fit[["se_log_ri"]][i]
    out[[i]] <- list(ri=exp(b), lower=exp(b - z * s),
                     upper=exp(b + z * s), log_ri=b, se=s)
  }
  list(intervals=out, level=as.numeric(level))
}

#' morie_sccsno_check_assumptions
#'
#' A step of the sccsno_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit_with_pre A list; the body reads \code{$relative_incidence} from it.
#' @param pre_index Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.25}.
#' @return A list with \code{pre_exposure_ri}, \code{consistent_with_design}, \code{tolerance_log}, \code{interpretation}.
#' @export
morie_sccsno_check_assumptions <- function(fit_with_pre, pre_index=0,
                                           tol=0.25) {
  # Read the pre-exposure window as a design diagnostic. A relative
  # incidence far from 1 in a window BEFORE exposure means the event
  # influenced whether or when exposure happened; that breaks the
  # derivation itself.
  ri <- fit_with_pre[["relative_incidence"]][as.integer(pre_index) + 1L]
  ok <- abs(log(ri)) <= as.numeric(tol)
  list(pre_exposure_ri=ri, consistent_with_design=ok,
       tolerance_log=as.numeric(tol),
       interpretation=paste0(
         "a pre-exposure RI near 1 is consistent with ",
         "event-independent exposure; far from 1 indicates the ",
         "event affected exposure, which invalidates the ",
         "design rather than biasing it"))
}

#' morie_sccsno_cheatsheet
#'
#' A step of the sccsno_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_sccsno_cheatsheet <- function() {
  paste0(
    "sccsno: SCCS. Cases ONLY. Conditioning on each person's ",
    "event count cancels phi_i exactly, so every ",
    "time-INVARIANT confounder -- measured or not -- is gone ",
    "by construction. What does NOT cancel is anything varying ",
    "WITHIN a person: age must be modelled with bands or it ",
    "leaks into beta. Requires no event-dependent censoring ",
    "and no event-dependent exposure; a pre-exposure window ",
    "with RI far from 1 says the latter failed."
  )
}

# compact alias per ledger/NAMING.md
morie_sccsno_sccsnoevent <- morie_sccsno_fit
morie_sccsno_sccs_no_replacement <- morie_sccsno_fit

#' @export
morie_sccsno <- morie_sccsno_fit
