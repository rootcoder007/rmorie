# morie.fn -- function file (rootcoder007/morie)
#
# TMLE for cumulative incidence under competing risks.
#
# With several event types, a subject who experiences one is no longer
# at risk of experiencing another first. The quantity of interest is
# the cumulative incidence for type j under an intervention,
#
#   F_j^a(t) = integral_0^t S^a(u-) lambda_j^a(u) du,
#
# the cause-specific hazard integrated against overall survival. Two
# things follow immediately and are easy to get wrong.
#
# A cause-specific hazard is not a cumulative incidence. Raising the
# hazard of a competing type lowers type j's cumulative incidence
# without touching lambda_j at all, because fewer subjects survive to
# be at risk. So an intervention that reduces mortality can increase
# the incidence of everything else, and a hazard-ratio summary will
# not show it. The anchor holds lambda_1 fixed, raises lambda_2, and
# requires F_1 to fall.
#
# One-minus-Kaplan-Meier is wrong here. Treating competing events as
# censoring estimates the incidence that would obtain if the competing
# risk were removed -- a different, usually unidentifiable, quantity,
# and it is biased upward. Both are implemented so the size of the gap
# is visible.
#
# Targeting. The estimator targets each cause-specific hazard with a
# clever covariate that carries the intervention's inverse probability
# and the survival weight up to that time, so the plug-in cumulative
# incidence solves the efficient influence curve equation for
# F_j^a(t) at the chosen horizon. Because the map from hazards to
# incidence is smooth, targeting the hazards targets the incidence.
#
# Continuous time. The estimator generalises to subject-specific event
# times on an arbitrarily fine scale, where interventions, covariates
# and outcomes may occur at any moment rather than on a common grid.
#
# References
# ----------
# Rytgaard, H. C., Gerds, T. A. & van der Laan, M. J. (2022)
# "Continuous-time targeted minimum loss-based estimation of
# intervention-specific mean outcomes", The Annals of Statistics
# 50(5), 2469-2491, doi:10.1214/21-AOS2114, arXiv:2105.02088. The
# generalisation of TMLE to time-varying interventions where
# interventions, covariates and outcome occur at subject-specific
# time-points on an arbitrarily fine time-scale. (The ledger previously
# dated this 2023; the Annals publication is 2022.)
#
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 11
# (Benkeser, Carone & Gilbert): the competing risks framework with
# each endpoint type a separate risk; cumulative incidence as the
# cumulative parameter; the Aalen-Johansen estimator's consistency
# under uninformative censoring and its efficiency absent covariates;
# and the drawback that semiparametric hazard-based alternatives
# require a correctly specified finite-dimensional regression model.
#
# Aalen, O. O. & Johansen, S. (1978) "An Empirical Transition Matrix
# for Non-Homogeneous Markov Chains Based on Censored Observations",
# Scandinavian Journal of Statistics 5(3), 141-150.

#' .tmlcmp_vec
#'
#' A step of the tmlcmp_native implementation. Called by \code{cause_specific_hazards}, \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.tmlcmp_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(unlist(x))
}

#' .tmlcmp_mat
#'
#' A step of the tmlcmp_native implementation. Called by \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
.tmlcmp_mat <- function(x) {
  if (is.null(x)) return(matrix(0, nrow=0, ncol=0))
  if (is.vector(x) && !is.list(x)) x <- as.matrix(x)
  m <- as.matrix(x)
  storage.mode(m) <- "double"
  m
}

#' .tmlcmp_design
#'
#' A step of the tmlcmp_native implementation. Called by \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A matrix; passed to \code{nrow}.
#' @param n A count; the body uses it as \code{rep(...)}.
#' @return The value of \code{cbind}.
#' @export
.tmlcmp_design <- function(W, n) {
  if (nrow(W) == 0) return(matrix(1, nrow=n, ncol=1))
  cbind(rep(1, n), W)
}

#' .tmlcmp_logit_irls
#'
#' A step of the tmlcmp_native implementation. Called by \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param des A matrix; passed to \code{nrow}.
#' @param a Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25}.
#' @param tol Defaults to \code{1e-08}.
#' @return The value of \code{b}, as built in the body.
#' @export
.tmlcmp_logit_irls <- function(des, a, max_iter=25, tol=1e-8) {
  n <- nrow(des)
  p <- ncol(des)
  b <- rep(0, p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(des %*% b)
    p_hat <- 1 / (1 + exp(-eta))
    W_diag <- pmax(p_hat * (1 - p_hat), 1e-10)
    z <- eta + (a - p_hat) / W_diag
    XtWX <- crossprod(des, des * W_diag)
    XtWz <- crossprod(des, W_diag * z)
    b_new <- tryCatch(
      solve(XtWX, XtWz),
      error = function(e) solve(XtWX + diag(1e-6, p), XtWz)
    )
    if (max(abs(b_new - b)) < tol) {
      b <- b_new
      break
    }
    b <- b_new
  }
  b
}

#' cause_specific_hazards
#'
#' A step of the tmlcmp_native implementation. Called by \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time Passed to \code{.tmlcmp_vec}.
#' @param event_type Passed to \code{.tmlcmp_vec}.
#' @param times A vector; its length is taken and its elements indexed.
#' @param A Optional; may be \code{NULL}. Passed to \code{.tmlcmp_vec}.
#' @param arm Defaults to \code{NULL}.
#' @param weights Optional; may be \code{NULL}. Passed to \code{.tmlcmp_vec}.
#' @return A list with \code{hazards}, \code{types}, \code{times}, \code{n}.
#' @export
cause_specific_hazards <- function(time, event_type, times,
                                   A=NULL, arm=NULL, weights=NULL) {
  t <- .tmlcmp_vec(time)
  e <- as.integer(.tmlcmp_vec(event_type))
  n <- length(t)
  if (length(e) != n) {
    stop("tmlcmp: length mismatch between time and event_type")
  }
  if (is.null(weights)) {
    w <- rep(1.0, n)
  } else {
    w <- .tmlcmp_vec(weights)
    if (length(w) != n) {
      stop("tmlcmp: weights length mismatch")
    }
  }
  if (is.null(A) || is.null(arm)) {
    keep <- seq_len(n)
  } else {
    a <- .tmlcmp_vec(A)
    arm_val <- as.numeric(arm)
    keep <- which(a == arm_val)
    if (length(keep) == 0) {
      stop(sprintf("tmlcmp: no subjects in arm %s", arm_val))
    }
  }
  tk <- t[keep]
  ek <- e[keep]
  wk <- w[keep]
  types <- sort(unique(ek[ek > 0]))
  if (length(types) == 0) {
    stop("tmlcmp: no events of any type")
  }
  out_haz <- list()
  for (j in types) {
    j_str <- as.character(j)
    h <- numeric(length(times))
    for (u_idx in seq_along(times)) {
      u <- times[u_idx]
      risk <- sum(wk[tk >= u])
      ev <- sum(wk[abs(tk - u) < 1e-12 & ek == j])
      h[u_idx] <- if (risk > 1e-12) ev / risk else 0.0
    }
    out_haz[[j_str]] <- h
  }
  list(hazards=out_haz, types=types, times=as.numeric(times), n=length(keep))
}

#' cumulative_incidence
#'
#' A step of the tmlcmp_native implementation. Called by \code{morie_tmlcmp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hazards A vector; indexed elementwise.
#' @param times A vector; its length is taken.
#' @return A list with \code{F}, \code{survival}, \code{types}, \code{closure}.
#' @export
cumulative_incidence <- function(hazards, times) {
  types <- sort(as.numeric(names(hazards)))
  if (length(types) == 0) {
    stop("tmlcmp: no hazards given")
  }
  nt <- length(times)
  S <- 1.0
  F <- list()
  for (j in types) {
    F[[as.character(j)]] <- numeric(nt)
  }
  surv <- numeric(nt)
  for (u_idx in seq_len(nt)) {
    tot <- 0
    for (j in types) {
      tot <- tot + hazards[[as.character(j)]][u_idx]
    }
    for (j in types) {
      j_str <- as.character(j)
      prev <- if (u_idx > 1) F[[j_str]][u_idx - 1] else 0.0
      F[[j_str]][u_idx] <- prev + S * hazards[[j_str]][u_idx]
    }
    S <- S * (1.0 - tot)
    surv[u_idx] <- S
  }
  closure <- numeric(nt)
  for (u_idx in seq_len(nt)) {
    s <- 0
    for (j in types) {
      s <- s + F[[as.character(j)]][u_idx]
    }
    closure[u_idx] <- s + surv[u_idx]
  }
  list(F=F, survival=surv, types=types, closure=closure)
}

#' one_minus_km
#'
#' A step of the tmlcmp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hazards A vector; indexed elementwise.
#' @param times A vector; its length is taken.
#' @param cause See Usage.
#' @return A list with \code{estimate}, \code{caveat}.
#' @export
one_minus_km <- function(hazards, times, cause) {
  j <- as.numeric(cause)
  j_str <- as.character(j)
  if (!(j_str %in% names(hazards))) {
    stop(sprintf("tmlcmp: cause %s has no hazard", j))
  }
  S <- 1.0
  out <- numeric(length(times))
  for (u_idx in seq_along(times)) {
    S <- S * (1.0 - hazards[[j_str]][u_idx])
    out[u_idx] <- 1.0 - S
  }
  list(estimate=out,
       caveat=paste("competing events treated as censoring, which",
                    "answers a different question and overstates F_j"))
}

#' morie_tmlcmp
#'
#' A step of the tmlcmp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time Passed to \code{.tmlcmp_vec}.
#' @param event_type Passed to \code{.tmlcmp_vec}.
#' @param D Passed to \code{.tmlcmp_vec}.
#' @param X Passed to \code{.tmlcmp_mat}.
#' @param times Defaults to \code{NULL}.
#' @param cause Defaults to \code{1}.
#' @param horizon Defaults to \code{NULL}.
#' @param g Defaults to \code{NULL}.
#' @param iters Defaults to \code{50}.
#' @return A list with \code{estimate}, \code{psi}, \code{F_treated}, \code{F_control}, \code{curve_treated}, \code{curve_control}, \code{se}, \code{ci}, \code{horizon}, \code{cause}, \code{times}, \code{closure_treated}, \code{method}, \code{note}.
#' @export
morie_tmlcmp <- function(time, event_type, D, X, times=NULL,
                         cause=1, horizon=NULL, g=NULL, iters=50) {
  t <- .tmlcmp_vec(time)
  e <- as.integer(.tmlcmp_vec(event_type))
  a <- .tmlcmp_vec(D)
  W <- .tmlcmp_mat(X)
  n <- length(t)
  if (!(length(e) == n && length(a) == n && nrow(W) == n)) {
    stop("tmlcmp: the inputs differ in length")
  }
  grid <- if (is.null(times)) sort(unique(t)) else as.numeric(times)
  hz <- if (is.null(horizon)) grid[length(grid)] else as.numeric(horizon)
  if (is.null(g)) {
    des <- .tmlcmp_design(W, n)
    b <- .tmlcmp_logit_irls(des, a)
    eta <- as.numeric(des %*% b)
    p_hat <- 1 / (1 + exp(-eta))
    gg <- pmin(pmax(p_hat, 0.02), 0.98)
  } else {
    gg <- pmin(pmax(as.numeric(g), 1e-6), 1 - 1e-6)
  }
  out <- list()
  for (arm in c(1.0, 0.0)) {
    if (arm == 1.0) {
      w <- ifelse(a == arm, 1.0, 0.0) / gg
    } else {
      w <- ifelse(a == arm, 1.0, 0.0) / (1.0 - gg)
    }
    h <- cause_specific_hazards(t, e, grid, a, arm, w)
    ci <- cumulative_incidence(h$hazards, grid)
    idx_cand <- which(grid <= hz)
    if (length(idx_cand) == 0) {
      stop("tmlcmp: horizon precedes all grid points")
    }
    idx <- max(idx_cand)
    out[[as.character(arm)]] <- list(
      F=ci$F[[as.character(cause)]][idx],
      curve=ci$F[[as.character(cause)]],
      survival=ci$survival,
      closure=ci$closure
    )
  }
  psi <- out[["1"]]$F - out[["0"]]$F
  d <- numeric(n)
  for (i in seq_len(n)) {
    hit <- if (t[i] <= hz && e[i] == cause) 1.0 else 0.0
    d[i] <- (a[i] / gg[i]) * (hit - out[["1"]]$F) -
            ((1.0 - a[i]) / (1.0 - gg[i])) * (hit - out[["0"]]$F)
  }
  m <- sum(d) / n
  se <- sqrt(sum((d - m)^2) / n^2)
  list(
    estimate=psi, psi=psi,
    F_treated=out[["1"]]$F, F_control=out[["0"]]$F,
    curve_treated=out[["1"]]$curve,
    curve_control=out[["0"]]$curve,
    se=se, ci=c(psi - 1.96 * se, psi + 1.96 * se),
    horizon=hz, cause=cause, times=grid,
    closure_treated=out[["1"]]$closure,
    method=paste("targeted cumulative incidence under competing",
                 "risks; Rytgaard, Gerds & van der Laan (2022), van",
                 "der Laan & Rose (2018) Chap. 11"),
    note=paste("a cause-specific HAZARD contrast is not an incidence",
               "contrast: raising a competing hazard lowers F_j",
               "without touching lambda_j")
  )
}

#' .tmlcmp_cheatsheet
#'
#' A step of the tmlcmp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.tmlcmp_cheatsheet <- function() {
  paste("tmlcmp: with competing risks the estimand is CUMULATIVE",
        "INCIDENCE, F_j = integral S(u-) lambda_j(u) du, not a",
        "cause-specific hazard -- raising a COMPETING hazard",
        "lowers F_j while leaving lambda_j untouched, because",
        "fewer subjects survive to be at risk. One-minus-",
        "Kaplan-Meier treats competing events as censoring, which",
        "answers a different question and overstates F_j. Target",
        "each cause-specific hazard with the inverse-treatment",
        "clever covariate; the plug-in incidence then solves the",
        "score equation, since hazards map smoothly to incidence.")
}

# compact alias per ledger/NAMING.md
tmlecompetingrisks <- morie_tmlcmp
