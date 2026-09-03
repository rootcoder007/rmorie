# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-stage joint model for a longitudinal outcome and survival
#'
#' Stage 1 fits the random-intercept linear mixed model
#' \code{y = X beta + b_i + e} by EM; stage 2 fits a Cox
#' proportional-hazards model for the event time with the empirical
#' Bayes random effects as an extra covariate, so its coefficient is the
#' association parameter eta.  eta = 0 means the longitudinal trajectory
#' carries no information about the hazard.  The Cox model uses the
#' Breslow handling of ties and Newton-Raphson.  Both stages are
#' deterministic with a fixed iteration count.  The two-stage estimator
#' is biased towards zero relative to the full joint likelihood; that is
#' a documented property of the method, not an artefact here.
#'
#' Formula: y_ij = x_ij' beta + b_i + e_ij;
#'   lambda_i(t) = lambda_0(t) exp(gamma' W_i + eta b_i).
#'
#' @param long_y Longitudinal measurements, one per record.
#' @param time Event or censoring time (constant within subject).
#' @param event Event indicator, 0 or 1 (constant within subject).
#' @param X Fixed-effect design for the longitudinal model; an intercept
#'   is prepended.  May be \code{NULL}.
#' @param Z Subject-level covariates for the survival model; one row per
#'   subject or per record.  May be \code{NULL}.
#' @param cluster Subject label of each record.
#' @return List with \code{estimate} (eta), \code{eta}, \code{se},
#'   \code{gamma}, \code{gamma_se}, \code{beta}, \code{tau2},
#'   \code{sigma2}, \code{icc}, \code{b}, \code{partial_loglik},
#'   \code{n_subjects}, \code{n_events}, \code{n_covariates}, \code{n},
#'   \code{method}.
#' @references Henderson, Diggle and Dobson (2000), Joint modelling of
#'   longitudinal measurements and event time data, Biostatistics
#'   1(4):465-480, \doi{10.1093/biostatistics/1.4.465}; Rizopoulos
#'   (2012), Joint Models for Longitudinal and Time-to-Event Data,
#'   Chapman and Hall/CRC, chapter 4. \doi{10.1201/b12208}
#' @export
#' @examples
#' Jntlmm(long_y = c(1, 2, 3, 4, 5, 6, 7, 8), time = c(1, 2, 3, 4, 5, 6, 7, 8), event =
#' c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8), Z = c(1, 2, 3, 4, 5, 6, 7,
#' 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
Jntlmm <- function(long_y, time, event, X, Z, cluster) {
  y <- .s03vec(long_y)
  n <- length(y)
  if (n == 0L) stop("joint_longitudinal_survival: long_y is empty")
  tv <- .s03vec(time)
  ev <- .s03vec(event)
  cl <- .s03vec(cluster)
  if (length(tv) != n || length(ev) != n || length(cl) != n)
    stop("joint_longitudinal_survival: long_y, time, event and cluster have different lengths")
  if (any(ev != 0 & ev != 1)) stop("joint_longitudinal_survival: event must be 0 or 1")
  Xd <- .s03design(X, n)
  if (nrow(Xd) != n) stop("joint_longitudinal_survival: X and long_y have different lengths")
  ord <- sort(unique(cl))
  g <- length(ord)
  if (g < 3L) stop("joint_longitudinal_survival: need at least three subjects")
  st <- numeric(g)
  sd <- numeric(g)
  for (i in seq_len(n)) {
    k <- match(cl[i], ord)
    st[k] <- tv[i]
    sd[k] <- ev[i]
  }
  if (sum(sd) == 0) stop("joint_longitudinal_survival: no events observed")
  fit <- .jnt_lmm_ri(y, Xd, cl, ord)
  b <- fit$b
  if (is.null(Z)) {
    Zs <- matrix(b, g, 1L)
  } else {
    rows <- .s03mat(Z)
    if (nrow(rows) == g) {
      Zs <- cbind(rows, b)
    } else if (nrow(rows) == n) {
      first <- matrix(0, g, ncol(rows))
      seen <- rep(FALSE, g)
      for (i in seq_len(n)) {
        k <- match(cl[i], ord)
        if (!seen[k]) { first[k, ] <- rows[i, ]
        seen[k] <- TRUE }
      }
      Zs <- cbind(first, b)
    } else {
      stop("joint_longitudinal_survival: Z must have one row per subject or per record")
    }
  }
  cx <- .jnt_cox(st, sd, Zs)
  eta <- cx$beta[ncol(Zs)]
  .t1_result(estimate = eta, eta = eta, se = cx$se[ncol(Zs)],
             gamma = cx$beta, gamma_se = cx$se, beta = fit$beta,
             tau2 = fit$t2, sigma2 = fit$s2, icc = fit$t2 / (fit$t2 + fit$s2),
             b = b, partial_loglik = cx$ll, n_subjects = g, n_events = sum(sd),
             n_covariates = ncol(Zs), n = n,
             method = "stage 1 random-intercept LMM by EM, stage 2 Cox on the empirical Bayes b_i, Henderson et al (2000)")
}

#' @keywords internal
#' @noRd
.jnt_lmm_ri <- function(y, X, grp, ord, iters = 200L) {
  n <- length(y)
  p <- ncol(X)
  g <- length(ord)
  idx <- lapply(ord, function(k) which(grp == k))
  beta <- .s03lstsq(X, y)
  r0 <- y - as.numeric(.s03matvec(X, beta))
  s2 <- sum(r0^2) / max(1, n - p)
  t2 <- s2
  eb <- numeric(g)
  vb <- numeric(g)
  for (it in seq_len(iters)) {
    fitv <- as.numeric(.s03matvec(X, beta))
    for (k in seq_len(g)) {
      m <- length(idx[[k]])
      prec <- m / s2 + 1 / t2
      resid <- sum(y[idx[[k]]] - fitv[idx[[k]]])
      eb[k] <- (resid / s2) / prec
      vb[k] <- 1 / prec
    }
    adj <- y - eb[match(grp, ord)]
    beta <- .s03lstsq(X, adj)
    fitv <- as.numeric(.s03matvec(X, beta))
    ss <- 0
    for (k in seq_len(g)) for (i in idx[[k]]) {
      e <- y[i] - fitv[i] - eb[k]
      ss <- ss + e * e + vb[k]
    }
    s2 <- ss / n
    t2 <- sum(eb^2 + vb) / g
  }
  list(beta = beta, t2 = t2, s2 = s2, b = eb)
}

#' @keywords internal
#' @noRd
.jnt_cox <- function(t, d, Z, iters = 50L) {
  g <- length(t)
  p <- ncol(Z)
  beta <- numeric(p)
  ord <- order(t, seq_len(g))
  H <- diag(p)
  for (it in seq_len(iters)) {
    u <- numeric(p)
    H <- matrix(0, p, p)
    ll <- 0
    for (kk in seq_len(g)) {
      i <- ord[kk]
      if (d[i] != 1) next
      risk <- ord[t[ord] >= t[i]]
      s0 <- 0
      s1 <- numeric(p)
      s2 <- matrix(0, p, p)
      for (j in risk) {
        e <- exp(sum(Z[j, ] * beta))
        s0 <- s0 + e
        for (a in seq_len(p)) {
          s1[a] <- s1[a] + e * Z[j, a]
          for (cc in seq_len(p)) s2[a, cc] <- s2[a, cc] + e * Z[j, a] * Z[j, cc]
        }
      }
      ll <- ll + sum(Z[i, ] * beta) - log(s0)
      for (a in seq_len(p)) {
        u[a] <- u[a] + Z[i, a] - s1[a] / s0
        for (cc in seq_len(p)) H[a, cc] <- H[a, cc] + s2[a, cc] / s0 - (s1[a] / s0) * (s1[cc] / s0)
      }
    }
    step <- .s03ridgesolve(H, u, 1e-10)
    beta <- beta + step
    if (max(abs(step)) < 1e-12) break
  }
  inv <- vapply(seq_len(p), function(j) .s03ridgesolve(H, as.numeric(seq_len(p) == j), 1e-10), numeric(p))
  se <- vapply(seq_len(p), function(j) if (inv[j, j] > 0) sqrt(inv[j, j]) else NaN, 0)
  list(beta = beta, se = se, ll = ll)
}
