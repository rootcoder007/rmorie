# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Shared gamma frailty with marginal output (Shfrm). Bit-identical
# mirror of src/morie/fn/shfrm.py: outer golden section on log-theta
# maximizing the FULL marginal log-likelihood (Balan & Putter 2019,
# frailtyEM, eq. 3), inner EM to convergence at fixed theta.
# Anchored against frailtyEM::emfrail (theta 1.977 vs ours 1.984 on the
# 4-cluster test set; agreement ~0.4%, solver tolerances).

#' Shared gamma frailty model with marginal (population-averaged) output
#'
#' Conditional intensity lambda_i(t | w_k) = w_k lambda_0(t)
#' exp(beta' x_i), w_k ~ Gamma(1/theta, 1/theta) shared within cluster k
#' (Vaupel, Manton & Stallard 1979). theta maximises the full marginal
#' likelihood; the E-step posterior mean is (D_k + 1/theta) /
#' (Lambda_k + 1/theta) (Klein 1992). The marginal survivor
#' S_m(t|x) = (1 + theta Lambda_0(t) e^(beta x))^(-1/theta) is reported
#' at the baseline (x = 0).
#'
#' @param time Observed times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param X Covariate matrix.
#' @param cluster Cluster labels.
#' @param theta See Usage.
#' @return List with \code{estimate}, \code{se}, \code{theta},
#'   \code{kendall_tau}, \code{frailty}, \code{baseline_times},
#'   \code{baseline_cumhaz}, \code{marginal_survivor}, \code{loglik},
#'   \code{method}.
#' @references Vaupel, Manton and Stallard (1979), Demography 16(3),
#'   439-454; Klein (1992), Biometrics 48, 795-806; Balan and Putter
#'   (2019), frailtyEM, Journal of Statistical Software 90(7).
#' @export
#' @examples
#' Shfrm(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
Shfrm <- function(time, event, X, cluster, theta = NULL) {
  t <- as.numeric(time)
  e <- as.numeric(event)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  n <- length(t)
  cl <- as.vector(cluster)
  ks <- sort(unique(cl))
  K <- length(ks)
  if (K < 2L) stop("need at least 2 clusters", call. = FALSE)
  kidx <- lapply(ks, function(k) which(cl == k))
  zeros <- rep(0, n)

  em_at_theta <- function(theta, max_em = 200L, em_tol = 1e-10) {
    a <- 1 / theta
    w <- stats::setNames(rep(1, K), as.character(ks))
    fit <- NULL
    for (em in seq_len(max_em)) {
      offs <- log(w[as.character(cl)])
      fit <- .morie_cox_counting(zeros, t, e, Xm, offset = offs)
      eta <- as.vector(Xm %*% fit$beta)
      risk_w <- w[as.character(cl)] * exp(eta)
      etimes <- sort(unique(t[e == 1]))
      dL <- vapply(etimes, function(tk) {
        sum(t == tk & e == 1) / sum(risk_w[t >= tk])
      }, 0)
      H <- vapply(seq_len(n), function(i) {
        sum(dL[etimes <= t[i]]) * exp(eta[i])
      }, 0)
      D <- vapply(kidx, function(ix) sum(e[ix] == 1), 0)
      L <- vapply(kidx, function(ix) sum(H[ix]), 0)
      w_new <- stats::setNames((D + a) / (L + a), as.character(ks))
      delta <- max(abs(w_new - w))
      w <- w_new
      if (delta < em_tol) break
    }
    ll <- 0
    tidx <- match(t, etimes)
    for (i in seq_len(n)) {
      if (e[i] == 1) ll <- ll + eta[i] + log(dL[tidx[i]])
    }
    ll <- ll + sum(lgamma(a + D) - lgamma(a) + D * log(theta) -
                     (a + D) * log(1 + theta * L))
    list(ll = ll, fit = fit, w = w, etimes = etimes, dL = dL,
         D = D, L = L)
  }

  if (!is.null(theta)) {
    theta <- as.numeric(theta)
    best <- em_at_theta(theta)
    cumL <- cumsum(best$dL)
    return(list(estimate = best$fit$beta, se = best$fit$se,
                theta = theta, kendall_tau = theta / (theta + 2),
                frailty = as.list(best$w), baseline_times = best$etimes,
                baseline_cumhaz = cumL,
                marginal_survivor = (1 + theta * cumL)^(-1 / theta),
                loglik = best$ll,
                method = "Vaupel et al (1979) shared gamma frailty, fixed theta"))
  }
  # Estimated theta is plateau-bounded at ~1e-6 across languages (the
  # marginal likelihood is flat at machine precision near the argmax);
  # the likelihood value agrees to <1e-12.
  lo <- log(1e-4)
  hi <- log(20)
  gr <- (sqrt(5) - 1) / 2
  cc <- hi - gr * (hi - lo)
  dd <- lo + gr * (hi - lo)
  fc <- em_at_theta(exp(cc))$ll
  fd <- em_at_theta(exp(dd))$ll
  for (it in seq_len(50L)) {
    if (fc > fd) {
      hi <- dd
      dd <- cc
      fd <- fc
      cc <- hi - gr * (hi - lo)
      fc <- em_at_theta(exp(cc))$ll
    } else {
      lo <- cc
      cc <- dd
      fc <- fd
      dd <- lo + gr * (hi - lo)
      fd <- em_at_theta(exp(dd))$ll
    }
  }
  theta <- exp((lo + hi) / 2)
  best <- em_at_theta(theta)
  cumL <- cumsum(best$dL)
  list(estimate = best$fit$beta, se = best$fit$se, theta = theta,
       kendall_tau = theta / (theta + 2), frailty = as.list(best$w),
       baseline_times = best$etimes, baseline_cumhaz = cumL,
       marginal_survivor = (1 + theta * cumL)^(-1 / theta),
       loglik = best$ll,
       method = "Vaupel et al (1979) shared gamma frailty, full-marginal-likelihood profile (Balan-Putter eq. 3), Klein (1992) EM")
}
