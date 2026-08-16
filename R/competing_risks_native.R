# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Competing-risks and shared-frailty shelf. R mirrors of the morie.fn
# modules crrcsh, crrfgs, fgsbh and coxfrl, over the Cox core in
# cox_native.R.
#
# The pairing of crrcsh and crrfgs is the point of the file: they answer
# different questions on the same data and will disagree, and the
# disagreement is information rather than a bug.

#' Cause-specific hazard model
#'
#' A Cox model for one cause, treating every competing event as
#' censored. This is the AETIOLOGICAL question -- what drives the rate
#' of this cause among those still at risk of it.
#'
#' It is not the incidence question, and the standard error is not the
#' issue: \eqn{1 - \exp(-\Lambda)} from a cause-specific fit OVERSTATES
#' the actual probability of the event, because it implicitly assumes
#' the competing events could be prevented. Use
#' \code{\link{morie_competing_risks_fg}} when the quantity of interest
#' is risk.
#'
#' @param time follow-up times.
#' @param event_type 0 for censored, \code{cause} for the event of
#'   interest, any other value for a competing event.
#' @param X covariate matrix.
#' @param cause the cause to model.
#' @param ties \code{"efron"} or \code{"breslow"}.
#' @return list with \code{beta}, \code{se}, \code{z}, \code{p_value},
#'   \code{hazard_ratio}, \code{loglik}, plus the event counts by type.
#' @references Prentice, R. L. et al. (1978). The analysis of failure
#'   times in the presence of competing risks. \emph{Biometrics}, 34(4),
#'   541-554.
#' @examples
#' set.seed(3)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- rexp(100)
#' ct <- sample(0:2, 100, TRUE)
#' morie_cause_specific_hazard(tt, ct, X)$hazard_ratio
#' @export
morie_cause_specific_hazard <- function(time, event_type, X, cause = 1,
                                        ties = "efron") {
  t <- as.numeric(time)
  d <- as.vector(event_type)
  if (length(t) != length(d)) {
    stop(sprintf(
      "time has %d entries but event_type has %d",
      length(t), length(d)
    ), call. = FALSE)
  }
  if (!any(d == cause)) {
    stop(sprintf("no events of cause %s in event_type", cause), call. = FALSE)
  }
  e <- as.numeric(d == cause)
  prep <- .morie_cox_prepare(t, e, X)
  fit <- .morie_cox_fit(prep$t, prep$e, prep$X, ties = ties)
  se <- tryCatch(sqrt(pmax(diag(solve(fit$I)), 0)),
    error = function(err) rep(NA_real_, length(fit$beta))
  )
  z <- fit$beta / se
  list(
    beta = fit$beta, se = se, z = z,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    hazard_ratio = exp(fit$beta), loglik = fit$loglik,
    information = fit$I, n_cause = as.integer(sum(e)),
    n_competing = as.integer(sum(d != 0 & d != cause)),
    n_censored = as.integer(sum(d == 0)), cause = cause,
    n = length(t), converged = fit$converged,
    incidence_caveat = paste(
      "competing events are censored here, so",
      "1 - exp(-Lambda) OVERSTATES incidence; use",
      "the Fine-Gray model for actual risk"
    ),
    method = "cause_specific_hazard"
  )
}


#' .morie_fg_newton
#'
#' Part of the competing_risks_native implementation; see the file
#' header for the source it follows.
#'
#' @param t A vector; indexed elementwise.
#' @param e See Usage.
#' @param X A matrix; indexed by row and column.
#' @param competing See Usage.
#' @param Gfun See Usage.
#' @param Gi Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @param tol Defaults to \code{1e-09}.
#' @return A list with \code{beta}, \code{loglik}, \code{I}, \code{U}.
#' @export
.morie_fg_newton <- function(t, e, X, competing, Gfun, Gi, max_iter = 50L,
                             tol = 1e-9) {
  p <- ncol(X)
  beta <- numeric(p)
  utimes <- unique(sort(t[e == 1]))
  ll <- 0
  I <- matrix(0, p, p)
  U <- numeric(p)
  for (iter in seq_len(max_iter)) {
    w <- exp(pmax(pmin(as.vector(X %*% beta), 500), -500))
    ll <- 0
    U <- numeric(p)
    I <- matrix(0, p, p)
    for (ut in utimes) {
      # The Fine-Gray weight: subjects who already failed of a competing
      # cause stay in the risk set, discounted by the censoring survival
      # ratio. That retention is exactly what ties the model to incidence.
      wt <- ifelse(t >= ut, 1, ifelse(competing & t < ut, Gfun(ut)[1] / Gi, 0))
      inr <- wt > 0
      if (!any(inr)) next
      died <- t == ut & e == 1
      dcount <- sum(died)
      if (dcount == 0L) next
      ww <- wt[inr] * w[inr]
      Xr <- X[inr, , drop = FALSE]
      S0 <- sum(ww)
      S1 <- as.vector(ww %*% Xr)
      S2 <- t(Xr * ww) %*% Xr
      ll <- ll + sum(X[died, , drop = FALSE] %*% beta) -
        dcount * log(max(S0, 1e-300))
      mu <- S1 / max(S0, 1e-300)
      U <- U + colSums(X[died, , drop = FALSE]) - dcount * mu
      I <- I + dcount * (S2 / max(S0, 1e-300) - outer(mu, mu))
    }
    step <- as.vector(tryCatch(solve(I, U),
      error = function(err) .morie_ginv(I) %*% U
    ))
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  list(beta = beta, loglik = ll, I = I, U = U)
}


#' Fine-Gray subdistribution hazard model
#'
#' A weighted Cox model on the subdistribution hazard. Subjects who have
#' already failed from a competing cause REMAIN in the risk set, with
#' inverse-probability-of-censoring weights; that retention is what makes
#' the fitted model map directly onto cumulative incidence, and it is
#' also what makes the coefficient hard to interpret mechanistically.
#'
#' A Fine-Gray hazard ratio is a statement about RISK, not about
#' mechanism. Report it beside the cause-specific fit rather than
#' instead of it; when the two disagree, the disagreement is the finding.
#'
#' @inheritParams morie_cause_specific_hazard
#' @return list with \code{beta}, \code{se}, \code{p_value},
#'   \code{subdistribution_hazard_ratio}, \code{weights}, \code{loglik}.
#' @references Fine, J. P. and Gray, R. J. (1999). A proportional
#'   hazards model for the subdistribution of a competing risk.
#'   \emph{JASA}, 94(446), 496-509.
#' @examples
#' set.seed(3)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- rexp(100)
#' ct <- sample(0:2, 100, TRUE)
#' morie_competing_risks_fg(tt, ct, X)$subdistribution_hazard_ratio
#' @export
morie_competing_risks_fg <- function(time, event_type, X, cause = 1,
                                     ties = "efron") {
  t <- as.numeric(time)
  d <- as.vector(event_type)
  if (length(t) != length(d)) {
    stop(sprintf(
      "time has %d entries but event_type has %d",
      length(t), length(d)
    ), call. = FALSE)
  }
  if (!any(d == cause)) {
    stop(sprintf("no events of cause %s in event_type", cause), call. = FALSE)
  }
  e <- as.numeric(d == cause)
  prep <- .morie_cox_prepare(t, e, X)
  km <- .morie_km_estimate(t, as.numeric(d == 0))
  ct <- km$times
  csurv <- km$survival
  Gfun <- function(u) {
    u <- as.numeric(u)
    if (length(ct) == 0L) {
      return(rep(1, length(u)))
    }
    pos <- findInterval(u, ct)
    ifelse(pos >= 1L, csurv[pmin(pmax(pos, 1L), length(csurv))], 1)
  }
  competing <- d != 0 & d != cause
  Gi <- pmax(Gfun(t), 1e-8)
  fg <- .morie_fg_newton(t, e, prep$X, competing, Gfun, Gi)
  se <- tryCatch(sqrt(pmax(diag(solve(fg$I)), 0)),
    error = function(err) rep(NA_real_, length(fg$beta))
  )
  z <- fg$beta / se
  list(
    beta = fg$beta, se = se, z = z,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    subdistribution_hazard_ratio = exp(fg$beta),
    hazard_ratio = exp(fg$beta), weights = Gi, loglik = fg$loglik,
    n_cause = as.integer(sum(e)), n_competing = as.integer(sum(competing)),
    cause = cause, n = length(t), converged = TRUE,
    interpretation_caveat = paste(
      "the risk set keeps subjects who already",
      "failed from a competing cause; a",
      "Fine-Gray hazard ratio is a statement",
      "about RISK, not about mechanism"
    ),
    method = "competing_risks_fg"
  )
}


#' Fine-Gray cumulative incidence
#'
#' The Fine-Gray fit plus the baseline subdistribution hazard it
#' implies, converted to cumulative incidence.
#'
#' Here \eqn{1 - \exp(-\Lambda)} IS a probability, which is precisely
#' what it is not for a cause-specific fit -- that is the whole reason
#' the subdistribution hazard was defined.
#'
#' @param time follow-up times.
#' @param cause event-type vector, as for
#'   \code{\link{morie_cause_specific_hazard}}.
#' @param X covariate matrix.
#' @param of_cause the cause to model.
#' @param ... passed to \code{\link{morie_competing_risks_fg}}.
#' @return list with \code{beta}, \code{se}, \code{times},
#'   \code{baseline_cif}, \code{cumulative_incidence} (n x times),
#'   \code{cumhazard}.
#' @references Fine, J. P. and Gray, R. J. (1999). A proportional
#'   hazards model for the subdistribution of a competing risk.
#'   \emph{JASA}, 94(446), 496-509.
#' @examples
#' set.seed(3)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- rexp(100)
#' ct <- sample(0:2, 100, TRUE)
#' tail(morie_fine_gray_subdistribution_hazard(tt, ct, X)$baseline_cif, 1)
#' @export
morie_fine_gray_subdistribution_hazard <- function(time, cause, X,
                                                   of_cause = 1, ...) {
  fit <- morie_competing_risks_fg(time, cause, X, cause = of_cause, ...)
  t <- as.numeric(time)
  d <- as.vector(cause)
  e <- as.numeric(d == of_cause)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  bh <- .morie_cox_baseline(t, e, Xm, fit$beta,
    offset = log(pmax(fit$weights, 1e-12))
  )
  base_cif <- 1 - exp(-bh$cumhazard)
  lin <- exp(pmax(pmin(as.vector(Xm %*% fit$beta), 500), -500))
  cif <- 1 - exp(-outer(lin, bh$cumhazard))
  list(
    beta = fit$beta, se = fit$se, p_value = fit$p_value,
    subdistribution_hazard_ratio = fit$subdistribution_hazard_ratio,
    times = bh$times, baseline_cif = base_cif,
    cumulative_incidence = cif, cumhazard = bh$cumhazard,
    cause = of_cause, n = length(t),
    method = "fine_gray_subdistribution_hazard"
  )
}


#' Cox model with a shared gamma frailty
#'
#' One multiplicative random effect per cluster, gamma distributed with
#' mean 1 and variance theta, fitted by alternating a Cox fit at the
#' current frailties with the closed-form posterior update, all inside a
#' profile over theta.
#'
#' Theta is profiled over the MARGINAL likelihood rather than estimated
#' from the fitted frailties, and that is not a stylistic choice. The
#' posterior means are shrunk toward 1, so their sample variance is
#' badly biased downward -- it collapses to zero and takes the frailty
#' with it, leaving a model that reports independence no matter how
#' clustered the data are.
#'
#' Ignoring clustering leaves standard errors too SMALL, which is the
#' direction that produces false findings. Kendall's tau for this model
#' is \code{theta / (theta + 2)}.
#'
#' @inheritParams morie_breslow_tie_correction
#' @param cluster cluster label per subject.
#' @param theta frailty variance. Profiled when NULL.
#' @param max_iter,tol inner-loop controls.
#' @param ties \code{"efron"} or \code{"breslow"}.
#' @return list with \code{beta}, \code{se}, \code{hazard_ratio},
#'   \code{theta}, \code{kendall_tau}, \code{frailty}, \code{clusters}.
#' @references Therneau, T. M. and Grambsch, P. M. (2000). \emph{Modeling
#'   Survival Data}, Ch. 9. Springer.
#' @examples
#' set.seed(4)
#' X <- matrix(rnorm(120), ncol = 1)
#' cl <- rep(1:20, each = 6)
#' tt <- rexp(120, exp(X * 0.5) * rep(rgamma(20, 2, 2), each = 6))
#' round(morie_cox_frailty(tt, rep(1, 120), X, cl)$kendall_tau, 3)
#' @export
morie_cox_frailty <- function(time, event, X, cluster, theta = NULL,
                              max_iter = 30L, tol = 1e-6, ties = "efron") {
  d <- .morie_cox_prepare(time, event, X)
  cl <- as.vector(cluster)
  if (length(cl) != length(d$t)) {
    stop(sprintf(
      "cluster has %d entries but time has %d",
      length(cl), length(d$t)
    ), call. = FALSE)
  }
  levels_ <- unique(sort(cl))
  idx <- match(cl, levels_)
  K <- length(levels_)
  if (K == length(d$t)) {
    stop(paste(
      "every cluster has one member, so a shared frailty is not",
      "identifiable"
    ), call. = FALSE)
  }

  inner <- function(th_val) {
    logw <- numeric(length(d$t))
    beta_l <- numeric(ncol(d$X))
    frail_l <- rep(1, K)
    conv_l <- FALSE
    ll_l <- -Inf
    it_l <- 0L
    d_k <- numeric(K)
    r_k <- numeric(K)
    for (it_l in seq_len(max_iter)) {
      f <- .morie_cox_fit(d$t, d$e, d$X, ties = ties, offset = logw)
      ll_l <- f$loglik
      bh <- .morie_cox_baseline(d$t, d$e, d$X, f$beta, offset = logw)
      pos <- findInterval(d$t, bh$times)
      H_at <- ifelse(pos >= 1L, bh$cumhazard[pmax(pos, 1L)], 0)
      risk <- exp(pmax(pmin(as.vector(d$X %*% f$beta), 500), -500)) * H_at
      d_k <- as.vector(tapply(d$e, factor(idx, levels = seq_len(K)), sum))
      d_k[is.na(d_k)] <- 0
      r_k <- as.vector(tapply(risk, factor(idx, levels = seq_len(K)), sum))
      r_k[is.na(r_k)] <- 0
      frail_new <- (1 / th_val + d_k) / (1 / th_val + r_k)
      delta <- max(max(abs(f$beta - beta_l)), max(abs(frail_new - frail_l)))
      beta_l <- f$beta
      frail_l <- frail_new
      logw <- log(pmax(frail_l[idx], 1e-12))
      if (delta < tol) {
        conv_l <- TRUE
        break
      }
    }
    list(
      beta = beta_l, frailty = frail_l, d_k = d_k, r_k = r_k,
      loglik = ll_l, n_iter = as.integer(it_l), converged = conv_l
    )
  }

  marginal <- function(th_val) {
    r <- inner(th_val)
    a <- 1 / th_val
    sum(lgamma(a + r$d_k) - lgamma(a) - (a + r$d_k) * log1p(th_val * r$r_k) +
      r$d_k * log(th_val)) + r$loglik
  }

  th <- if (is.null(theta)) {
    opt <- stats::optimize(function(lg) -marginal(exp(lg)),
      interval = c(log(1e-4), log(10)), tol = 1e-3
    )
    exp(opt$minimum)
  } else {
    as.numeric(theta)
  }
  res <- inner(th)
  logw <- log(pmax(res$frailty[idx], 1e-12))
  f1 <- .morie_cox_fit(d$t, d$e, d$X,
    ties = ties, offset = logw,
    max_iter = 1L
  )
  Iinv <- tryCatch(solve(f1$I), error = function(err) .morie_ginv(f1$I))
  scale <- 1 + th * (mean(tabulate(idx, nbins = K)) - 1)
  se <- sqrt(pmax(diag(Iinv) * max(scale, 1), 0))
  z <- res$beta / se
  list(
    beta = res$beta, se = se, z = z,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    hazard_ratio = exp(res$beta), theta = th,
    kendall_tau = th / (th + 2), frailty = res$frailty,
    clusters = levels_, n_clusters = as.integer(K), loglik = res$loglik,
    n = length(d$t), n_iter = res$n_iter, converged = res$converged,
    method = "cox_frailty"
  )
}
