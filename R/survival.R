# Survival analysis: Kaplan-Meier, Nelson-Aalen, log-rank and Cox.
#
# R mirror of morie/src/morie/fn/_survival_core.py.  Both arms are
# verified against the `survival` package, which is the reference
# implementation for this material, so morie provides these natively.
#
# Censoring is the whole difficulty: a subject still alive at the end of
# follow-up carries the information that it survived that long, and
# either discarding it or counting it as an event biases every estimate.

#' Kaplan-Meier product-limit estimator
#'
#' `S(t) = prod (1 - d_i/n_i)` with Greenwood's variance and a log-log
#' confidence interval, which cannot leave `\[0, 1\]` the way a symmetric
#' one can.  Note `survival::survfit`'s `std.err` is the error of the
#' CUMULATIVE HAZARD; `se` here is the error of `S(t)` and `se_cumhaz`
#' is survfit's, the two differing by a factor of `S`.
#' @param time follow-up times
#' @param event 1 for an event, 0 for censored
#' @param alpha significance level for the interval
#' @return list with `time`, `surv`, `se`, `se_cumhaz`, `lower`,
#'   `upper`, `n_risk`, `n_event`
#' @export
#' @examples
#' morie_kaplan_meier(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0))
morie_kaplan_meier <- function(time, event, alpha = 0.05) {
  if (length(time) != length(event))
    stop("time and event must have the same length")
  if (!all(event %in% c(0, 1))) stop("event must be 0 or 1")
  ut <- sort(unique(time[event == 1]))
  z <- stats::qnorm(1 - alpha / 2)
  surv <- 1
  vs <- 0
  S <- se <- sec <- lo <- hi <- nr <- ne <- numeric(length(ut))
  for (i in seq_along(ut)) {
    u <- ut[i]
    n_i <- sum(time >= u)
    d_i <- sum(time == u & event == 1)
    surv <- surv * (1 - d_i / n_i)
    if (n_i > d_i) vs <- vs + d_i / (n_i * (n_i - d_i))
    S[i] <- surv
    se[i] <- surv * sqrt(vs)
    sec[i] <- sqrt(vs)
    nr[i] <- n_i
    ne[i] <- d_i
    if (surv > 0 && surv < 1) {
      ll <- log(-log(surv))
      sd <- sqrt(vs) / abs(log(surv))
      lo[i] <- exp(-exp(ll + z * sd))
      hi[i] <- exp(-exp(ll - z * sd))
    } else { lo[i] <- surv
    hi[i] <- surv }
  }
  list(time = ut, surv = S, se = se, se_cumhaz = sec, lower = lo,
       upper = hi, n_risk = nr, n_event = ne, n = length(time),
       n_events = sum(event))
}

#' Nelson-Aalen cumulative hazard
#'
#' `H(t) = sum d_i/n_i`.  Better behaved than `-log(KM)` in small
#' samples, and the natural scale on which to judge proportional
#' hazards.
#' @inheritParams morie_kaplan_meier
#' @return list with `time`, `cumhaz`, `se` and `surv = exp(-H)`
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_nelson_aalen(V, V)
morie_nelson_aalen <- function(time, event) {
  ut <- sort(unique(time[event == 1]))
  H <- 0
  V <- 0
  ch <- se <- sv <- numeric(length(ut))
  for (i in seq_along(ut)) {
    u <- ut[i]
    n_i <- sum(time >= u)
    d_i <- sum(time == u & event == 1)
    H <- H + d_i / n_i
    V <- V + d_i / n_i^2
    ch[i] <- H
    se[i] <- sqrt(V)
    sv[i] <- exp(-H)
  }
  list(time = ut, cumhaz = ch, se = se, surv = sv)
}

#' Log-rank test
#'
#' Compares observed failures per group with the number expected under
#' a common hazard, standardised by the hypergeometric variance.
#' Equivalent to `survival::survdiff` with `rho = 0`.
#' @inheritParams morie_kaplan_meier
#' @param group grouping vector
#' @return list with `statistic`, `df`, `p_value`, `observed`, `expected`
#' @export
#' @examples
#' morie_logrank_test(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1,
#' 0), group = c("a", "b", "c"))
morie_logrank_test <- function(time, event, group) {
  lev <- sort(unique(group))
  k <- length(lev)
  if (k < 2) stop("need at least 2 groups")
  obs <- exp_ <- numeric(k)
  V <- matrix(0, k, k)
  for (u in sort(unique(time[event == 1]))) {
    n_i <- sum(time >= u)
    d_i <- sum(time == u & event == 1)
    nj <- vapply(lev, function(g) sum(time >= u & group == g), numeric(1))
    dj <- vapply(lev, function(g)
      sum(time == u & event == 1 & group == g), numeric(1))
    obs <- obs + dj
    exp_ <- exp_ + d_i * nj / n_i
    if (n_i > 1) {
      f <- d_i * (n_i - d_i) / (n_i - 1)
      V <- V + f * (diag(nj / n_i, k, k) - outer(nj, nj) / n_i^2)
    }
  }
  m <- k - 1
  dif <- (obs - exp_)[seq_len(m)]
  stat <- as.numeric(t(dif) %*% solve(V[seq_len(m), seq_len(m),
                                       drop = FALSE], dif))
  list(statistic = stat, df = m,
       p_value = stats::pchisq(stat, m, lower.tail = FALSE),
       observed = obs, expected = exp_, groups = lev)
}

#' Cox proportional-hazards model
#'
#' `lambda(t | x) = lambda_0(t) exp(x' beta)`, fitted by Newton-Raphson
#' on the partial likelihood with Efron's tie handling -- the default in
#' `survival::coxph`, and materially better than Breslow's when ties are
#' common, as they are whenever follow-up is recorded in whole days.
#' The baseline hazard is left unspecified, which is the point of the
#' model; `exp(beta)` is a hazard ratio assumed constant over time.
#' @inheritParams morie_kaplan_meier
#' @param X covariate matrix
#' @param ties "efron" or "breslow"
#' @param max_iter,tol iteration controls
#' @param beta coefficient vector at which to evaluate the likelihood
#' @return list with `coef`, `se`, `z`, `p_value`, `hazard_ratio`,
#'   `loglik` and the likelihood-ratio test
#' @export
#' @examples
#' # X must vary AMONG THE EVENTS: if every event shares one covariate
#' # value, or X is a linear function of the event times, the partial
#' # likelihood has no unique maximum and the Hessian is singular.
#' morie_cox_ph(time = c(1, 2, 3, 4, 5, 6, 7, 8),
#'              event = c(0, 1, 0, 1, 1, 0, 1, 0),
#'              X = c(0, 1, 0, 0, 1, 1, 0, 1))
morie_cox_ph <- function(time, event, X, ties = "efron",
                         max_iter = 50, tol = 1e-9) {
  X <- as.matrix(X)
  n <- length(time)
  p <- ncol(X)
  if (sum(event) == 0) stop("no events: the partial likelihood is empty")
  ut <- sort(unique(time[event == 1]))
  beta <- numeric(p)
  info <- function(b) {
    w <- exp(as.numeric(X %*% b))
    g <- numeric(p)
    H <- matrix(0, p, p)
    for (u in ut) {
      rk <- time >= u
      dd <- time == u & event == 1
      m <- sum(dd)
      s0r <- sum(w[rk])
      s1r <- colSums(X[rk, , drop = FALSE] * w[rk])
      s2r <- crossprod(X[rk, , drop = FALSE], X[rk, , drop = FALSE] * w[rk])
      s0d <- sum(w[dd])
      s1d <- colSums(X[dd, , drop = FALSE] * w[dd])
      s2d <- crossprod(X[dd, , drop = FALSE], X[dd, , drop = FALSE] * w[dd])
      g <- g + colSums(X[dd, , drop = FALSE])
      steps <- if (ties == "breslow") 1 else m
      for (r in seq_len(steps) - 1) {
        fr <- if (ties == "breslow") 0 else r / m
        cnt <- if (ties == "breslow") m else 1
        s0 <- s0r - fr * s0d
        s1 <- s1r - fr * s1d
        s2 <- s2r - fr * s2d
        g <- g - cnt * s1 / s0
        H <- H + cnt * (s2 / s0 - outer(s1, s1) / s0^2)
      }
    }
    list(g = g, H = H)
  }
  sing_msg <- paste("singular information matrix in the Cox Newton step:",
                    "the covariates are collinear (or one is a linear",
                    "function of the event times), so the partial",
                    "likelihood has no unique maximum")
  for (it in seq_len(max_iter)) {
    i <- info(beta)
    # Test the rank EXPLICITLY rather than waiting for solve() to throw.
    # Whether a singular system raises is a LAPACK detail: Windows
    # raises "system is exactly singular" where the reference BLAS on
    # Linux and macOS returns a value, so the same degenerate design
    # errored on one platform and silently produced garbage on the
    # others -- and a local example sweep could never catch it.
    if (qr(i$H)$rank < ncol(i$H)) stop(sing_msg, call. = FALSE)
    step <- tryCatch(solve(i$H, i$g),
                     error = function(e) stop(sing_msg, call. = FALSE))
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  H <- info(beta)$H
  if (qr(H)$rank < ncol(H)) stop(sing_msg, call. = FALSE)
  V <- tryCatch(solve(H), error = function(e) stop(sing_msg, call. = FALSE))
  # The same monotone/singular diagnosis .morie_cox_counting already
  # applies. It matters here because whether solve() RAISES on a
  # near-singular system is a LAPACK detail -- Windows raises where the
  # reference BLAS returns a value -- so without this the identical
  # degenerate design failed on Windows and quietly produced a diverged
  # coefficient everywhere else.
  dg <- diag(V)
  if (any(!is.finite(dg)) || any(dg <= 0) || max(abs(beta)) > 50) {
    stop(sing_msg, call. = FALSE)
  }
  se <- sqrt(diag(V))
  z <- beta / se
  ll <- morie_cox_partial_loglik(time, event, X, beta, ties)
  ll0 <- morie_cox_partial_loglik(time, event, X, numeric(p), ties)
  # Perfect separation: the partial likelihood reaches its supremum of 1,
  # so the log-likelihood is exactly 0 and beta is not identified -- it
  # is only wandering toward +/-Inf until the iteration stops. Testing
  # this rather than the magnitude of beta or the conditioning of H is
  # what makes the diagnosis identical on every platform: whether
  # solve() raises on the resulting matrix is a LAPACK detail, and for a
  # single covariate the condition number is 1 by construction.
  if (is.finite(ll) && ll > -1e-10 && is.finite(ll0) && ll0 < -1e-10) {
    stop(sing_msg, call. = FALSE)
  }
  list(coef = beta, se = se, z = z,
       p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       hazard_ratio = exp(beta), vcov = V, loglik = ll,
       loglik_null = ll0, lr_statistic = 2 * (ll - ll0),
       lr_p_value = stats::pchisq(2 * (ll - ll0), p, lower.tail = FALSE),
       n = n, n_events = sum(event), ties = ties)
}

#' @rdname morie_cox_ph
#' @export
morie_cox_partial_loglik <- function(time, event, X, beta,
                                     ties = "efron") {
  X <- as.matrix(X)
  eta <- as.numeric(X %*% beta)
  ll <- 0
  for (u in sort(unique(time[event == 1]))) {
    rk <- time >= u
    dd <- time == u & event == 1
    m <- sum(dd)
    sr <- sum(exp(eta[rk]))
    sd <- sum(exp(eta[dd]))
    ll <- ll + sum(eta[dd])
    if (ties == "breslow") ll <- ll - m * log(sr)
    else for (r in seq_len(m) - 1) ll <- ll - log(sr - r * sd / m)
  }
  ll
}

#' Harrell's concordance index
#'
#' Over the pairs whose order is known despite censoring, the
#' proportion in which the subject failing first carried the higher
#' predicted risk, ties counted as a half.  0.5 is chance.
#' @inheritParams morie_kaplan_meier
#' @param predicted_risk predicted risk score, higher meaning sooner
#' @return list with `c_index` and the pair counts
#' @export
#' @examples
#' morie_concordance_index(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0,
#' 1, 0), predicted_risk = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_concordance_index <- function(time, event, predicted_risk) {
  n <- length(time)
  conc <- disc <- tied <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    if (time[i] < time[j] && event[i] == 1) { lo <- i
    hi <- j }
    else if (time[j] < time[i] && event[j] == 1) { lo <- j
    hi <- i }
    else if (time[i] == time[j] && event[i] == 1 && event[j] == 1) {
      if (predicted_risk[i] != predicted_risk[j]) tied <- tied + 1
      next
    } else next
    if (predicted_risk[lo] > predicted_risk[hi]) conc <- conc + 1
    else if (predicted_risk[lo] < predicted_risk[hi]) disc <- disc + 1
    else tied <- tied + 1
  }
  tot <- conc + disc + tied
  if (tot == 0) stop("no comparable pairs")
  list(c_index = (conc + 0.5 * tied) / tot, concordant = conc,
       discordant = disc, tied = tied, n_pairs = tot)
}

# Pre-policy spellings of the estimators renamed when the survival
# family was de-externalized.  Kept working as aliases.

#' @rdname morie_kaplan_meier
#' @export
morie_survival_km <- morie_kaplan_meier

#' @rdname morie_nelson_aalen
#' @export
morie_survival_nelsonaalen <- morie_nelson_aalen

#' @rdname morie_logrank_test
#' @export
morie_survival_logrank <- morie_logrank_test

#' @rdname morie_cox_ph
#' @export
morie_survival_cox <- morie_cox_ph

#' @rdname morie_concordance_index
#' @export
morie_survival_concordance <- morie_concordance_index
