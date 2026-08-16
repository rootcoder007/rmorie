# R arm of sschin -- multiple imputation by chained equations, a Cox
# proportional-hazards fit per imputation and Rubin pooling.
# van Buuren, S. (2018) Flexible Imputation of Missing Data, 2nd ed., CRC
# Press, Ch. 3-4; Rubin, D. B. (1987) Multiple Imputation for Nonresponse in
# Surveys, Wiley, Ch. 3; Barnard, J. & Rubin, D. B. (1999) Biometrika 86(4),
# 948-955; Breslow, N. E. (1974) Biometrics 30(1), 89-99.
# Mirrors src/morie/fn/sschin.py.

.sschin_EPS <- 1e-12
.sschin_PRIMES <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
                    53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
                    109, 113)

#' .sschin_cholsolve
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.sschin_cholsolve <- function(A, b) {
  Lc <- chol(A)
  as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
}

# Least squares with a ridge scaled to the design, plus sigma.
#' Least squares with a ridge scaled to the design, plus sigma
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param ridge_rel Defaults to \code{1e-08}.
#' @return A list with \code{beta}, \code{sigma}.
#' @export
.sschin_ols <- function(X, y, ridge_rel = 1e-8) {
  n <- nrow(X); p <- ncol(X)
  A <- crossprod(X)
  scale_ <- sum(diag(A)) / p
  diag(A) <- diag(A) + ridge_rel * max(scale_, .sschin_EPS)
  beta <- .sschin_cholsolve(A, as.numeric(crossprod(X, y)))
  fit <- as.numeric(X %*% beta)
  dof <- max(n - p, 1L)
  sig2 <- sum((y - fit) ^ 2) / dof
  list(beta = beta, sigma = sqrt(max(sig2, 0.0)))
}

# Cox partial likelihood, Breslow ties, Newton-Raphson.
#' Cox partial likelihood, Breslow ties, Newton-Raphson
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param e See Usage.
#' @param X See Usage.
#' @param max_iter Defaults to \code{100L}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{beta}, \code{var}, \code{loglik}, \code{iterations}, \code{converged}.
#' @export
.sschin_cox_breslow <- function(t, e, X, max_iter = 100L, tol = 1e-10) {
  n <- length(t); p <- ncol(X)
  if (p == 0L) stop("sschin: the Cox model needs at least one covariate")
  ord <- order(-t, seq_len(n))
  beta <- numeric(p); it <- 0L; converged <- FALSE
  info <- matrix(0.0, p, p); ll <- 0.0
  for (it in seq_len(as.integer(max_iter))) {
    ll <- 0.0
    grad <- numeric(p)
    info <- matrix(0.0, p, p)
    s0 <- 0.0; s1 <- numeric(p); s2 <- matrix(0.0, p, p)
    pos <- 1L
    while (pos <= n) {
      tt <- t[ord[pos]]
      grp <- integer(0)
      while (pos <= n && t[ord[pos]] == tt) {
        grp <- c(grp, ord[pos]); pos <- pos + 1L
      }
      for (i in grp) {
        z <- sum(X[i, ] * beta)
        w <- exp(max(-500.0, min(500.0, z)))
        s0 <- s0 + w
        s1 <- s1 + w * X[i, ]
        s2 <- s2 + w * outer(X[i, ], X[i, ])
      }
      d <- grp[e[grp] > 0.5]
      if (length(d) == 0L) next
      dk <- length(d)
      for (i in d) {
        ll <- ll + sum(X[i, ] * beta)
        grad <- grad + X[i, ]
      }
      ll <- ll - dk * log(max(s0, 1e-300))
      grad <- grad - dk * s1 / max(s0, 1e-300)
      info <- info + dk * (s2 / max(s0, 1e-300) -
                             outer(s1, s1) / max(s0 * s0, 1e-300))
    }
    Ir <- info; diag(Ir) <- diag(Ir) + 1e-10
    step <- .sschin_cholsolve(Ir, grad)
    beta <- beta + step
    if (max(abs(step)) < tol) { converged <- TRUE; break }
  }
  Ir <- info; diag(Ir) <- diag(Ir) + 1e-10
  cov2 <- matrix(0.0, p, p)
  for (a in seq_len(p)) {
    ea <- numeric(p); ea[a] <- 1.0
    cov2[, a] <- .sschin_cholsolve(Ir, ea)
  }
  list(beta = beta, var = pmax(diag(cov2), 0.0), loglik = ll,
       iterations = as.integer(it), converged = converged)
}

# Two-sided Student-t quantile by Cornish-Fisher on the normal.
#' Two-sided Student-t quantile by Cornish-Fisher on the normal
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @param pq See Usage.
#' @param df See Usage.
#' @return A numeric value.
#' @export
.sschin_t_quantile <- function(pq, df) {
  z <- qnorm(pq)
  if (df > 1e8) return(z)
  g1 <- (z ^ 3 + z) / 4.0
  g2 <- (5.0 * z ^ 5 + 16.0 * z ^ 3 + 3.0 * z) / 96.0
  g3 <- (3.0 * z ^ 7 + 19.0 * z ^ 5 + 17.0 * z ^ 3 - 15.0 * z) / 384.0
  z + g1 / df + g2 / df ^ 2 + g3 / df ^ 3
}

#' morie_sschin_chained_imputation
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @param time See Usage.
#' @param event See Usage.
#' @param X See Usage.
#' @param mi_iter Defaults to \code{5L}.
#' @param cycles Defaults to \code{10L}.
#' @param ties Defaults to \code{"breslow"}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{hazard_ratio}, \code{std_error}, \code{total_variance}, \code{within_variance}, \code{between_variance}, \code{ci_lower}, \code{ci_upper}, \code{t_quantile}, \code{df}, \code{relative_increase_variance}, \code{fraction_missing_info}, \code{per_imputation}, \code{complete_case_coefficients}, \code{complete_case_se}, \code{n_complete_cases}, \code{n}, \code{p}, \code{m}, \code{cycles}, \code{n_missing}, \code{columns_imputed}, \code{n_events}, \code{df_complete}, \code{method}, \code{note}.
#' @export
morie_sschin_chained_imputation <- function(time, event, X, mi_iter = 5L,
                                            cycles = 10L, ties = "breslow") {
  tv <- as.numeric(time)
  ev <- as.numeric(event)
  # NOT a numeric coercion of the whole block: a missing entry is exactly
  # what cannot be coerced, so the rows are kept and tested for missingness
  Xr <- if (is.matrix(X)) X else if (is.data.frame(X)) as.matrix(X) else
    if (is.list(X)) do.call(rbind, lapply(X, function(r)
      vapply(r, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)))
    else matrix(as.numeric(X), ncol = 1L)
  storage.mode(Xr) <- "double"
  n <- length(tv)
  if (n == 0L) stop("sschin: no observations")
  if (length(ev) != n || nrow(Xr) != n)
    stop(sprintf(paste0("sschin: time, event and X must agree in length ",
                        "(%d, %d, %d)"), n, length(ev), nrow(Xr)))
  p <- ncol(Xr)
  m <- as.integer(mi_iter)
  if (m < 2L)
    stop(paste0("sschin: multiple imputation needs at least two ",
                "imputations -- the between-imputation variance is ",
                "undefined for m = 1"))
  if (m > length(.sschin_PRIMES))
    stop(sprintf("sschin: at most %d imputations", length(.sschin_PRIMES)))
  if (ties != "breslow")
    stop(sprintf(paste0("sschin: only the Breslow handling of ties is ",
                        "implemented, got '%s'"), ties))
  if (!any(ev > 0.5))
    stop("sschin: no events -- the partial likelihood is flat")

  miss <- is.na(Xr)
  obs <- Xr; obs[miss] <- 0.0
  n_missing <- sum(miss)
  cols_missing <- which(apply(miss, 2L, any))
  for (a in cols_missing) if (all(miss[, a]))
    stop(sprintf(paste0("sschin: column %d is missing for every ",
                        "observation and cannot be imputed"), a - 1L))

  colmean <- vapply(seq_len(p), function(a) {
    ok <- obs[!miss[, a], a]
    if (length(ok) > 0L) sum(ok) / length(ok) else 0.0
  }, 0)

  ests <- vector("list", m); vars_ <- vector("list", m)
  per <- vector("list", m)
  for (ell in seq_len(m)) {
    # one deterministic normal stream per imputation; a different base
    # means genuinely different draws, so B is not degenerate
    draws <- .s03normdraws(max(n_missing * as.integer(cycles), 1L),
                           .sschin_PRIMES[ell])
    pos <- 0L
    F_ <- obs
    for (a in seq_len(p)) F_[miss[, a], a] <- colmean[a]
    for (cc in seq_len(as.integer(cycles))) {
      for (a in cols_missing) {
        rows_obs <- which(!miss[, a])
        rows_mis <- which(miss[, a])
        others <- setdiff(seq_len(p), a)
        # the outcome belongs in the imputation model: imputing a covariate
        # without it biases the fitted hazard ratio towards the null
        # (White & Royston 2009)
        rowf <- function(i) c(1.0, F_[i, others], ev[i],
                              log(max(tv[i], 1e-12)))
        Xo <- do.call(rbind, lapply(rows_obs, rowf))
        yo <- F_[rows_obs, a]
        if (length(rows_obs) <= ncol(Xo)) {
          beta <- c(sum(yo) / length(yo), rep(0.0, ncol(Xo) - 1L))
          sig <- 0.0
        } else {
          fit <- .sschin_ols(Xo, yo)
          beta <- fit$beta; sig <- fit$sigma
        }
        for (i in rows_mis) {
          rr <- rowf(i)
          mu <- sum(rr * beta)
          F_[i, a] <- mu + sig * draws[(pos %% length(draws)) + 1L]
          pos <- pos + 1L
        }
      }
    }
    fit <- .sschin_cox_breslow(tv, ev, F_)
    ests[[ell]] <- fit$beta
    vars_[[ell]] <- fit$var
    per[[ell]] <- list(coefficients = fit$beta, variance = fit$var,
                       loglik = fit$loglik, iterations = fit$iterations,
                       converged = fit$converged)
  }

  E <- do.call(rbind, ests)
  V <- do.call(rbind, vars_)
  qbar <- colSums(E) / m
  ubar <- colSums(V) / m
  B <- vapply(seq_len(p), function(a) sum((E[, a] - qbar[a]) ^ 2) / (m - 1),
              0)
  Tv <- ubar + (1.0 + 1.0 / m) * B
  se <- sqrt(pmax(Tv, 0.0))
  n_events <- sum(ev > 0.5)
  dfcom <- max(n_events - p, 1L)
  riv <- numeric(p); fmi <- numeric(p); df <- numeric(p)
  for (a in seq_len(p)) {
    r <- (1.0 + 1.0 / m) * B[a] / max(ubar[a], 1e-300)
    riv[a] <- r
    lam <- (r + 2.0 / (dfcom + 3.0)) / (r + 1.0)
    fmi[a] <- lam
    if (B[a] <= 0.0) {
      df[a] <- dfcom
    } else {
      dold <- (m - 1.0) / max(lam * lam, 1e-300)
      dobs <- (dfcom + 1.0) / (dfcom + 3.0) * dfcom * (1.0 - lam)
      df[a] <- dold * dobs / (dold + dobs)
    }
  }
  tq <- vapply(seq_len(p), function(a) .sschin_t_quantile(0.975, df[a]), 0)

  cc <- which(!apply(miss, 1L, any))
  if (length(cc) > p && any(ev[cc] > 0.5)) {
    cfit <- .sschin_cox_breslow(tv[cc], ev[cc], obs[cc, , drop = FALSE])
    cb <- cfit$beta
    cc_se <- sqrt(pmax(cfit$var, 0.0))
  } else {
    cb <- rep(NaN, p); cc_se <- rep(NaN, p)
  }

  list(estimate = qbar, coefficients = qbar, hazard_ratio = exp(qbar),
       std_error = se, total_variance = Tv,
       within_variance = ubar, between_variance = B,
       ci_lower = qbar - tq * se, ci_upper = qbar + tq * se,
       t_quantile = tq, df = df,
       relative_increase_variance = riv, fraction_missing_info = fmi,
       per_imputation = per,
       complete_case_coefficients = cb, complete_case_se = cc_se,
       n_complete_cases = as.integer(length(cc)),
       n = as.integer(n), p = as.integer(p), m = as.integer(m),
       cycles = as.integer(cycles),
       n_missing = as.integer(n_missing),
       # REPORTED indices follow the Python spec and are 0-based
       columns_imputed = as.numeric(cols_missing - 1L),
       n_events = as.integer(n_events), df_complete = as.integer(dfcom),
       method = paste0("multiple imputation by chained equations ",
                       "(norm.nob, outcome included in the imputation ",
                       "model), Cox proportional hazards with Breslow ties ",
                       "per imputation, pooled by Rubin's rules with the ",
                       "Barnard-Rubin degrees of freedom (van Buuren 2018 ",
                       "Ch. 3-4; Rubin 1987 Ch. 3)"),
       note = paste0("between_variance is exactly zero when nothing is ",
                     "missing, so the pooled standard error then equals the ",
                     "complete-data one; fraction_missing_info is how much ",
                     "of the final variance came from not knowing the fills"))
}

#' .sschin_cheatsheet
#'
#' Part of the sschin_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.sschin_cheatsheet <- function() {
  paste0("sschin: morie_sschin_chained_imputation(time, event, X, mi_iter) ",
         "-> MICE imputation, per-imputation Cox fits and Rubin-pooled ",
         "hazard ratios (van Buuren 2018; Rubin 1987)")
}

morie_sschin <- morie_sschin_chained_imputation
