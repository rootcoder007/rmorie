# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Recurrent-event Cox shelf: counting-process Newton core shared by
# Agrec (Andersen-Gill 1982), Pwpgt (Prentice-Williams-Peterson 1981)
# and Wlwmm (Wei-Lin-Weissfeld 1989), plus the truncated
# time-dependent concordance Survtdc (Antolini et al 2005).
# Bit-identical to the Python in src/morie/fn/_recur_core.py: Breslow
# ties, Newton on the stratified counting-process partial likelihood.

#' .morie_cox_counting
#'
#' A step of the recur_native implementation. Called by \code{Agrec}, \code{Pwpgt}, \code{Shfrm} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param start Coerced to numeric by the body, with \code{as.numeric}.
#' @param stop Coerced to numeric by the body, with \code{as.numeric}.
#' @param event Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param strata Optional; may be \code{NULL}. A vector; its length is taken.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-09}.
#' @param offset Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{beta}, \code{se}, \code{cov}, \code{loglik}, \code{n_iter}, \code{n_events}.
#' @export
.morie_cox_counting <- function(start, stop, event, X, strata = NULL,
                                max_iter = 50L, tol = 1e-9,
                                offset = NULL) {
  s <- as.numeric(start)
  t <- as.numeric(stop)
  e <- as.numeric(event)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  n <- length(t)
  if (length(s) != n || length(e) != n || nrow(Xm) != n) {
    stop("start, stop, event and X must have equal length", call. = FALSE)
  }
  if (any(t <= s)) stop("every interval needs stop > start", call. = FALSE)
  if (!all(e == 0 | e == 1)) stop("event must be 0 or 1", call. = FALSE)
  p <- ncol(Xm)
  if (is.null(strata)) strata <- rep(0L, n)
  if (length(strata) != n) {
    stop("strata must match the number of rows", call. = FALSE)
  }
  if (is.null(offset)) offs <- rep(0, n) else {
    offs <- as.numeric(offset)
    if (length(offs) != n) {
      stop("offset must match the number of rows", call. = FALSE)
    }
  }
  n_events <- sum(e)
  if (n_events == 0) stop("no events in the data", call. = FALSE)
  beta <- rep(0, p)
  loglik <- 0
  info <- matrix(0, p, p)
  it <- 0L
  for (it in seq_len(max_iter)) {
    eta <- pmax(pmin(as.vector(Xm %*% beta) + offs, 500), -500)
    w <- exp(eta)
    U <- rep(0, p)
    info <- matrix(0, p, p)
    loglik <- 0
    for (g in unique(strata)) {
      idx <- which(strata == g)
      ts <- sort(unique(t[idx][e[idx] == 1]))
      for (tk in ts) {
        D <- idx[t[idx] == tk & e[idx] == 1]
        R <- idx[s[idx] < tk & tk <= t[idx]]
        S0 <- sum(w[R])
        S1 <- as.vector(crossprod(Xm[R, , drop = FALSE], w[R]))
        S2 <- crossprod(Xm[R, , drop = FALSE] * w[R], Xm[R, , drop = FALSE])
        d <- length(D)
        xbar <- S1 / S0
        loglik <- loglik + sum(eta[D]) - d * log(S0)
        U <- U + colSums(Xm[D, , drop = FALSE]) - d * xbar
        info <- info + d * (S2 / S0 - tcrossprod(xbar))
      }
    }
    step <- solve(info, U)
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  cov <- solve(info)
  dg <- diag(cov)
  if (any(!is.finite(dg)) || any(dg <= 0) || max(abs(beta)) > 50) {
    stop("partial likelihood is monotone or information singular",
         call. = FALSE)
  }
  list(beta = beta, se = sqrt(diag(cov)), cov = cov, loglik = loglik,
       n_iter = it, n_events = n_events)
}

#' Andersen-Gill counting-process Cox model for recurrent events
#'
#' Intensity lambda_i(t | H) = Y_i(t) lambda_0(t) exp(beta' X_i) with
#' at-risk indicator Y_i(t) = 1 on (start_i, stop_i]. Newton-Raphson on
#' the Breslow partial likelihood over counting-process risk sets. With
#' one interval per subject starting at zero this reduces exactly to the
#' ordinary Cox model.
#'
#' @param start,stop Left-open interval endpoints, stop > start.
#' @param event 1 if an event ends the interval, 0 if censored.
#' @param X Covariate matrix, one row per interval.
#' @param max_iter,tol Newton-Raphson controls.
#' @return List with \code{estimate}, \code{se}, \code{cov},
#'   \code{loglik}, \code{n_iter}, \code{n_events}, \code{method}.
#' @references Andersen, P. K. and Gill, R. D. (1982), Annals of
#'   Statistics 10(4), 1100-1120.
#' @export
#' @examples
#' set.seed(1)
#' occ <- rep(1:3, times = 20)
#' start <- (occ - 1) * 10
#' stop <- occ * 10 - runif(60, 0, 3)
#' event <- rbinom(60, 1, 0.6)
#' X <- matrix(rnorm(60 * 2), 60, 2)
#' fit <- Agrec(start, stop, event, X)
#' length(fit$estimate) == 2L
Agrec <- function(start, stop, event, X, max_iter = 50L, tol = 1e-9) {
  fit <- .morie_cox_counting(start, stop, event, X,
                             max_iter = max_iter, tol = tol)
  list(estimate = fit$beta, se = fit$se, cov = fit$cov,
       loglik = fit$loglik, n_iter = fit$n_iter, n_events = fit$n_events,
       method = "Andersen-Gill (1982) counting-process Cox, Breslow ties")
}

#' Prentice-Williams-Peterson conditional gap-time model
#'
#' Stratified Cox fit on gap = stop - start with stratum = occurrence
#' number and a common beta: lambda_k(t | H) =
#' lambda_0k(t - t_(k-1)) exp(beta' X).
#'
#' @param start,stop Interval endpoints on the total-time scale.
#' @param event 1 if an event ends the interval, 0 if censored.
#' @param X Covariate matrix, one row per interval.
#' @param occurrence Occurrence number of each interval (stratum).
#' @param max_iter,tol Newton-Raphson controls.
#' @return List as in \code{\link{Agrec}}.
#' @references Prentice, R. L., Williams, B. J. and Peterson, A. V.
#'   (1981), Biometrika 68(2), 373-379.
#' @export
#' @examples
#' set.seed(1)
#' occ <- rep(1:3, times = 20)
#' start <- (occ - 1) * 10
#' stop <- occ * 10 - runif(60, 0, 3)
#' event <- rbinom(60, 1, 0.6)
#' X <- matrix(rnorm(60 * 2), 60, 2)
#' fit <- Pwpgt(start, stop, event, X, occurrence = occ)
#' length(fit$estimate) == 2L
Pwpgt <- function(start, stop, event, X, occurrence,
                  max_iter = 50L, tol = 1e-9) {
  gap <- as.numeric(stop) - as.numeric(start)
  fit <- .morie_cox_counting(rep(0, length(gap)), gap, event, X,
                             strata = occurrence,
                             max_iter = max_iter, tol = tol)
  list(estimate = fit$beta, se = fit$se, cov = fit$cov,
       loglik = fit$loglik, n_iter = fit$n_iter, n_events = fit$n_events,
       method = "Prentice-Williams-Peterson (1981) gap-time stratified Cox, Breslow ties")
}

#' Wei-Lin-Weissfeld marginal model for multivariate failure times
#'
#' Per-occurrence marginal Cox models on the total-time scale plus the
#' common-effect summary: the stratified fit with a single beta across
#' occurrence strata (WLW Section 3).
#'
#' @param time Total time on study for each row.
#' @param event 1 if the row's occurrence was observed, else 0.
#' @param X Covariate matrix, one row per (subject, occurrence).
#' @param occurrence Occurrence number of each row.
#' @param max_iter,tol Newton-Raphson controls.
#' @return List with \code{estimate} (common beta), \code{se},
#'   \code{cov}, \code{per_event_beta} (named list), \code{loglik},
#'   \code{n_iter}, \code{n_events}, \code{method}.
#' @references Wei, L. J., Lin, D. Y. and Weissfeld, L. (1989), JASA
#'   84(408), 1065-1073.
#' @export
#' @examples
#' set.seed(1)
#' occ <- rep(1:3, times = 20)
#' stop <- occ * 10 - runif(60, 0, 3)
#' event <- rbinom(60, 1, 0.6)
#' X <- matrix(rnorm(60 * 2), 60, 2)
#' fit <- Wlwmm(time = stop, event, X, occurrence = occ)
#' is.list(fit)
Wlwmm <- function(time, event, X, occurrence, max_iter = 50L, tol = 1e-9) {
  t <- as.numeric(time)
  e <- as.numeric(event)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  per_event <- list()
  for (k in sort(unique(occurrence))) {
    idx <- which(occurrence == k)
    if (!any(e[idx] == 1)) next
    sub <- tryCatch(
      .morie_cox_counting(rep(0, length(idx)), t[idx], e[idx],
                          Xm[idx, , drop = FALSE],
                          max_iter = max_iter, tol = tol),
      error = function(err) NULL)
    if (is.null(sub)) next
    per_event[[as.character(k)]] <- sub$beta
  }
  fit <- .morie_cox_counting(rep(0, length(t)), t, e, Xm,
                             strata = occurrence,
                             max_iter = max_iter, tol = tol)
  list(estimate = fit$beta, se = fit$se, cov = fit$cov,
       per_event_beta = per_event, loglik = fit$loglik,
       n_iter = fit$n_iter, n_events = fit$n_events,
       method = "Wei-Lin-Weissfeld (1989) marginal model, total-time stratified Cox, Breslow ties")
}

#' Truncated time-dependent concordance index
#'
#' C^td(t) = P(marker_i > marker_j | T_i < T_j, T_i <= t, delta_i = 1),
#' marker ties counting one half -- Antolini, Boracchi and Biganzoli
#' (2005) with a scalar marker, equal to Harrell's C truncated at t.
#'
#' @param time Observed times.
#' @param event Event indicators (1 = event, 0 = censored).
#' @param marker Scalar risk marker, higher = riskier.
#' @param t Truncation horizon.
#' @return List with \code{estimate}, \code{concordant}, \code{tied},
#'   \code{comparable}, \code{t}, \code{method}.
#' @references Antolini, L., Boracchi, P. and Biganzoli, E. (2005),
#'   Statistics in Medicine 24(24), 3927-3944.
#' @export
#' @examples
#' Survtdc(time = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), event = c(0, 1, 0, 1, 1, 0, 1, 0), marker = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
Survtdc <- function(time, event, marker, t) {
  tt <- as.numeric(time)
  e <- as.numeric(event)
  m <- as.numeric(marker)
  t <- as.numeric(t)[1]
  n <- length(tt)
  if (length(e) != n || length(m) != n) {
    stop("time, event and marker must have equal length", call. = FALSE)
  }
  conc <- 0
  tied <- 0
  comp <- 0L
  for (i in seq_len(n)) {
    if (e[i] != 1 || tt[i] > t) next
    for (j in seq_len(n)) {
      if (j == i || !(tt[i] < tt[j])) next
      comp <- comp + 1L
      if (m[i] > m[j]) conc <- conc + 1
      else if (m[i] == m[j]) tied <- tied + 0.5
    }
  }
  if (comp == 0L) stop("no comparable pairs at horizon t", call. = FALSE)
  list(estimate = (conc + tied) / comp, concordant = conc,
       tied = 2 * tied, comparable = comp, t = t,
       method = "Antolini-Boracchi-Biganzoli (2005) truncated time-dependent concordance")
}
