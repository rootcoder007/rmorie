# SPDX-License-Identifier: AGPL-3.0-or-later
#' Discrete-time survival with a complementary log-log link
#'
#' Prentice and Gloeckler (1978), Regression analysis of grouped survival
#' data with application to breast cancer data, Biometrics 34(1), 57-67:
#' grouping a proportional hazards model into intervals gives exactly
#' log(-log(1 - h_t(x))) = alpha_t + beta' x, with beta identical to the
#' continuous-time Cox coefficient -- which is why this link, and not the
#' logit, makes the discrete model a GROUPED proportional hazards model.
#' The paper is paywalled; the link and that identification are quoted in
#' their standard published form.  Fitted by Newton-Raphson on the
#' person-period likelihood.
#'
#' @param time_discrete interval index of the event or censoring.
#' @param event event indicator.
#' @param X covariates.
#' @param max_iter,tol,ridge Newton controls.
#' @return list: estimate, alpha, beta, hazard, loglik, intervals, n,
#'   n_person_periods, method.
#' @keywords internal
#' @examples
#' Disctime(c(1, 2, 2, 3), c(1, 0, 1, 1), matrix(c(0, 1, 0, 1), 4, 1))$beta
#' @export
Disctime <- function(time_discrete, event, X = NULL, max_iter = 100,
                     tol = 1e-12, ridge = 1e-8) {
  t <- as.integer(.s03vec(time_discrete)); e <- .s03vec(event); n <- length(t)
  Xr <- if (!is.null(X)) .s03mat(X) else matrix(0, n, 0)
  p <- ncol(Xr)
  ivals <- sort(unique(t)); Tn <- length(ivals)
  rows <- list(); ys <- numeric(0)
  for (i in seq_len(n)) {
    for (j in seq_len(Tn)) {
      if (ivals[j] > t[i]) break
      d <- as.numeric(seq_len(Tn) == j)
      rows[[length(rows) + 1L]] <- c(d, if (p) as.numeric(Xr[i, ]) else numeric(0))
      ys <- c(ys, if (ivals[j] == t[i] && e[i] > 0.5) 1 else 0)
    }
  }
  m <- length(rows); q <- Tn + p
  Rm <- do.call(rbind, rows)
  beta <- numeric(q); ll <- -Inf
  for (it in seq_len(as.integer(max_iter))) {
    gr <- numeric(q); H <- matrix(0, q, q); ll <- 0
    for (r in seq_len(m)) {
      eta <- 0
      for (a in seq_len(q)) eta <- eta + Rm[r, a] * beta[a]
      ee <- if (eta < 700) exp(eta) else exp(700)
      h <- 1 - exp(-ee)
      if (h < 1e-12) h <- 1e-12
      if (h > 1 - 1e-12) h <- 1 - 1e-12
      ll <- ll + ys[r] * log(h) + (1 - ys[r]) * log(1 - h)
      dh <- ee * exp(-ee)
      s <- (ys[r] / h - (1 - ys[r]) / (1 - h)) * dh
      wgt <- dh * dh / (h * (1 - h))
      for (a in seq_len(q)) {
        gr[a] <- gr[a] + Rm[r, a] * s
        for (b in seq_len(q)) H[a, b] <- H[a, b] + Rm[r, a] * wgt * Rm[r, b]
      }
    }
    step <- .s03ridgesolve(H, gr, ridge)
    mx <- 0
    for (a in seq_len(q)) {
      beta[a] <- beta[a] + step[a]
      if (abs(step[a]) > mx) mx <- abs(step[a])
    }
    if (mx < tol) break
  }
  haz <- numeric(Tn)
  for (j in seq_len(Tn)) haz[j] <- 1 - exp(-exp(beta[j]))
  list(estimate = if (p) beta[Tn + 1L] else NaN, alpha = beta[seq_len(Tn)],
       beta = if (p) beta[seq(Tn + 1L, q)] else numeric(0), hazard = haz,
       loglik = ll, intervals = ivals, n = n, n_person_periods = m,
       method = "Grouped proportional hazards by complementary log-log (Prentice and Gloeckler 1978)")
}
