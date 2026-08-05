# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-stage (longitudinal) TMLE for a sequential intervention
#'
#' van der Laan and Gruber (2012), Targeted minimum loss based estimation
#' of causal effects of multiple time point interventions, The
#' International Journal of Biostatistics 8(1), art. 9.  The estimator is
#' the sequential-regression one: E\[Y(d1, d2)\] = E\[E\[E\[Y | A2 = d2, L2, A1
#' = d1, L1] | A1 = d1, L1]] is estimated from the inside out, with a
#' targeting fluctuation at each stage using the cumulative clever
#' covariate H_t = 1{A_1 = d_1, ..., A_t = d_t} / prod_{s<=t} g_s.  The
#' article is open access but was not retrievable here; both are quoted in
#' their standard published form.  The effect returned is E\[Y(1,1)\] -
#' E\[Y(0,0)\]; all four regime means are reported.
#'
#' @param y outcome.
#' @param D1,D2 stage-1 and stage-2 treatment indicators.
#' @param X1,X2 stage-1 and stage-2 covariates.
#' @param alpha interval level.
#' @return list: estimate, se, ci_lo, ci_hi, ey11, ey10, ey01, ey00, n,
#'   method.
#' @keywords internal
#' @examples
#' Tmle2stage(c(1, 0, 1, 0, 1, 1), c(1, 0, 1, 0, 1, 0),
#'            c(1, 1, 0, 0, 1, 0))$ey11
#' @export
Tmle2stage <- function(y, D1, D2, X1 = NULL, X2 = NULL, alpha = 0.05) {
  yv <- .s03vec(y); d1 <- .s03vec(D1); d2 <- .s03vec(D2); n <- length(yv)
  regime <- function(a1, a2) {
    Z1 <- .s03design(X1, n); Z2 <- .s03design(X2, n)
    H2 <- cbind(Z2, d1)
    g2 <- vapply(.s03matvec(H2, .s03logit(H2, d2, 60L)), .s03sigmoid, 0)
    g1 <- vapply(.s03matvec(Z1, .s03logit(Z1, d1, 60L)), .s03sigmoid, 0)
    Q2 <- cbind(1, d2, d1, Z2[, -1, drop = FALSE], Z1[, -1, drop = FALSE])
    b2 <- .s03lstsq(Q2, yv)
    qbar2 <- numeric(n)
    for (i in seq_len(n)) {
      row <- c(1, as.numeric(a2), as.numeric(a1), Z2[i, -1], Z1[i, -1])
      s <- 0
      for (j in seq_along(b2)) s <- s + b2[j] * row[j]
      qbar2[i] <- s
    }
    Q1 <- cbind(1, d1, Z1[, -1, drop = FALSE])
    b1 <- .s03lstsq(Q1, qbar2)
    qbar1 <- numeric(n)
    for (i in seq_len(n)) {
      row <- c(1, as.numeric(a1), Z1[i, -1])
      s <- 0
      for (j in seq_along(b1)) s <- s + b1[j] * row[j]
      qbar1[i] <- s
    }
    m <- 0
    for (v in qbar1) m <- m + v / n
    ic <- numeric(n)
    for (i in seq_len(n)) {
      ind1 <- if (abs(d1[i] - a1) < 0.5) 1 else 0
      ind2 <- if (abs(d2[i] - a2) < 0.5) 1 else 0
      p1 <- if (a1 > 0.5) g1[i] else 1 - g1[i]
      p2 <- if (a2 > 0.5) g2[i] else 1 - g2[i]
      h2 <- if (p1 > 0 && p2 > 0) ind1 * ind2 / (p1 * p2) else 0
      h1 <- if (p1 > 0) ind1 / p1 else 0
      ic[i] <- h2 * (yv[i] - qbar2[i]) + h1 * (qbar2[i] - qbar1[i]) + qbar1[i] - m
    }
    list(m = m, ic = ic)
  }
  r11 <- regime(1, 1); r10 <- regime(1, 0)
  r01 <- regime(0, 1); r00 <- regime(0, 0)
  est <- r11$m - r00$m
  v <- 0
  for (i in seq_len(n)) { dd <- r11$ic[i] - r00$ic[i]; v <- v + dd * dd }
  se <- if (n) sqrt(v / (n * n)) else NaN
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = est, se = se, ci_lo = est - z * se, ci_hi = est + z * se,
       ey11 = r11$m, ey10 = r10$m, ey01 = r01$m, ey00 = r00$m, n = n,
       method = "Sequential-regression TMLE for a two-stage intervention (van der Laan and Gruber 2012)")
}
