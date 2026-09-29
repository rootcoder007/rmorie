# SPDX-License-Identifier: AGPL-3.0-or-later

#' Dorfman group testing operating characteristics
#'
#' Expected tests E(T) = 1 + I P(group positive) and pooling accuracy (PSe, PSp,
#' PPPV, PNPV) for one group, allowing specimen-specific probabilities.
#'
#' @param p Common probability (with size) or one probability per specimen.
#' @param se,sp Accuracy of the group test.
#' @param se_r,sp_r Accuracy of retests.
#' @param size Group size when p is a single number.
#' @return list(expected_tests, tests_per_specimen, p_group_positive, individual, overall).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (6.26)-(6.29).
#' @examples
#' GtDorfman(0.05, 0.95, 0.97, 0.9, 0.99, size = 6)$expected_tests
#' @export
GtDorfman <- function(p, se, sp, se_r = se, sp_r = sp, size = NULL) {
  ps <- if (!is.null(size)) rep(p, size) else p
  I <- length(ps)
  if (I < 1 || any(ps < 0 | ps >= 1)) stop("need at least one specimen and 0 <= p < 1", call. = FALSE)
  allneg <- prod(1 - ps)
  pz <- se * (1 - allneg) + (1 - sp) * allneg
  et <- if (I == 1) 1 else 1 + I * pz
  pse <- if (I > 1) se * se_r else se
  if (I == 1) {
    py <- se * ps + (1 - sp) * (1 - ps)
    psp <- sp
  } else {
    py <- (1 - sp) * (1 - sp_r) * allneg + se * se_r * ps + se * (1 - sp_r) * (1 - ps) * (1 - allneg / (1 - ps))
    psp <- 1 - (py - pse * ps) / (1 - ps)
  }
  ind <- data.frame(PSe = pse, PSp = psp, PPPV = pse * ps / py, PNPV = psp * (1 - ps) / (1 - py), PY1 = py)
  overall <- c(PSe = if (sum(ps) > 0) sum(ps * pse) / sum(ps) else pse, PSp = sum((1 - ps) * psp) / sum(1 - ps),
               PPPV = sum(ps * pse) / sum(py), PNPV = sum((1 - ps) * psp) / sum(1 - py))
  list(expected_tests = et, tests_per_specimen = et / I, p_group_positive = pz, individual = ind, overall = overall)
}

#' Prevalence from group testing
#'
#' MLE 1 - (1 - x/n)^(1/m), Clopper-Pearson, Wilson score or Wald interval,
#' and the Bilder-Tebbs empirical Bayes estimates.
#'
#' @param x Positive groups.
#' @param m Group size.
#' @param n Number of groups.
#' @param ci "CP", "Wald" or "score".
#' @param alpha Level.
#' @param b_range Search interval for b.
#' @return list(estimate, ci, theta, b_hat, eb1, eb2).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (6.30)-(6.31).
#' @examples
#' GtPrev(3, 7, 24)$ci
#' @export
GtPrev <- function(x, m, n, ci = c("CP", "Wald", "score"), alpha = 0.05, b_range = c(0.0001, 1000)) {
  ci <- match.arg(ci)
  if (x < 0 || x > n || m < 1 || n < 1) stop("need 0 <= x <= n, m >= 1 and n >= 1", call. = FALSE)
  th <- x / n
  tr <- function(t) 1 - (1 - t)^(1 / m)
  est <- tr(th)
  z <- qnorm(1 - alpha / 2)
  interval <- switch(ci,
    CP = tr(c(if (x == 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1), if (x == n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x))),
    score = {
      cen <- (x + z^2 / 2) / (n + z^2)
      half <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
      tr(c(max(0, cen - half), min(1, cen + half)))
    },
    Wald = {
      s <- if (th > 0 && th < 1) sqrt(th * (1 - th) / n) * (1 - th)^(1 / m - 1) / m else 0
      c(max(0, est - z * s), min(1, est + z * s))
    })
  g <- function(b) log(b) + lgamma(n - x + b / m) - lgamma(n + b / m + 1)
  b <- stats::optimize(g, b_range, maximum = TRUE)$maximum
  eb1 <- 1 - exp(lgamma(n - x + b / m + 1 / m) + lgamma(n + b / m + 1) - lgamma(n + b / m + 1 + 1 / m) - lgamma(n - x + b / m))
  eb2 <- 1 - (1 - (x + 1) / (n + b / m + 1))^(1 / m)
  list(estimate = est, ci = interval, theta = th, b_hat = b, eb1 = eb1, eb2 = eb2)
}

#' Group testing regression by Xie's EM algorithm
#'
#' @param z Group responses (0/1), one per group.
#' @param group Group label (0-based) of each individual.
#' @param X Individual design matrix with intercept.
#' @param se,sp Group test accuracy.
#' @param max_iter,tol EM controls.
#' @return list(beta, loglik, iterations, converged).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (6.32)-(6.33).
#' @examples
#' xs <- ((7 * (0:39)) %% 13) / 4 - 1.5
#' z <- c(1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0)
#' GtRegEM(z, (0:39) %/% 2, cbind(1, xs), se = 0.95, sp = 0.95)$beta
#' @export
GtRegEM <- function(z, group, X, se = 1, sp = 1, max_iter = 1000, tol = 1e-10) {
  X <- as.matrix(X)
  g <- group + 1
  if (se + sp <= 1 || max(g) > length(z)) stop("group labels must index z and Se + Sp > 1", call. = FALSE)
  pz_of <- function(pi) {
    q <- vapply(seq_along(z), function(k) prod(1 - pi[g == k]), numeric(1))
    se - (se + sp - 1) * q
  }
  beta <- rep(0, ncol(X))
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    pi <- drop(stats::plogis(X %*% beta))
    pz <- pz_of(pi)[g]
    w <- ifelse(z[g] == 1, se * pi / pz, (1 - se) * pi / (1 - pz))
    b <- beta
    for (s in 1:100) {
      pr <- drop(stats::plogis(X %*% b))
      step <- solve(crossprod(X, pr * (1 - pr) * X), crossprod(X, w - pr))
      b <- b + drop(step)
      if (max(abs(step)) < 1e-13) break
    }
    change <- max(abs(b - beta))
    beta <- b
    if (change < tol) {
      conv <- TRUE
      break
    }
  }
  pz <- pz_of(drop(stats::plogis(X %*% beta)))
  list(beta = beta, loglik = sum(ifelse(z == 1, log(pz), log(1 - pz))), iterations = it, converged = conv)
}

#' Simultaneous pairwise marginal independence (SPMI)
#'
#' Sum of the I x J 2 x 2 Pearson statistics with Bonferroni and second-order
#' Rao-Scott tests.
#'
#' @param W n x I 0/1 matrix.
#' @param Y n x J 0/1 matrix.
#' @param add_constant Replacement for zero cells.
#' @return list(statistic, statistic_ij, p_ij, p_bonferroni, rs2_statistic, rs2_df, rs2_p).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Sec 6.2, eq (6.13).
#' @examples
#' Spmi(cbind(c(1, 0, 1, 0, 1, 0), c(0, 1, 1, 0, 0, 1)), cbind(c(1, 0, 1, 0, 1, 0)))$statistic
#' @export
Spmi <- function(W, Y, add_constant = 0.5) {
  W <- as.matrix(W)
  Y <- as.matrix(Y)
  n <- nrow(W)
  I <- ncol(W)
  J <- ncol(Y)
  xs <- matrix(0, I, J)
  for (i in 1:I) for (j in 1:J) {
    cells <- c(sum(W[, i] == 0 & Y[, j] == 0), sum(W[, i] == 0 & Y[, j] == 1), sum(W[, i] == 1 & Y[, j] == 0), sum(W[, i] == 1 & Y[, j] == 1))
    if (min(cells[1] + cells[2], cells[3] + cells[4], cells[1] + cells[3], cells[2] + cells[4]) == 0) {
      stop(sprintf("item pair (%d, %d) has a constant response", i - 1, j - 1), call. = FALSE)
    }
    cells[cells == 0] <- add_constant
    a <- cells[1]
    b <- cells[2]
    cc <- cells[3]
    d <- cells[4]
    xs[i, j] <- (a + b + cc + d) * (a * d - b * cc)^2 / ((a + b) * (cc + d) * (a + cc) * (b + d))
  }
  ps <- pchisq(xs, 1, lower.tail = FALSE)
  pr <- colMeans(W)
  pc <- colMeans(Y)
  idx <- expand.grid(j = 1:J, i = 1:I)[, 2:1]
  Fm <- sapply(seq_len(nrow(idx)), function(a) {
    i <- idx$i[a]
    j <- idx$j[a]
    W[, i] * Y[, j] - pr[i] * Y[, j] - pc[j] * W[, i]
  })
  S <- crossprod(Fm) / n - tcrossprod(colMeans(Fm))
  dvec <- pr[idx$i] * pc[idx$j] * (1 - pr[idx$i]) * (1 - pc[idx$j])
  DS <- S / dvec
  s2 <- sum(DS * t(DS))
  stat <- sum(xs)
  rs2 <- I * J * stat / s2
  df <- I^2 * J^2 / s2
  list(statistic = stat, statistic_ij = xs, p_ij = ps, p_bonferroni = min(1, I * J * min(ps)),
       p_ij_bonferroni = pmin(1, I * J * ps), rs2_statistic = rs2, rs2_df = df, rs2_p = pchisq(rs2, df, lower.tail = FALSE))
}
