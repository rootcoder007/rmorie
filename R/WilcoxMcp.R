# SPDX-License-Identifier: AGPL-3.0-or-later

#' Studentized range distribution function
#'
#' @param q Quantile.
#' @param nmeans Number of means.
#' @param df Degrees of freedom.
#' @param nranges Number of ranges.
#' @param lower_tail Lower tail.
#' @return list(p).
#' @references Copenhaver, M. D. & Holland, B. S. (1988). JSCS 30, 1-15.
#' @examples
#' PTukey(3.5, 3, 20)$p
#' @export
PTukey <- function(q, nmeans, df, nranges = 1, lower_tail = TRUE) {
  list(p = stats::ptukey(q, nmeans, df, nranges, lower.tail = lower_tail))
}

#' Studentized range quantile
#'
#' @param p Probability.
#' @param nmeans Number of means.
#' @param df Degrees of freedom.
#' @param nranges Number of ranges.
#' @return list(q).
#' @references Copenhaver, M. D. & Holland, B. S. (1988). JSCS 30, 1-15.
#' @examples
#' QTukey(0.95, 3, 20)$q
#' @export
QTukey <- function(p, nmeans, df, nranges = 1) {
  if (p <= 0 || p >= 1 || df < 2 || nmeans < 2 || nranges < 1) stop("need 0 < p < 1, df >= 2, nmeans >= 2 and nranges >= 1", call. = FALSE)
  list(q = stats::qtukey(p, nmeans, df, nranges))
}

#' Tukey-Kramer intervals for all pairwise differences
#'
#' (mean_j - mean_k) +- q sqrt((MSWG/2)(1/n_j + 1/n_k)).
#'
#' @param groups List of numeric vectors.
#' @param alpha Level.
#' @return list(q, mswg, df, comparisons).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (12.4).
#' @examples
#' TukeyKramer(list(1:3, 2:4, 6:8))$q
#' @export
TukeyKramer <- function(groups, alpha = 0.05) {
  J <- length(groups)
  n <- lengths(groups)
  df <- sum(n) - J
  if (J < 2 || df < 2) stop("need at least two groups and N - J >= 2", call. = FALSE)
  m <- vapply(groups, mean, numeric(1))
  mswg <- sum(vapply(groups, function(g) sum((g - mean(g))^2), numeric(1))) / df
  q <- stats::qtukey(1 - alpha, J, df)
  pr <- utils::combn(J, 2)
  se <- sqrt(mswg / 2 * (1 / n[pr[1, ]] + 1 / n[pr[2, ]]))
  diff <- m[pr[1, ]] - m[pr[2, ]]
  list(q = q, mswg = mswg, df = df,
       comparisons = data.frame(j = pr[1, ] - 1, k = pr[2, ] - 1, diff = diff, lower = diff - q * se, upper = diff + q * se,
                                p_adj = stats::ptukey(abs(diff) / se, J, df, lower.tail = FALSE)))
}

#' Scheffe interval for a contrast
#'
#' Psi_hat +- sqrt((L - 1) f MSWG sum c^2 / n).
#'
#' @param groups List of numeric vectors.
#' @param contrast Contrast coefficients.
#' @param alpha Level.
#' @return list(estimate, S, ci, mswg, df).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eqs (12.7)-(12.9).
#' @examples
#' ScheffeCI(list(1:3, 2:4, 6:8), c(1, -1, 0))$ci
#' @export
ScheffeCI <- function(groups, contrast, alpha = 0.05) {
  L <- length(groups)
  n <- lengths(groups)
  df <- sum(n) - L
  if (L < 2 || length(contrast) != L || df < 1) stop("need L >= 2 groups, one coefficient per group and N > L", call. = FALSE)
  m <- vapply(groups, mean, numeric(1))
  mswg <- sum(vapply(groups, function(g) sum((g - mean(g))^2), numeric(1))) / df
  est <- sum(contrast * m)
  S <- sqrt((L - 1) * qf(1 - alpha, L - 1, df) * mswg * sum(contrast^2 / n))
  list(estimate = est, S = S, ci = est + c(-1, 1) * S, mswg = mswg, df = df)
}

#' Johansen test of a linear hypothesis about trimmed means
#'
#' Q = Xbar'C'(CVC')^{-1}C Xbar with V = diag of Yuen squared standard errors and
#' Johansen's adjusted chi-squared critical value.
#'
#' @param groups List of numeric vectors.
#' @param C Contrast matrix (k x J).
#' @param tr Trimming proportion.
#' @param alpha Level.
#' @return list(statistic, crit, reject, p_value, A).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eqs (10.3)-(10.4).
#' @examples
#' JohanQ(list(1:5, c(1:4, 9)), matrix(c(1, -1), 1))$statistic
#' @export
JohanQ <- function(groups, C, tr = 0.2, alpha = 0.05) {
  C <- as.matrix(C)
  J <- length(groups)
  if (J < 2 || ncol(C) != J) stop("C must have one column per group", call. = FALSE)
  xb <- vapply(groups, .wil_tmean, numeric(1), tr = tr)
  h <- vapply(groups, function(g) length(g) - 2 * floor(tr * length(g)), numeric(1))
  if (min(h) < 2) stop("too few observations left after trimming", call. = FALSE)
  v <- vapply(seq_len(J), function(j) (length(groups[[j]]) - 1) * .wil_winvar(groups[[j]], tr) / (h[j] * (h[j] - 1)), numeric(1))
  V <- diag(v, J, J)
  inv <- solve(C %*% V %*% t(C))
  cx <- C %*% xb
  Q <- drop(t(cx) %*% inv %*% cx)
  R <- V %*% t(C) %*% inv %*% C
  A <- sum(diag(R)^2 / (h - 1))
  k <- nrow(C)
  crit <- function(al) {
    c0 <- qchisq(1 - al, k)
    c0 + c0 / (2 * k) * A * (1 + 3 * c0 / (k + 2))
  }
  p <- if (crit(1 - 1e-12) >= Q) 1 else if (crit(1e-12) <= Q) 0 else stats::uniroot(function(al) crit(al) - Q, c(1e-12, 1 - 1e-12), tol = 1e-14)$root
  list(statistic = Q, crit = crit(alpha), reject = Q > crit(alpha), p_value = p, A = A)
}
