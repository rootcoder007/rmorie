# SPDX-License-Identifier: AGPL-3.0-or-later

#' Percentile bootstrap interval from ordered bootstrap values
#'
#' (D*(l+1), D*(u)) with l = round(alpha B / 2) and u = B - l.
#'
#' @param values Bootstrap estimates.
#' @param alpha Level.
#' @return list(ci, l, u, p_value).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (7.15).
#' @examples
#' PBCI(1:1000)$ci
#' @export
PBCI <- function(values, alpha = 0.05) {
  v <- sort(as.numeric(values))
  B <- length(v)
  if (B < 2 || alpha <= 0 || alpha >= 1) stop("need at least 2 bootstrap values and 0 < alpha < 1", call. = FALSE)
  l <- round(alpha * B / 2)
  u <- B - l
  ph <- (sum(v < 0) + 0.5 * sum(v == 0)) / B
  list(ci = c(v[l + 1], v[u]), l = l, u = u, p_value = 2 * min(ph, 1 - ph))
}

#' Modified percentile bootstrap for two variances
#'
#' B = 599 rounds of n_m = min(n1, n2) draws per group (morie Philox streams 0
#' and 1); CI (D*(l), D*(u)) with (l, u) set by n_m.
#'
#' @param x,y Numeric vectors.
#' @param seed Seed of the Philox stream.
#' @return list(ci, estimate, l, u, boot, reject).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (7.39).
#' @examples
#' ComVar2(1:8, seq(1, 15, 2))$ci
#' @export
ComVar2 <- function(x, y, seed = 0) {
  if (length(x) < 2 || length(y) < 2) stop("need at least 2 observations per group", call. = FALSE)
  B <- 599
  nm <- min(length(x), length(y))
  ix <- floor(.morie_random_uniform(B * nm, seed = seed, stream = 0) * length(x)) + 1
  iy <- floor(.morie_random_uniform(B * nm, seed = seed, stream = 1) * length(y)) + 1
  d <- vapply(seq_len(B), function(b) {
    k <- (b - 1) * nm + seq_len(nm)
    stats::var(x[ix[k]]) - stats::var(y[iy[k]])
  }, numeric(1))
  d <- sort(d)
  lu <- if (nm < 40) c(7, 593) else if (nm < 80) c(8, 592) else if (nm < 180) c(11, 588) else if (nm < 250) c(14, 585) else c(15, 584)
  ci <- d[lu]
  list(ci = ci, estimate = stats::var(x) - stats::var(y), l = lu[1], u = lu[2], boot = d, reject = !(ci[1] <= 0 && 0 <= ci[2]))
}

#' Bootstrap-t all-pairs comparison of trimmed means
#'
#' T*max over pairs of |(bootstrap difference) - (observed difference)| /
#' sqrt(d*_j + d*_k); intervals diff +- T*max(u) sqrt(d_j + d_k).
#'
#' @param groups List of numeric vectors.
#' @param tr Trimming proportion.
#' @param alpha Level.
#' @param B Bootstrap rounds.
#' @param seed Seed of the Philox streams (stream j - 1 for group j).
#' @return list(crit, comparisons, tmax).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Sec 12.1.13.
#' @examples
#' LinconBT(list(1:8, 2:9, 5:12), B = 99)$crit
#' @export
LinconBT <- function(groups, tr = 0.2, alpha = 0.05, B = 599, seed = 0) {
  J <- length(groups)
  dj <- function(v) {
    h <- length(v) - 2 * floor(tr * length(v))
    (length(v) - 1) * .wil_winvar(v, tr) / (h * (h - 1))
  }
  if (J < 2 || any(vapply(groups, function(g) length(g) - 2 * floor(tr * length(g)), numeric(1)) < 2)) {
    stop("need at least two groups with 2+ observations left after trimming", call. = FALSE)
  }
  est <- vapply(groups, .wil_tmean, numeric(1), tr = tr)
  d <- vapply(groups, dj, numeric(1))
  idx <- lapply(seq_len(J), function(j) floor(.morie_random_uniform(B * length(groups[[j]]), seed = seed, stream = j - 1) * length(groups[[j]])) + 1)
  pairs <- utils::combn(J, 2)
  tmax <- vapply(seq_len(B), function(b) {
    bs <- lapply(seq_len(J), function(j) {
      n <- length(groups[[j]])
      groups[[j]][idx[[j]][(b - 1) * n + seq_len(n)]]
    })
    eb <- vapply(bs, .wil_tmean, numeric(1), tr = tr)
    db <- vapply(bs, dj, numeric(1))
    max(abs((eb[pairs[1, ]] - eb[pairs[2, ]]) - (est[pairs[1, ]] - est[pairs[2, ]])) / sqrt(db[pairs[1, ]] + db[pairs[2, ]]))
  }, numeric(1))
  tmax <- sort(tmax)
  crit <- tmax[round((1 - alpha) * B)]
  se <- sqrt(d[pairs[1, ]] + d[pairs[2, ]])
  diff <- est[pairs[1, ]] - est[pairs[2, ]]
  comps <- data.frame(j = pairs[1, ] - 1, k = pairs[2, ] - 1, diff = diff, lower = diff - crit * se, upper = diff + crit * se,
                      test = abs(diff) / se, reject = abs(diff) / se >= crit)
  list(crit = crit, comparisons = comps, tmax = tmax)
}

#' Friedman's test in F form
#'
#' F = (n - 1)(B - C)/(A - B) from within-row (mid)ranks.
#'
#' @param X Matrix, rows subjects, columns groups.
#' @return list(statistic, df1, df2, p_value, A, B, C).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (11.6).
#' @examples
#' FriedF(rbind(c(9, 7, 12), c(1, 10, 4), c(8, 2, 1), c(5, 6, 9)))$statistic
#' @export
FriedF <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X)
  J <- ncol(X)
  if (n < 2 || J < 2) stop("need an n x J table with n, J >= 2", call. = FALSE)
  R <- t(apply(X, 1, rank))
  A <- sum(R^2)
  Bv <- sum(colSums(R)^2) / n
  C <- n * J * (J + 1)^2 / 4
  if (A == Bv) {
    f <- Inf
    p <- 0
  } else {
    f <- (n - 1) * (Bv - C) / (A - Bv)
    p <- pf(f, J - 1, (n - 1) * (J - 1), lower.tail = FALSE)
  }
  list(statistic = f, df1 = J - 1, df2 = (n - 1) * (J - 1), p_value = p, A = A, B = Bv, C = C)
}

#' Outliers by robust Mahalanobis distance
#'
#' Flags rows whose distance from a robust center exceeds sqrt(qchisq(q, p));
#' center and scatter default to the minimum volume ellipsoid (Mvedet).
#'
#' @param X Numeric matrix.
#' @param center,scatter Optional robust center and scatter.
#' @param q Chi-squared quantile.
#' @return list(outliers (0-based), distance, crit).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (13.2).
#' @examples
#' OutMah(rbind(c(0, 0), c(1, 0), c(0, 1), c(9, 9)), center = c(0, 0), scatter = diag(2))$outliers
#' @export
OutMah <- function(X, center = NULL, scatter = NULL, q = 0.975) {
  X <- as.matrix(X)
  if (is.null(center) || is.null(scatter)) {
    est <- Mvedet(X)
    if (is.null(center)) center <- est$center
    if (is.null(scatter)) scatter <- est$cov
  }
  e <- sweep(X, 2, center)
  dist <- sqrt(pmax(0, rowSums((e %*% solve(scatter)) * e)))
  crit <- sqrt(qchisq(q, ncol(X)))
  list(outliers = which(dist > crit) - 1, distance = dist, crit = crit)
}

#' Kernel smoother for a binary outcome
#'
#' P(x) = sum w_j Y_j / sum w_j with w_i = I(|z_i - z| < h) exp(-(z_i - z)^2) on the
#' median/MADN standardized scale.
#'
#' @param x Predictor.
#' @param y Binary outcome (0/1).
#' @param pts Evaluation points (default x).
#' @param h Window half-width.
#' @return list(phat, pts).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (15.15).
#' @examples
#' LogRSM(1:6, c(0, 0, 1, 0, 1, 1), pts = 3.5)$phat
#' @export
LogRSM <- function(x, y, pts = x, h = 1.2) {
  if (length(x) != length(y) || length(x) < 2 || any(!y %in% c(0, 1))) stop("x and y must match and y must be 0/1", call. = FALSE)
  m <- stats::median(x)
  madn <- stats::median(abs(x - m)) / 0.6745
  if (madn <= 0) stop("MAD of x is zero", call. = FALSE)
  z <- (x - m) / madn
  phat <- vapply((pts - m) / madn, function(t) {
    w <- ifelse(abs(z - t) < h, exp(-(z - t)^2), 0)
    if (sum(w) > 0) sum(w * y) / sum(w) else NaN
  }, numeric(1))
  list(phat = phat, pts = pts)
}
