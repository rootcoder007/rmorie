# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Nonparametric mirrors for the Gibbons shelf (batch 5) -- the 137
# modules that carried a generated template body and now compute the
# equation they cite.  Source: Gibbons, J. D. & Chakraborti, S. (2011),
# Nonparametric Statistical Inference, 5th ed., CRC Press.  Page
# numbers in the roxygen blocks are BOOK pages (pdf page - 21).
#
# These are internal mirrors used to check the Python arm.  They are
# deliberately NOT exported: reach them as morie:::Name.  See the
# 2026-08-03 NAMESPACE note in SHELF_LEDGER.txt.

#' EDF count is binomial -- Gibbons Theorem 2.3.1 (book p. 33)
#' @noRd
Edfbinom <- function(n, fx, i = NULL) {
  n <- as.integer(n); fx <- as.numeric(fx)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (fx < 0 || fx > 1) stop("fx must lie in [0, 1].", call. = FALSE)
  pmf <- NaN; cdf <- NaN
  if (!is.null(i)) {
    i <- as.integer(i)
    if (i < 0L || i > n) stop("i must lie in 0..n.", call. = FALSE)
    pmf <- choose(n, i) * fx^i * (1 - fx)^(n - i)
    cdf <- sum(choose(n, 0:i) * fx^(0:i) * (1 - fx)^(n - 0:i))
  }
  list(mean = n * fx, var = n * fx * (1 - fx), pmf = pmf, cdf = cdf,
       n = n, fx = fx)
}

#' CDF of the r-th order statistic -- Theorem 2.4.1, eq. (2.4.1), p. 37
#' @noRd
Ostatcdf <- function(t, r, n, cdf) {
  r <- as.integer(r); n <- as.integer(n)
  if (r < 1L || r > n) stop("need 1 <= r <= n.", call. = FALSE)
  p <- if (is.function(cdf)) as.numeric(cdf(t)) else as.numeric(cdf)
  if (p < 0 || p > 1) stop("F_X(t) must lie in [0, 1].", call. = FALSE)
  ii <- r:n
  val <- sum(choose(n, ii) * p^ii * (1 - p)^(n - ii))
  list(cdf = val, sf = 1 - val, fx = p, r = r, n = n, t = as.numeric(t))
}

#' PDF of the r-th order statistic -- Theorem 2.4.2, eq. (2.4.2), p. 37
#' @noRd
Ostatpdf <- function(x, r, n, cdf, pdf) {
  r <- as.integer(r); n <- as.integer(n)
  if (r < 1L || r > n) stop("need 1 <= r <= n.", call. = FALSE)
  fx <- if (is.function(cdf)) as.numeric(cdf(x)) else as.numeric(cdf)
  dx <- if (is.function(pdf)) as.numeric(pdf(x)) else as.numeric(pdf)
  coef <- factorial(n) / (factorial(r - 1) * factorial(n - r))
  list(pdf = coef * fx^(r - 1) * (1 - fx)^(n - r) * dx, coef = coef,
       fx = fx, dx = dx, r = r, n = n)
}

#' Uniform order statistic is Beta(r, n-r+1) -- Theorem 2.4.3, p. 38
#' @noRd
Ostatbeta <- function(u, r, n) {
  r <- as.integer(r); n <- as.integer(n); u <- as.numeric(u)
  if (r < 1L || r > n) stop("need 1 <= r <= n.", call. = FALSE)
  if (u < 0 || u > 1) stop("u must lie in [0, 1].", call. = FALSE)
  a <- as.numeric(r); b <- as.numeric(n - r + 1)
  coef <- factorial(n) / (factorial(r - 1) * factorial(n - r))
  list(pdf = coef * u^(r - 1) * (1 - u)^(n - r),
       cdf = stats::pbeta(u, a, b),
       mean = a / (a + b), var = a * b / ((a + b)^2 * (a + b + 1)),
       alpha = a, beta = b, r = r, n = n)
}

#' Asymptotic normality of the sample quantile -- Theorem 2.10.1, p. 60
#' @noRd
Ostatasymp <- function(p, n, xp, fxp) {
  p <- as.numeric(p); n <- as.integer(n); fxp <- as.numeric(fxp)
  if (p <= 0 || p >= 1) stop("p must lie strictly inside (0, 1).", call. = FALSE)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (fxp <= 0) stop("fxp must be strictly positive.", call. = FALSE)
  v <- p * (1 - p) / (n * fxp * fxp)
  list(mean = as.numeric(xp), var = v, se = sqrt(v), p = p, n = n)
}

#' Empirical distribution function -- eq. (2.3.1), p. 32
#' @noRd
Edfstep <- function(x, t) {
  xs <- sort(as.numeric(x)); n <- length(xs)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  ts <- as.numeric(t)
  counts <- vapply(ts, function(v) sum(xs <= v), 0)
  list(edf = counts / n, count = counts, n = n, sorted = xs)
}

#' Moments of a uniform order statistic -- Sec. 2.4, p. 38
#' @noRd
Ostatmom <- function(r, n, k = 1) {
  r <- as.integer(r); n <- as.integer(n); k <- as.integer(k)
  if (r < 1L || r > n) stop("need 1 <= r <= n.", call. = FALSE)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  j <- 0:(k - 1)
  list(moment = prod((r + j) / (n + 1 + j)), mean = r / (n + 1),
       var = r * (n - r + 1) / ((n + 1)^2 * (n + 2)), r = r, n = n, k = k)
}

#' Covariance of two uniform order statistics -- Sec. 2.4, p. 38
#' @noRd
Ostatcov <- function(r, s, n) {
  r <- as.integer(r); s <- as.integer(s); n <- as.integer(n)
  if (r < 1L || r > s || s > n) stop("need 1 <= r <= s <= n.", call. = FALSE)
  den <- (n + 1)^2 * (n + 2)
  cv <- r * (n - s + 1) / den
  vr <- r * (n - r + 1) / den
  vs <- s * (n - s + 1) / den
  list(cov = cv, corr = cv / sqrt(vr * vs), var_r = vr, var_s = vs,
       r = r, s = s, n = n)
}

#' Joint density of X_(r), X_(s) -- Sec. 2.5, p. 39
#' @noRd
Ostatjoint <- function(x, y, r, s, n, cdf, pdf) {
  r <- as.integer(r); s <- as.integer(s); n <- as.integer(n)
  if (r < 1L || r >= s || s > n) stop("need 1 <= r < s <= n.", call. = FALSE)
  x <- as.numeric(x); y <- as.numeric(y)
  coef <- factorial(n) /
    (factorial(r - 1) * factorial(s - r - 1) * factorial(n - s))
  if (x >= y) {
    return(list(pdf = 0, coef = coef, fx = NaN, fy = NaN, r = r, s = s, n = n))
  }
  fx <- if (is.function(cdf)) as.numeric(cdf(x)) else as.numeric(cdf)
  fy <- if (is.function(cdf)) as.numeric(cdf(y)) else as.numeric(cdf)
  dx <- if (is.function(pdf)) as.numeric(pdf(x)) else as.numeric(pdf)
  dy <- if (is.function(pdf)) as.numeric(pdf(y)) else as.numeric(pdf)
  val <- coef * fx^(r - 1) * (fy - fx)^(s - r - 1) * (1 - fy)^(n - s) * dx * dy
  list(pdf = val, coef = coef, fx = fx, fy = fy, r = r, s = s, n = n)
}

#' Joint density of all n order statistics -- Sec. 2.2, p. 31
#' @noRd
Ostatjall <- function(x, pdf) {
  xs <- as.numeric(x); n <- length(xs)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  ordered <- all(diff(xs) > 0)
  pr <- prod(vapply(xs, function(v) as.numeric(pdf(v)), 0))
  coef <- factorial(n)
  list(pdf = if (ordered) coef * pr else 0, coef = coef, prod = pr,
       ordered = as.integer(ordered), n = n)
}

#' Sample quantile as an order statistic -- Sec. 2.6, p. 42
#' @noRd
Sampquant <- function(x, p) {
  xs <- sort(as.numeric(x)); n <- length(xs); p <- as.numeric(p)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  if (p <= 0 || p >= 1) stop("p must lie strictly inside (0, 1).", call. = FALSE)
  r <- as.integer(floor(n * p)) + 1L
  if (r > n) r <- n
  list(estimate = xs[r], r = r, n = n, p = p, u_mean = r / (n + 1),
       u_var = r * (n - r + 1) / ((n + 1)^2 * (n + 2)))
}

#' Placement / exceedance null law -- Problem 2.28(c), p. 70
#' @noRd
Exceed <- function(i, m, n, j = NULL) {
  i <- as.integer(i); m <- as.integer(m); n <- as.integer(n)
  if (i < 1L || i > n) stop("need 1 <= i <= n.", call. = FALSE)
  if (m < 1L) stop("m must be at least 1.", call. = FALSE)
  den <- choose(m + n, n); k <- 0:m
  pmf <- choose(m + n - i - k, m - k) * choose(i + k - 1, k) / den
  mu <- sum(k * pmf); e2 <- sum(k * k * pmf)
  pmf_j <- NaN; cdf_j <- NaN
  if (!is.null(j)) {
    j <- as.integer(j)
    if (j < 0L || j > m) stop("j must lie in 0..m.", call. = FALSE)
    pmf_j <- pmf[j + 1L]; cdf_j <- sum(pmf[1:(j + 1L)])
  }
  list(pmf = pmf, pmf_j = pmf_j, cdf_j = cdf_j, mean = mu, var = e2 - mu^2,
       i = i, m = m, n = n)
}

#' Placements of Y among the X order statistics -- Sec. 2.11, p. 65
#' @noRd
Placement <- function(x, y) {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y))
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  plc <- vapply(ys, function(v) sum(xs <= v), 0)
  list(placements = plc, ranks = plc + seq_len(n),
       blocks = c(plc[1], diff(plc)), total = sum(plc), m = m, n = n)
}

#' Distribution-free quantile confidence interval -- Sec. 5.2, p. 158
#' @noRd
Quantci <- function(x, p, r, s) {
  xs <- sort(as.numeric(x)); n <- length(xs)
  r <- as.integer(r); s <- as.integer(s); p <- as.numeric(p)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (r < 1L || r >= s || s > n) stop("need 1 <= r < s <= n.", call. = FALSE)
  if (p <= 0 || p >= 1) stop("p must lie strictly inside (0, 1).", call. = FALSE)
  ii <- r:(s - 1L)
  cov <- sum(choose(n, ii) * p^ii * (1 - p)^(n - ii))
  list(lower = xs[r], upper = xs[s], coverage = cov, alpha = 1 - cov,
       r = r, s = s, n = n, p = p)
}

#' Distribution-free quantile test -- Sec. 5.3, p. 163
#' @noRd
Quanttest <- function(x, q0, p = 0.5, alternative = "two-sided") {
  xs <- as.numeric(x); n <- length(xs); p <- as.numeric(p)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  if (p <= 0 || p >= 1) stop("p must lie strictly inside (0, 1).", call. = FALSE)
  k <- sum(xs <= as.numeric(q0))
  pm <- function(i) choose(n, i) * p^i * (1 - p)^(n - i)
  lower <- sum(pm(0:k)); upper <- sum(pm(k:n))
  pv <- switch(alternative,
    "less" = lower, "greater" = upper,
    "two-sided" = min(1, 2 * min(lower, upper)),
    stop("alternative must be two-sided, less or greater.", call. = FALSE))
  list(statistic = k, p_value = pv, n = n, p = p, mean = n * p,
       var = n * p * (1 - p), alternative = alternative)
}

#' Sign-test statistic K -- Sec. 5.4, eq. (5.4.1), p. 168
#' @noRd
Signk <- function(x, m0 = 0) {
  xs <- as.numeric(x) - as.numeric(m0)
  n_raw <- length(xs)
  if (n_raw < 1L) stop("x must be non-empty.", call. = FALSE)
  nz <- sum(xs == 0); n <- n_raw - nz; k <- sum(xs > 0)
  list(statistic = k, n = n, nzero = nz, mean = n / 2, var = n / 4,
       n_raw = n_raw)
}

#' Exact sign-test p-value -- eq. (5.4.3), p. 169
#' @noRd
Signp <- function(k, n, alternative = "two-sided") {
  k <- as.integer(k); n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (k < 0L || k > n) stop("k must lie in 0..n.", call. = FALSE)
  half <- 0.5^n
  lower <- sum(choose(n, 0:k)) * half
  upper <- sum(choose(n, k:n)) * half
  pv <- switch(alternative,
    "greater" = upper, "less" = lower,
    "two-sided" = min(1, 2 * min(lower, upper)),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(p_value = pv, p_lower = lower, p_upper = upper, statistic = k,
       n = n, alternative = alternative)
}

#' Sign-test normal approximation -- eq. (5.4.7), p. 174
#' @noRd
Signz <- function(k, n, alternative = "two-sided", correct = TRUE) {
  k <- as.integer(k); n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (k < 0L || k > n) stop("k must lie in 0..n.", call. = FALSE)
  mean <- n / 2; sd <- sqrt(n / 4); d <- k - mean
  if (correct) {
    if (d > 0) d <- d - 0.5 else if (d < 0) d <- d + 0.5
  }
  z <- d / sd
  pv <- switch(alternative,
    "greater" = 1 - stats::pnorm(z), "less" = stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(z = z, p_value = min(1, pv), statistic = k, n = n, mean = mean,
       var = n / 4, alternative = alternative)
}

#' Zero differences in the sign test -- Sec. 5.4.8, p. 180
#' @noRd
Signzero <- function(x, m0 = 0, method = "discard") {
  xs <- as.numeric(x) - as.numeric(m0)
  n_raw <- length(xs)
  if (n_raw < 1L) stop("x must be non-empty.", call. = FALSE)
  nz <- sum(xs == 0); kpos <- sum(xs > 0); kneg <- n_raw - nz - kpos
  if (method == "discard") {
    k <- kpos; n <- n_raw - nz
  } else if (method == "half") {
    k <- kpos + nz / 2; n <- n_raw
  } else if (method == "conservative") {
    if (kpos < kneg) { k <- kpos + nz } else { k <- kpos }
    n <- n_raw
  } else {
    stop("method must be discard, half or conservative.", call. = FALSE)
  }
  list(statistic = as.numeric(k), n = as.integer(n), nzero = nz,
       k_raw = kpos, n_raw = n_raw)
}

#' Power of the sign test -- eq. (5.4.8), Table 5.4.1, p. 174
#' @noRd
Signpow <- function(n, theta, alpha = 0.05, exact = TRUE) {
  n <- as.integer(n); theta <- as.numeric(theta); alpha <- as.numeric(alpha)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (theta <= 0 || theta >= 1)
    stop("theta must lie strictly inside (0, 1).", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  za <- stats::qnorm(1 - alpha)
  approx <- 1 - stats::pnorm((n * (0.5 - theta) + 0.5 * sqrt(n) * za) /
                             sqrt(n * theta * (1 - theta)))
  ka <- n; aex <- NaN; pex <- NaN
  if (exact) {
    half <- 0.5^n
    for (cc in 0:n) {
      tail <- sum(choose(n, cc:n)) * half
      if (tail <= alpha) { ka <- cc; aex <- tail; break }
    }
    ii <- ka:n
    pex <- sum(choose(n, ii) * theta^ii * (1 - theta)^(n - ii))
  }
  list(power = approx, power_exact = pex, k_alpha = ka, alpha_exact = aex,
       n = n, theta = theta)
}

#' Simulated sign-test power over supplied samples -- Sec. 5.4.5, p. 175
#' @noRd
Signsimpow <- function(samples, m0, kcrit) {
  rows <- if (is.matrix(samples)) split(samples, row(samples)) else samples
  nsim <- length(rows)
  if (nsim < 1L) stop("samples must be non-empty.", call. = FALSE)
  kcrit <- as.integer(kcrit)
  ks <- vapply(rows, function(r) sum(as.numeric(r) > as.numeric(m0)), 0)
  rej <- sum(ks >= kcrit)
  list(power = rej / nsim, rejections = rej, nsim = nsim,
       kmean = sum(ks) / nsim, kcrit = kcrit)
}

#' Sign-test sample size -- eq. (5.4.9), p. 179
#' @noRd
Signn <- function(theta, alpha = 0.05, beta = 0.10) {
  theta <- as.numeric(theta); alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  if (theta == 0.5) stop("theta must differ from 0.5.", call. = FALSE)
  if (theta <= 0 || theta >= 1)
    stop("theta must lie strictly inside (0, 1).", call. = FALSE)
  za <- stats::qnorm(1 - alpha); zb <- stats::qnorm(1 - beta)
  root <- (sqrt(theta * (1 - theta)) * zb + 0.5 * za) / (0.5 - theta)
  nraw <- root * root
  list(n = as.integer(ceiling(nraw)), n_raw = nraw, root_n = abs(root),
       z_alpha = za, z_beta = zb, theta = theta)
}

#' Two-sided sign-test sample size -- eq. (5.4.9) with alpha/2, p. 179
#' @noRd
Signnasy <- function(theta, alpha = 0.05, beta = 0.10) {
  theta <- as.numeric(theta); alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  if (theta == 0.5) stop("theta must differ from 0.5.", call. = FALSE)
  if (theta <= 0 || theta >= 1)
    stop("theta must lie strictly inside (0, 1).", call. = FALSE)
  za <- stats::qnorm(1 - alpha / 2); zb <- stats::qnorm(1 - beta)
  root <- (sqrt(theta * (1 - theta)) * zb + 0.5 * za) / (0.5 - theta)
  nraw <- root * root
  list(n = as.integer(ceiling(nraw)), n_raw = nraw, root_n = abs(root),
       z_alpha = za, z_beta = zb, theta = theta)
}

#' Median CI from sign-test inversion -- eq. (5.4.11), p. 179
#' @noRd
Signmedci <- function(x, alpha = 0.05) {
  xs <- sort(as.numeric(x)); n <- length(xs); alpha <- as.numeric(alpha)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  half <- 0.5^n; r <- 0L; tail <- 0
  for (cand in 1:n) {
    t <- sum(choose(n, 0:(cand - 1))) * half
    if (t <= alpha / 2) { r <- as.integer(cand); tail <- t } else break
  }
  if (r == 0L) {
    return(list(lower = NaN, upper = NaN, r = 0L, s = 0L, coverage = NaN,
                tail = 0, n = n))
  }
  s <- n - r + 1L
  list(lower = xs[r], upper = xs[s], r = r, s = s,
       coverage = 1 - 2 * tail, tail = tail, n = n)
}

#' Midranks of |d| with signs -- Sec. 5.5, p. 189
#' @noRd
Absrank <- function(d) {
  ds <- as.numeric(d); n <- length(ds)
  if (n < 1L) stop("d must be non-empty.", call. = FALSE)
  a <- abs(ds)
  ranks <- rank(a, ties.method = "average")
  tb <- table(a); ties <- as.numeric(tb[tb > 1])
  signs <- sign(ds)
  list(ranks = ranks, signs = signs, signed = signs * ranks,
       ties = ties, n = n)
}

#' Wilcoxon signed-rank T+ -- Sec. 5.7, eqs. (5.7.1)/(5.7.9), p. 195
#' @noRd
Wsr <- function(x, m0 = 0) {
  ds <- as.numeric(x) - as.numeric(m0)
  nzero <- sum(ds == 0); ds <- ds[ds != 0]; n <- length(ds)
  if (n < 1L) stop("no non-zero differences.", call. = FALSE)
  a <- abs(ds)
  ranks <- rank(a, ties.method = "average")
  tb <- as.numeric(table(a)); tb <- tb[tb > 1]
  corr <- if (length(tb)) sum(tb * (tb^2 - 1)) else 0
  tplus <- sum(ranks[ds > 0])
  mean <- n * (n + 1) / 4
  var <- n * (n + 1) * (2 * n + 1) / 24 - corr / 48
  z <- if (var > 0) (tplus - mean) / sqrt(var) else NaN
  pv <- if (var > 0) 2 * (1 - stats::pnorm(abs(z))) else NaN
  list(statistic = tplus, tminus = n * (n + 1) / 2 - tplus, n = n,
       nzero = nzero, mean = mean, var = var, z = z, p_value = min(1, pv))
}

#' Null moments of T+ -- eq. (5.7.2), p. 197
#' @noRd
Wsrmom <- function(n) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  mean <- n * (n + 1) / 4
  var <- n * (n + 1) * (2 * n + 1) / 24
  list(mean = mean, var = var, sd = sqrt(var), total = n * (n + 1) / 2,
       skew = 0, n = n)
}

#' Tie-corrected Var[T+] -- eqs. (5.7.10)-(5.7.11), p. 203
#' @noRd
Wsrties <- function(d, m0 = 0) {
  ds <- as.numeric(d) - as.numeric(m0)
  nzero <- sum(ds == 0); a <- sort(abs(ds[ds != 0])); n <- length(a)
  if (n < 1L) stop("no non-zero differences.", call. = FALSE)
  tb <- as.numeric(table(a)); ties <- tb[tb > 1]
  corr <- if (length(ties)) sum(ties * (ties^2 - 1)) / 48 else 0
  v0 <- n * (n + 1) * (2 * n + 1) / 24
  list(var = v0 - corr, var_uncorrected = v0, correction = corr, n = n,
       nzero = nzero, ties = ties)
}

#' Signed-rank normal approximation -- eq. (5.7.9), p. 202
#' @noRd
Wsrz <- function(tplus, n, alternative = "two-sided", correct = FALSE) {
  n <- as.integer(n); tplus <- as.numeric(tplus)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  mean <- n * (n + 1) / 4
  var <- n * (n + 1) * (2 * n + 1) / 24
  d <- tplus - mean
  if (correct) {
    if (d > 0) d <- d - 0.5 else if (d < 0) d <- d + 0.5
  }
  z <- d / sqrt(var)
  pv <- switch(alternative,
    "greater" = 1 - stats::pnorm(z), "less" = stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(z = z, p_value = min(1, pv), mean = mean, var = var,
       statistic = tplus, n = n, alternative = alternative)
}

#' Signed-rank power -- eqs. (5.7.13)-(5.7.14), p. 205
#' @noRd
Wsrpow <- function(n, p1, p2, alpha = 0.05) {
  n <- as.integer(n); p1 <- as.numeric(p1); p2 <- as.numeric(p2)
  alpha <- as.numeric(alpha)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  shift <- n * (p1 - 0.5) + n * (n - 1) * (p2 - 0.5) / 2
  sd0 <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  za <- stats::qnorm(1 - alpha)
  zb <- shift / sd0 - za
  list(power = stats::pnorm(zb), z_beta = zb, shift = shift, sd0 = sd0,
       n = n, p1 = p1, p2 = p2)
}

#' Simulated signed-rank power over supplied samples -- Sec. 5.7.3, p. 204
#' @noRd
Wsrsimpow <- function(samples, m0, tcrit) {
  rows <- if (is.matrix(samples)) split(samples, row(samples)) else samples
  nsim <- length(rows)
  if (nsim < 1L) stop("samples must be non-empty.", call. = FALSE)
  tcrit <- as.numeric(tcrit)
  ts <- vapply(rows, function(row) {
    ds <- as.numeric(row) - as.numeric(m0)
    ds <- ds[ds != 0]
    if (!length(ds)) return(0)
    sum(rank(abs(ds), ties.method = "average")[ds > 0])
  }, 0)
  rej <- sum(ts >= tcrit)
  list(power = rej / nsim, rejections = rej, nsim = nsim,
       tmean = sum(ts) / nsim, tcrit = tcrit)
}

#' Signed-rank sample size -- eq. (5.7.15), p. 206
#' @noRd
Wsrn <- function(p2, alpha = 0.05, beta = 0.05, twosided = FALSE) {
  p2 <- as.numeric(p2); alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  if (p2 == 0.5) stop("p2 must differ from 0.5.", call. = FALSE)
  if (p2 <= 0 || p2 >= 1)
    stop("p2 must lie strictly inside (0, 1).", call. = FALSE)
  a <- if (twosided) alpha / 2 else alpha
  za <- stats::qnorm(1 - a); zb <- stats::qnorm(1 - beta)
  nraw <- (za + zb)^2 / (3 * (p2 - 0.5)^2)
  list(n = as.integer(ceiling(nraw)), n_raw = nraw, z_alpha = za,
       z_beta = zb, p2 = p2)
}

#' Exact null pmf of T+ (subset-sum counting)
#' @noRd
.gbWsrNull <- function(n) {
  total <- n * (n + 1) / 2
  counts <- numeric(total + 1); counts[1] <- 1
  for (r in 1:n) {
    for (t in seq(total, r, by = -1)) counts[t + 1] <- counts[t + 1] + counts[t - r + 1]
  }
  counts / 2^n
}

#' Walsh-average confidence interval -- Sec. 5.7.5, pp. 207-209
#' @noRd
Wsrci <- function(x, tcrit) {
  xs <- as.numeric(x); n <- length(xs); tcrit <- as.integer(tcrit)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (tcrit < 0L) stop("tcrit must be non-negative.", call. = FALSE)
  walsh <- sort(as.numeric(outer(xs, xs, "+")[!lower.tri(matrix(0, n, n))]) / 2)
  nw <- length(walsh)
  if (tcrit + 1L > nw) stop("tcrit too large for this sample size.", call. = FALSE)
  pmf <- .gbWsrNull(n)
  tail <- sum(pmf[1:(tcrit + 1L)])
  list(lower = walsh[tcrit + 1L], upper = walsh[nw - tcrit],
       coverage = 1 - 2 * tail, tail = tail, nwalsh = nw, n = n,
       tcrit = tcrit, estimate = stats::median(walsh))
}

#' Signed-rank test of symmetry -- Sec. 5.7.7, p. 211
#' @noRd
Wsrsym <- function(x, centre = 0) {
  ds <- as.numeric(x) - as.numeric(centre); ds <- ds[ds != 0]
  n <- length(ds)
  if (n < 2L) stop("need at least 2 non-zero differences.", call. = FALSE)
  ranks <- rank(abs(ds), ties.method = "average")
  tplus <- sum(ranks[ds > 0])
  mean <- n * (n + 1) / 4
  var <- n * (n + 1) * (2 * n + 1) / 24
  z <- (tplus - mean) / sqrt(var)
  list(statistic = tplus, z = z, p_value = 2 * (1 - stats::pnorm(abs(z))),
       mean = mean, var = var,
       skewdir = if (tplus > mean) 1L else if (tplus < mean) -1L else 0L,
       n = n)
}

#' Hodges-Lehmann estimator and Walsh counting identity -- pp. 209-210
#' @noRd
Hlwsrlink <- function(x, m0 = 0) {
  xs <- as.numeric(x); n <- length(xs); m0 <- as.numeric(m0)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  walsh <- sort(as.numeric(outer(xs, xs, "+")[!lower.tri(matrix(0, n, n))]) / 2)
  nw <- length(walsh)
  below <- sum(walsh < m0); equal <- sum(walsh == m0)
  above <- nw - below - equal
  list(estimate = stats::median(walsh), tplus = above + equal / 2,
       tminus = below + equal / 2, nbelow = below, nequal = equal,
       nabove = above, nwalsh = nw, n = n)
}

#' Total runs asymptotic normality -- eq. (3.2.9), p. 82
#' @noRd
Runsz <- function(r, n1, n2, correct = FALSE) {
  n1 <- as.integer(n1); n2 <- as.integer(n2); r <- as.numeric(r)
  if (n1 < 1L || n2 < 1L) stop("n1 and n2 must be at least 1.", call. = FALSE)
  n <- n1 + n2; lam <- n1 / n
  mean <- 2 * n * lam * (1 - lam)
  sd <- 2 * sqrt(n) * lam * (1 - lam)
  me <- 2 * n1 * n2 / n + 1
  ve <- 2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1))
  d <- r - mean; de <- r - me
  if (correct) {
    if (d > 0) d <- d - 0.5 else if (d < 0) d <- d + 0.5
    if (de > 0) de <- de - 0.5 else if (de < 0) de <- de + 0.5
  }
  z <- d / sd
  list(z = z, z_exact = de / sqrt(ve),
       p_value = 2 * (1 - stats::pnorm(abs(z))), mean = mean, var = sd^2,
       mean_exact = me, var_exact = ve, lam = lam, n = n)
}

#' Runs up-and-down moments -- Sec. 3.4, p. 93 (Levene 1952)
#' @noRd
Runsudvar <- function(n, r = NULL, alpha = 0.05) {
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  mean <- (2 * n - 1) / 3; var <- (16 * n - 29) / 90; sd <- sqrt(var)
  zl <- NaN; zr <- NaN; pv <- NaN
  if (!is.null(r)) {
    r <- as.numeric(r)
    zl <- (r + 0.5 - mean) / sd
    zr <- (r - 0.5 - mean) / sd
    pv <- min(1, 2 * min(stats::pnorm(zl), 1 - stats::pnorm(zr)))
  }
  list(mean = mean, var = var, sd = sd, z_left = zl, z_right = zr,
       p_value = pv, zcrit = stats::qnorm(1 - as.numeric(alpha) / 2), n = n)
}

#' RVN null moments -- Sec. 3.5, p. 95 (Bartels 1982)
#' @noRd
Rvnmom <- function(n) {
  n <- as.integer(n)
  if (n < 3L) stop("n must be at least 3.", call. = FALSE)
  var <- 4 * (n - 2) * (5 * n^2 - 2 * n - 9) / (5 * n * (n + 1) * (n - 1)^2)
  list(mean = 2, var = var, var_approx = 20 / (5 * n + 7), sd = sqrt(var),
       denom = n * (n^2 - 1) / 12, n = n)
}

#' Rank von Neumann test -- eqs. (3.5.1)-(3.5.2), p. 95
#' @noRd
Rvntest <- function(x, alternative = "two-sided") {
  xs <- as.numeric(x); n <- length(xs)
  if (n < 3L) stop("need at least 3 observations.", call. = FALSE)
  ranks <- rank(xs, ties.method = "average")
  nm <- sum((ranks[-n] - ranks[-1])^2)
  den <- sum((ranks - (n + 1) / 2)^2)
  rvn <- nm / den
  var <- 4 * (n - 2) * (5 * n^2 - 2 * n - 9) / (5 * n * (n + 1) * (n - 1)^2)
  z <- (rvn - 2) / sqrt(var)
  pv <- switch(alternative,
    "less" = stats::pnorm(z), "greater" = 1 - stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, less or greater.", call. = FALSE))
  list(statistic = rvn, nm = nm, denom = den, z = z, p_value = min(1, pv),
       mean = 2, var = var, n = n)
}

#' Exact null distribution of R -- Theorem 3.2.2, eq. (3.2.3), p. 79
#' @noRd
.gbRunsPmf <- function(n1, n2) {
  n <- n1 + n2; den <- choose(n, n1); support <- 2:n
  vapply(support, function(rr) {
    if (rr %% 2 == 0) {
      k <- rr %/% 2
      2 * choose(n1 - 1, k - 1) * choose(n2 - 1, k - 1) / den
    } else {
      k <- (rr - 1) %/% 2
      (choose(n1 - 1, k - 1) * choose(n2 - 1, k) +
       choose(n1 - 1, k) * choose(n2 - 1, k - 1)) / den
    }
  }, 0)
}

#' Table D: exact runs distribution -- Theorem 3.2.2, p. 79
#' @noRd
Runstab <- function(n1, n2, r = NULL) {
  n1 <- as.integer(n1); n2 <- as.integer(n2)
  if (n1 < 1L || n2 < 1L) stop("n1 and n2 must be at least 1.", call. = FALSE)
  n <- n1 + n2; support <- 2:n
  pmf <- .gbRunsPmf(n1, n2); cdf <- cumsum(pmf)
  mu <- sum(support * pmf); e2 <- sum(support^2 * pmf)
  pmf_r <- NaN; cdf_r <- NaN; sf_r <- NaN
  if (!is.null(r)) {
    r <- as.integer(r)
    if (r < 2L || r > n) stop("r must lie in 2..n1+n2.", call. = FALSE)
    idx <- r - 1L
    pmf_r <- pmf[idx]; cdf_r <- cdf[idx]
    sf_r <- 1 - (if (idx > 1L) cdf[idx - 1L] else 0)
  }
  list(support = support, pmf = pmf, cdf = cdf, pmf_r = pmf_r,
       cdf_r = cdf_r, sf_r = sf_r, mean = mu, var = e2 - mu^2,
       n1 = n1, n2 = n2)
}

#' Exact runs-test critical region -- Sec. 3.2, p. 84; Table D
#' @noRd
Runscrit <- function(n1, n2, alpha = 0.05, tail = "two-sided") {
  n1 <- as.integer(n1); n2 <- as.integer(n2); alpha <- as.numeric(alpha)
  if (n1 < 1L || n2 < 1L) stop("n1 and n2 must be at least 1.", call. = FALSE)
  if (!tail %in% c("two-sided", "left", "right"))
    stop("tail must be two-sided, left or right.", call. = FALSE)
  n <- n1 + n2; support <- 2:n
  pmf <- .gbRunsPmf(n1, n2)
  a <- if (tail == "two-sided") alpha / 2 else alpha
  lower <- NaN; al <- 0
  if (tail %in% c("two-sided", "left")) {
    acc <- 0
    for (i in seq_along(support)) {
      acc <- acc + pmf[i]
      if (acc <= a) { lower <- as.numeric(support[i]); al <- acc } else break
    }
  }
  upper <- NaN; au <- 0
  if (tail %in% c("two-sided", "right")) {
    acc <- 0
    for (i in rev(seq_along(support))) {
      acc <- acc + pmf[i]
      if (acc <= a) { upper <- as.numeric(support[i]); au <- acc } else break
    }
  }
  list(lower = lower, upper = upper, alpha_lower = al, alpha_upper = au,
       alpha_exact = al + au, n1 = n1, n2 = n2)
}

#' Attainable exact sizes of a discrete test -- Sec. 1.2.9, p. 26
#' @noRd
Exactsize <- function(pmf, alpha = 0.05, upper = TRUE) {
  p <- as.numeric(pmf); k <- length(p)
  if (k < 1L) stop("pmf must be non-empty.", call. = FALSE)
  alpha <- as.numeric(alpha)
  sizes <- if (upper) rev(cumsum(rev(p))) else cumsum(p)
  best <- NaN; cut <- -1L
  rng <- if (upper) rev(seq_len(k)) else seq_len(k)
  for (i in rng) {
    if (sizes[i] <= alpha) { best <- sizes[i]; cut <- as.integer(i - 1L); break }
  }
  list(sizes = sizes, alpha_exact = best, cut = cut, nlevels = k)
}

#' Randomized decision rule of exact size -- Sec. 1.2.9, pp. 26-27
#' @noRd
Randtest <- function(pmf, alpha = 0.05, pmf_alt = NULL) {
  p <- as.numeric(pmf); k <- length(p)
  if (k < 2L) stop("pmf needs at least 2 support points.", call. = FALSE)
  alpha <- as.numeric(alpha)
  t2 <- k; hard <- 0
  for (i in rev(seq_len(k))) {
    tail <- sum(p[i:k])
    if (tail <= alpha) { t2 <- as.integer(i - 1L); hard <- tail } else break
  }
  t1 <- t2 - 1L
  if (t1 < 0L) {
    gamma <- 0; t1 <- 0L
  } else {
    gamma <- if (p[t1 + 1L] > 0) (alpha - hard) / p[t1 + 1L] else 0
    gamma <- min(1, max(0, gamma))
  }
  power <- NaN
  if (!is.null(pmf_alt)) {
    q <- as.numeric(pmf_alt)
    if (length(q) != k) stop("pmf_alt must match pmf in length.", call. = FALSE)
    power <- (if (t2 < k) sum(q[(t2 + 1L):k]) else 0) +
      gamma * (if (t2 > 0L) q[t1 + 1L] else 0)
  }
  list(gamma = gamma, t2 = t2, t1 = t1, size_hard = hard,
       size = hard + gamma * (if (t2 > 0L) p[t1 + 1L] else 0), power = power)
}

#' Consistency of a test -- Sec. 1.2.10, p. 23
#' @noRd
Consist <- function(nvals, effect, alpha = 0.05) {
  ns <- as.integer(nvals)
  if (!length(ns)) stop("nvals must be non-empty.", call. = FALSE)
  if (any(ns < 1L)) stop("sample sizes must be at least 1.", call. = FALSE)
  effect <- as.numeric(effect); alpha <- as.numeric(alpha)
  za <- stats::qnorm(1 - alpha)
  pw <- 1 - stats::pnorm(za - sqrt(ns) * effect)
  mono <- all(diff(pw) >= -1e-15)
  list(power = pw, consistent = as.integer(effect > 0),
       monotone = as.integer(mono), limit = pw[length(pw)], effect = effect)
}

#' Chi-square goodness of fit -- eq. (4.2.1), p. 104
#' @noRd
Chigof <- function(observed, expected, ddof = 0) {
  o <- as.numeric(observed); e <- as.numeric(expected); k <- length(o)
  if (k < 2L) stop("need at least 2 categories.", call. = FALSE)
  if (length(e) != k) stop("observed and expected must have equal length.", call. = FALSE)
  if (any(e <= 0)) stop("expected frequencies must be strictly positive.", call. = FALSE)
  contrib <- (o - e)^2 / e
  q <- sum(contrib); df <- k - 1L - as.integer(ddof)
  if (df < 1L) stop("degrees of freedom must be at least 1.", call. = FALSE)
  list(statistic = q, df = df, p_value = stats::pchisq(q, df, lower.tail = FALSE),
       k = k, n = sum(o), contrib = contrib)
}

#' KS statistics via the PIT -- Theorem 4.3.1, p. 111
#' @noRd
Ksdistfree <- function(x, cdf) {
  xs <- sort(as.numeric(x)); n <- length(xs)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  z <- vapply(xs, function(v) as.numeric(cdf(v)), 0)
  dp <- max(seq_len(n) / n - z)
  dm <- max(z - (seq_len(n) - 1) / n)
  list(statistic = max(dp, dm), dplus = dp, dminus = dm, z = z, n = n)
}

#' Exact P(D_n < d) -- Theorem 4.3.2 via Durbin's matrix identity, p. 111
#' @noRd
Ksexact <- function(d, n) {
  d <- as.numeric(d); n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (d <= 0) return(list(cdf = 0, sf = 1, k = 0L, t = 0, n = n, d = d))
  if (d >= 1) return(list(cdf = 1, sf = 0, k = n, t = 0, n = n, d = d))
  k <- as.integer(ceiling(n * d)); t <- k - n * d; m <- 2L * k - 1L
  h <- matrix(0, m, m)
  for (i in seq_len(m)) for (j in seq_len(m)) if (i - j + 1 >= 0) h[i, j] <- 1
  for (i in seq_len(m)) {
    h[i, 1] <- h[i, 1] - t^i
    h[m, i] <- h[m, i] - t^(m - i + 1)
  }
  h[m, 1] <- h[m, 1] + if (2 * t - 1 > 0) (2 * t - 1)^m else 0
  for (i in seq_len(m)) for (j in seq_len(m)) {
    if (i - j + 1 > 0) for (g in seq_len(i - j + 1)) h[i, j] <- h[i, j] / g
  }
  eq <- 0
  res <- diag(m); base <- h; e_base <- 0; p <- n
  rescale <- function(a, e) {
    v <- a[(m %/% 2) + 1L, (m %/% 2) + 1L]
    if (v > 1e140) { a <- a * 1e-140; e <- e + 140 }
    list(a = a, e = e)
  }
  while (p > 0) {
    if (bitwAnd(p, 1L) == 1L) {
      res <- res %*% base; eq <- eq + e_base
      rr <- rescale(res, eq); res <- rr$a; eq <- rr$e
    }
    p <- p %/% 2L
    if (p > 0) {
      base <- base %*% base; e_base <- 2 * e_base
      rr <- rescale(base, e_base); base <- rr$a; e_base <- rr$e
    }
  }
  s <- res[k, k]
  for (i in seq_len(n)) {
    s <- s * i / n
    if (s < 1e-140) { s <- s * 1e140; eq <- eq + 140 }
  }
  val <- if (eq != 0) s * 10^(-eq) else s
  val <- min(1, max(0, val))
  list(cdf = val, sf = 1 - val, k = k, t = t, n = n, d = d)
}

#' Exact P(D+ >= c) -- Theorem 4.3.4, Birnbaum-Tingey, p. 115
#' @noRd
Ksplusdist <- function(c, n) {
  c <- as.numeric(c); n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (c <= 0) return(list(sf = 1, cdf = 0, terms = 0L, n = n, c = c))
  if (c >= 1) return(list(sf = 0, cdf = 1, terms = 0L, n = n, c = c))
  jmax <- as.integer(floor(n * (1 - c))); j <- 0:jmax
  total <- sum(choose(n, j) * (c + j / n)^(j - 1) * (1 - c - j / n)^(n - j))
  sf <- min(1, max(0, c * total))
  list(sf = sf, cdf = 1 - sf, terms = jmax + 1L, n = n, c = c)
}

#' Kolmogorov limit Q(k)
#' @noRd
.gbKsQ <- function(k) 2 * sum((-1)^(0:99) * exp(-2 * (1:100)^2 * k * k))

#' KS critical value (Table F) by exact bisection -- p. 565
#' @noRd
Kscrit <- function(n, alpha = 0.05, exact = TRUE) {
  n <- as.integer(n); alpha <- as.numeric(alpha)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  lo <- 1e-6; hi <- 10
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (.gbKsQ(mid) > alpha) lo <- mid else hi <- mid
  }
  ka <- (lo + hi) / 2
  dasy <- ka / sqrt(n)
  dex <- NaN
  if (exact) {
    lo <- 1e-9; hi <- 1
    for (i in 1:200) {
      mid <- (lo + hi) / 2
      if (1 - Ksexact(mid, n)$cdf > alpha) lo <- mid else hi <- mid
    }
    dex <- (lo + hi) / 2
  }
  list(dcrit = dex, dcrit_asymp = dasy, k_alpha = ka, n = n, alpha = alpha)
}

#' KS confidence band -- Sec. 4.4.2, p. 121
#' @noRd
Ksband <- function(x, dcrit, at = NULL) {
  xs <- sort(as.numeric(x)); n <- length(xs); dcrit <- as.numeric(dcrit)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  if (dcrit <= 0 || dcrit >= 1)
    stop("dcrit must lie strictly inside (0, 1).", call. = FALSE)
  pts <- if (is.null(at)) xs else as.numeric(at)
  edf <- vapply(pts, function(v) sum(xs <= v) / n, 0)
  lo <- pmax(edf - dcrit, 0); hi <- pmin(edf + dcrit, 1)
  list(at = pts, edf = edf, lower = lo, upper = hi, width = hi - lo,
       dcrit = dcrit, n = n)
}

#' KS sample size for uniform error c -- Sec. 4.4.3, p. 122
#' @noRd
Ksn <- function(c, alpha = 0.05) {
  c <- as.numeric(c); alpha <- as.numeric(alpha)
  if (c <= 0 || c >= 1) stop("c must lie strictly inside (0, 1).", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  lo <- 1e-6; hi <- 10
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (.gbKsQ(mid) > alpha) lo <- mid else hi <- mid
  }
  ka <- (lo + hi) / 2
  nasy <- as.integer(ceiling((ka / c)^2))
  n <- max(1L, nasy - 20L); cov <- 0
  for (i in 1:400) {
    cov <- Ksexact(c, n)$cdf
    if (cov >= 1 - alpha) break
    n <- n + 1L
  }
  list(n = n, n_asymp = nasy, k_alpha = ka, coverage = cov, c = c, alpha = alpha)
}

#' Cramer-von Mises W^2 -- Problem 4.14, p. 150
#' @noRd
Cvmw2 <- function(x, cdf) {
  xs <- sort(as.numeric(x)); n <- length(xs)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  z <- vapply(xs, function(v) as.numeric(cdf(v)), 0)
  j <- seq_len(n)
  w2 <- 1 / (12 * n) + sum((z - (2 * j - 1) / (2 * n))^2)
  list(statistic = w2, nw2 = n * w2, z = z, n = n)
}

#' KS vs Cramer-von Mises on one sample -- Sec. 4.9, p. 146
#' @noRd
Kscvmcmp <- function(x, cdf) {
  xs <- sort(as.numeric(x)); n <- length(xs)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  z <- vapply(xs, function(v) as.numeric(cdf(v)), 0)
  j <- seq_len(n)
  devs <- pmax(j / n - z, z - (j - 1) / n)
  d <- max(devs); arg <- which.max(devs)
  terms <- (z - (2 * j - 1) / (2 * n))^2
  w2 <- 1 / (12 * n) + sum(terms)
  list(d = d, w2 = w2, argmax = as.integer(arg - 1L),
       share = terms[arg] / w2, n = n)
}

#' Table O -- Lilliefors normal critical values, p. 589
#' @noRd
.gbTableO <- list(
  N = c(4:12, 14, 16, 18, 20, 25, 30, 40, 50, 60, 75, 100),
  V = rbind(
    c(.344, .375, .414, .432), c(.320, .344, .398, .427),
    c(.298, .323, .369, .421), c(.281, .305, .351, .399),
    c(.266, .289, .334, .383), c(.252, .273, .316, .366),
    c(.240, .261, .305, .350), c(.231, .251, .291, .331),
    c(.223, .242, .281, .327), c(.208, .226, .262, .302),
    c(.195, .213, .249, .291), c(.185, .201, .234, .272),
    c(.176, .192, .223, .266), c(.159, .173, .202, .236),
    c(.146, .159, .186, .219), c(.127, .139, .161, .190),
    c(.114, .125, .145, .173), c(.105, .114, .133, .159),
    c(.094, .102, .119, .138), c(.082, .089, .104, .121)),
  TAIL = c(.816, .888, 1.038, 1.212))

#' Lilliefors test for normality -- Sec. 4.5, p. 126; Table O
#' @noRd
Lillienorm <- function(x, alpha = 0.05) {
  xs <- sort(as.numeric(x)); n <- length(xs); alpha <- as.numeric(alpha)
  if (n < 4L) stop("need at least 4 observations.", call. = FALSE)
  lev <- c(0.100, 0.050, 0.010, 0.001)
  col <- which(abs(lev - alpha) < 1e-12)
  if (!length(col)) stop("alpha must be one of 0.10, 0.05, 0.01, 0.001.", call. = FALSE)
  mu <- mean(xs); sd <- stats::sd(xs)
  if (sd <= 0) stop("sample has zero variance.", call. = FALSE)
  z <- stats::pnorm((xs - mu) / sd); j <- seq_len(n)
  d <- max(pmax(j / n - z, z - (j - 1) / n))
  if (n > 100L) {
    ntab <- 0L; dcrit <- .gbTableO$TAIL[col] / sqrt(n)
  } else {
    idx <- max(which(.gbTableO$N <= n))
    ntab <- as.integer(.gbTableO$N[idx]); dcrit <- .gbTableO$V[idx, col]
  }
  list(statistic = d, dcrit = dcrit, reject = as.integer(d > dcrit),
       mean = mu, sd = sd, n = n, n_table = ntab, alpha = alpha)
}

#' Table T -- Lilliefors exponential critical values, p. 598
#' @noRd
.gbTableT <- list(
  N = c(4:12, 14, 16, 18, 20, 25, 30, 40, 50, 60, 75, 100),
  V = rbind(
    c(.444, .483, .556, .626), c(.405, .443, .514, .585),
    c(.374, .410, .477, .551), c(.347, .381, .444, .509),
    c(.327, .359, .421, .502), c(.310, .339, .399, .460),
    c(.296, .325, .379, .444), c(.284, .312, .366, .433),
    c(.271, .299, .350, .412), c(.252, .277, .325, .388),
    c(.237, .261, .311, .366), c(.224, .247, .293, .328),
    c(.213, .234, .279, .329), c(.192, .211, .251, .296),
    c(.176, .193, .229, .270), c(.153, .168, .201, .241),
    c(.137, .150, .179, .214), c(.125, .138, .164, .193),
    c(.113, .124, .146, .173), c(.098, .108, .127, .150)),
  TAIL = c(.980, 1.077, 1.274, 1.501))

#' Lilliefors test for exponentiality -- Sec. 4.6, p. 133; Table T
#' @noRd
Lillieexp <- function(x, alpha = 0.05) {
  xs <- sort(as.numeric(x)); n <- length(xs); alpha <- as.numeric(alpha)
  if (n < 4L) stop("need at least 4 observations.", call. = FALSE)
  if (any(xs < 0)) stop("exponential data must be non-negative.", call. = FALSE)
  lev <- c(0.100, 0.050, 0.010, 0.001)
  col <- which(abs(lev - alpha) < 1e-12)
  if (!length(col)) stop("alpha must be one of 0.10, 0.05, 0.01, 0.001.", call. = FALSE)
  mu <- mean(xs)
  if (mu <= 0) stop("sample mean must be strictly positive.", call. = FALSE)
  z <- 1 - exp(-xs / mu); j <- seq_len(n)
  d <- max(pmax(j / n - z, z - (j - 1) / n))
  if (n > 100L) {
    ntab <- 0L; dcrit <- .gbTableT$TAIL[col] / sqrt(n)
  } else {
    idx <- max(which(.gbTableT$N <= n))
    ntab <- as.integer(.gbTableT$N[idx]); dcrit <- .gbTableT$V[idx, col]
  }
  list(statistic = d, dcrit = dcrit, reject = as.integer(d > dcrit),
       mean = mu, n = n, n_table = ntab, alpha = alpha)
}

#' Table 4.7.1 -- A-D upper tail percentage points (Stephens 1986), p. 140
#' @noRd
.gbAdTable <- list(
  levels = c(0.01, 0.025, 0.05, 0.10, 0.15),
  rows = list(
    "specified"   = c(3.857, 3.070, 2.492, 1.933, 1.610),
    "normal-mean" = c(1.551, 1.285, 1.087, 0.894, 0.782),
    "normal-var"  = c(3.702, 2.898, 2.308, 1.743, 1.430),
    "normal-both" = c(1.035, 0.873, 0.752, 0.631, 0.561),
    "exponential" = c(1.959, 1.591, 1.321, 1.062, 0.916)))

#' Anderson-Darling W_n^2 -- eq. (4.7.1), p. 138; Table 4.7.1, p. 140
#' @noRd
Adtest <- function(x, cdf, case = "specified", alpha = 0.05) {
  xs <- sort(as.numeric(x)); n <- length(xs); alpha <- as.numeric(alpha)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (!case %in% names(.gbAdTable$rows))
    stop("case must be a row label of Table 4.7.1.", call. = FALSE)
  col <- which(abs(.gbAdTable$levels - alpha) < 1e-12)
  if (!length(col))
    stop("alpha must be one of 0.01, 0.025, 0.05, 0.10, 0.15.", call. = FALSE)
  z <- vapply(xs, function(v) as.numeric(cdf(v)), 0)
  if (any(z <= 0 | z >= 1))
    stop("F_0 values must lie strictly inside (0, 1).", call. = FALSE)
  j <- seq_len(n)
  s <- sum((2 * j - 1) * (log(z) + log(1 - rev(z))))
  a2 <- -n - s / n
  astar <- if (case == "normal-both") a2 * (1 + 0.75 / n + 2.25 / n^2) else
    if (case == "exponential") a2 * (1 + 0.3 / n) else a2
  crit <- .gbAdTable$rows[[case]][col]
  list(statistic = a2, astar = astar, crit = crit,
       reject = as.integer(astar > crit), z = z, n = n, case = case,
       alpha = alpha)
}

#' Runs pmf helper for the two-sample test -- eq. (3.2.3), p. 79
#' @noRd
.gbRunsPmf2 <- function(m, n) .gbRunsPmf(m, n)

#' Wald-Wolfowitz two-sample runs test -- Sec. 6.2, p. 231
#' @noRd
Wwruns <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  o <- order(c(xs, ys), c(rep(0L, m), rep(1L, n)))
  labels <- c(rep(0L, m), rep(1L, n))[o]
  r <- 1L + sum(labels[-1] != labels[-length(labels)])
  nn <- m + n
  pmf <- .gbRunsPmf(m, n); support <- 2:nn
  tail <- sum(pmf[support <= r])
  mean <- 2 * m * n / nn + 1
  var <- 2 * m * n * (2 * m * n - nn) / (nn^2 * (nn - 1))
  z <- (r - mean) / sqrt(var)
  list(statistic = r, p_value = min(1, tail), z = z,
       p_normal = stats::pnorm(z), mean = mean, var = var, m = m, n = n)
}

#' Runs-test tie bounds -- Sec. 6.2.1, p. 233
#' @noRd
Wwties <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  vals <- sort(unique(c(xs, ys)))
  ga <- vapply(vals, function(v) sum(xs == v), 0L)
  gb <- vapply(vals, function(v) sum(ys == v), 0L)
  nties <- sum(ga > 0L & gb > 0L)
  runs <- function(s) if (!length(s)) 0L else 1L + sum(s[-1] != s[-length(s)])
  lo <- integer(0)
  for (i in seq_along(vals)) {
    a <- ga[i]; b <- gb[i]
    if (a > 0L && b > 0L) {
      if (length(lo) && lo[length(lo)] == 0L) {
        lo <- c(lo, rep(0L, a), rep(1L, b))
      } else {
        lo <- c(lo, rep(1L, b), rep(0L, a))
      }
    } else lo <- c(lo, rep(0L, a), rep(1L, b))
  }
  hi <- integer(0)
  for (i in seq_along(vals)) {
    a <- ga[i]; b <- gb[i]
    if (a > 0L && b > 0L) {
      ii <- 0L; jj <- 0L
      start <- if (length(hi) && hi[length(hi)] == 0L) 1L else 0L
      while (ii < a || jj < b) {
        if (start == 0L && ii < a) { hi <- c(hi, 0L); ii <- ii + 1L }
        else if (jj < b) { hi <- c(hi, 1L); jj <- jj + 1L }
        else if (ii < a) { hi <- c(hi, 0L); ii <- ii + 1L }
        start <- 1L - start
      }
    } else hi <- c(hi, rep(0L, a), rep(1L, b))
  }
  rmin <- runs(lo); rmax <- runs(hi)
  list(rmin = rmin, rmax = rmax, nties = nties,
       ambiguous = as.integer(rmin != rmax), m = m, n = n)
}

#' Exact two-sample runs test -- Sec. 6.2, p. 231
#' @noRd
Wwexact <- function(x, y, tail = "left") {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  if (!tail %in% c("left", "right", "two-sided"))
    stop("tail must be left, right or two-sided.", call. = FALSE)
  o <- order(c(xs, ys), c(rep(0L, m), rep(1L, n)))
  labels <- c(rep(0L, m), rep(1L, n))[o]
  r <- 1L + sum(labels[-1] != labels[-length(labels)])
  nn <- m + n; support <- 2:nn
  pmf <- .gbRunsPmf(m, n)
  left <- sum(pmf[support <= r]); right <- sum(pmf[support >= r])
  pv <- switch(tail, "left" = left, "right" = right,
               "two-sided" = min(1, 2 * min(left, right)))
  list(statistic = r, p_value = min(1, pv), p_left = left, p_right = right,
       support = support, pmf = pmf, m = m, n = n)
}

#' Lattice-path count for the two-sample KS tail
#' @noRd
.gbKs2count <- function(m, n, d, onesided = FALSE) {
  lim <- d - 1e-12
  prev <- numeric(n + 1); row <- numeric(n + 1)
  for (i in 0:m) {
    for (j in 0:n) {
      dev <- i / m - j / n
      if (!onesided) dev <- abs(dev)
      if (dev >= lim) {
        row[j + 1] <- 0
      } else if (i == 0 && j == 0) {
        row[j + 1] <- 1
      } else {
        v <- if (i > 0) prev[j + 1] else 0
        v <- v + if (j > 0) row[j] else 0
        row[j + 1] <- v
      }
    }
    prev <- row
  }
  total <- choose(m + n, m)
  max(0, min(1, 1 - prev[n + 1] / total))
}

#' Two-sample KS test -- Sec. 6.3, p. 239
#' @noRd
Ks2 <- function(x, y) {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y))
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  pts <- sort(unique(c(xs, ys)))
  sm <- vapply(pts, function(t) sum(xs <= t) / m, 0)
  sn <- vapply(pts, function(t) sum(ys <= t) / n, 0)
  dp <- max(sm - sn); dm <- max(sn - sm); d <- max(dp, dm)
  list(statistic = d, p_value = .gbKs2count(m, n, d, FALSE),
       dplus = dp, dminus = dm, m = m, n = n)
}

#' Two-sample KS asymptotic -- Sec. 6.3, p. 241
#' @noRd
Ks2asymp <- function(d, m, n) {
  d <- as.numeric(d); m <- as.integer(m); n <- as.integer(n)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (d < 0) stop("d must be non-negative.", call. = FALSE)
  neff <- m * n / (m + n); k <- sqrt(neff) * d
  pv <- if (k <= 0) 1 else min(1, max(0, .gbKsQ(k)))
  list(p_value = pv, k = k, neff = neff, m = m, n = n)
}

#' Exact two-sided Smirnov distribution -- Sec. 6.3, p. 239
#' @noRd
Smirnov2 <- function(d, m, n) {
  d <- as.numeric(d); m <- as.integer(m); n <- as.integer(n)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (d <= 0 || d > 1) stop("d must lie in (0, 1].", call. = FALSE)
  sf <- .gbKs2count(m, n, d, FALSE); total <- choose(m + n, m)
  list(sf = sf, cdf = 1 - sf, npaths = total, inside = (1 - sf) * total,
       m = m, n = n)
}

#' Exact one-sided Smirnov distribution -- Sec. 6.3, p. 241
#' @noRd
Smirnov1 <- function(d, m, n) {
  d <- as.numeric(d); m <- as.integer(m); n <- as.integer(n)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (d <= 0 || d > 1) stop("d must lie in (0, 1].", call. = FALSE)
  sf <- .gbKs2count(m, n, d, TRUE)
  k <- sqrt(m * n / (m + n)) * d
  list(sf = sf, cdf = 1 - sf, sf_asymp = exp(-2 * k * k), k = k, m = m, n = n)
}

#' Two-sample median test -- Sec. 6.4, p. 247
#' @noRd
Medtest <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  pooled <- sort(c(xs, ys)); nn <- m + n
  med <- if (nn %% 2 == 1) pooled[(nn %/% 2) + 1] else
    (pooled[nn %/% 2] + pooled[(nn %/% 2) + 1]) / 2
  t <- sum(pooled > med); u <- sum(xs > med)
  pk <- function(k) {
    if (k < 0 || k > m || t - k < 0 || t - k > n) return(0)
    choose(m, k) * choose(n, t - k) / choose(nn, t)
  }
  lower <- sum(vapply(0:u, pk, 0)); upper <- sum(vapply(u:m, pk, 0))
  mean <- m * t / nn
  var <- if (nn > 1) m * n * t * (nn - t) / (nn^2 * (nn - 1)) else NaN
  list(statistic = u, p_value = min(1, 2 * min(lower, upper)),
       p_lower = lower, p_upper = upper, median = med, t = t,
       mean = mean, var = var, m = m, n = n)
}

#' Median-test confidence interval -- Sec. 6.4.2, p. 251
#' @noRd
Medtestci <- function(x, y, c) {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y))
  m <- length(xs); n <- length(ys); c <- as.integer(c)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  if (c < 1L || c > min(m, n)) stop("c must lie in 1..min(m, n).", call. = FALSE)
  list(lower = ys[c] - xs[m - c + 1L], upper = ys[n - c + 1L] - xs[c],
       estimate = stats::median(ys) - stats::median(xs), c = c, m = m, n = n)
}

#' Composite Simpson on (0, 1), fixed odd node count
#' @noRd
.gbSimpson <- function(f, nodes = 2001) {
  if (nodes %% 2 == 0) nodes <- nodes + 1
  h <- 1 / (nodes - 1); i <- 0:(nodes - 1); u <- i * h
  w <- ifelse(i == 0 | i == nodes - 1, 1, ifelse(i %% 2 == 1, 4, 2))
  sum(w * vapply(u, f, 0)) * h / 3
}

#' Precedence/median-test power -- eqs. (6.4.9)-(6.4.10), p. 254
#' @noRd
Medtestpow <- function(m, n, r, wcrit, g, nodes = 2001) {
  m <- as.integer(m); n <- as.integer(n); r <- as.integer(r)
  wcrit <- as.integer(wcrit)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (r < 1L || r > m) stop("need 1 <= r <= m.", call. = FALSE)
  beta <- gamma(r) * gamma(m - r + 1) / gamma(m + 1)
  pmf <- vapply(0:n, function(i) {
    f <- function(u) {
      gu <- min(1, max(0, as.numeric(g(u))))
      gu^i * (1 - gu)^(n - i) * u^(r - 1) * (1 - u)^(m - r)
    }
    choose(n, i) * .gbSimpson(f, nodes) / beta
  }, 0)
  power <- if (wcrit > 0L) sum(pmf[1:min(wcrit, n + 1L)]) else 0
  list(power = power, pmf = pmf, m = m, n = n, r = r, wcrit = wcrit)
}

#' Two-sided median test with exact region -- Sec. 6.4, p. 247
#' @noRd
Medtest2 <- function(x, y, alpha = 0.05) {
  xs <- as.numeric(x); ys <- as.numeric(y); alpha <- as.numeric(alpha)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  pooled <- sort(c(xs, ys)); nn <- m + n
  med <- if (nn %% 2 == 1) pooled[(nn %/% 2) + 1] else
    (pooled[nn %/% 2] + pooled[(nn %/% 2) + 1]) / 2
  t <- sum(pooled > med); u <- sum(xs > med)
  pk <- function(k) {
    if (k < 0 || k > m || t - k < 0 || t - k > n) return(0)
    choose(m, k) * choose(n, t - k) / choose(nn, t)
  }
  lo <- NaN; al <- 0; acc <- 0
  for (k in 0:m) {
    acc <- acc + pk(k)
    if (acc <= alpha / 2) { lo <- as.numeric(k); al <- acc } else break
  }
  hi <- NaN; au <- 0; acc <- 0
  for (k in m:0) {
    acc <- acc + pk(k)
    if (acc <= alpha / 2) { hi <- as.numeric(k); au <- acc } else break
  }
  lower <- sum(vapply(0:u, pk, 0)); upper <- sum(vapply(u:m, pk, 0))
  list(statistic = u, p_value = min(1, 2 * min(lower, upper)),
       lower = lo, upper = hi, alpha_exact = al + au, t = t, m = m, n = n)
}

#' Ties at the combined median -- Sec. 6.4, p. 247
#' @noRd
Medties <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  pooled <- sort(c(xs, ys)); nn <- m + n
  med <- if (nn %% 2 == 1) pooled[(nn %/% 2) + 1] else
    (pooled[nn %/% 2] + pooled[(nn %/% 2) + 1]) / 2
  nties <- sum(pooled == med); xt <- sum(xs == med); us <- sum(xs > med)
  list(u_strict = as.numeric(us), u_inclusive = as.numeric(us + xt),
       u_split = us + xt / 2, t_strict = sum(pooled > med),
       t_inclusive = sum(pooled >= med), nties = nties, median = med,
       m = m, n = n)
}

#' Control median test -- Sec. 6.5, eq. (6.5.1), p. 256
#' @noRd
Ctrlmed <- function(x, y, alternative = "two-sided") {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y))
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  if (n %% 2 == 0)
    stop("the control sample size n must be odd (n = 2r+1).", call. = FALSE)
  r <- (n - 1L) %/% 2L
  my <- ys[r + 1L]
  v <- sum(xs <= my)
  den <- choose(m + 2 * r + 1, m); j <- 0:m
  pmf <- choose(m + r - j, m - j) * choose(j + r, j) / den
  lower <- sum(pmf[1:(v + 1L)]); upper <- sum(pmf[(v + 1L):(m + 1L)])
  pv <- switch(alternative,
    "greater" = upper, "less" = lower,
    "two-sided" = min(1, 2 * min(lower, upper)),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  var <- m * (m + n) / (4 * n)
  list(statistic = v, p_value = pv, z = (v - m / 2) / sqrt(var),
       mean = m / 2, var = var, pmf = pmf, r = r, m = m, n = n)
}

#' Curtailed control median test -- eq. (6.5.2), p. 258
#' @noRd
Ctrlmedcur <- function(m, n, alpha = 0.05) {
  m <- as.integer(m); n <- as.integer(n); alpha <- as.numeric(alpha)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (n %% 2 == 0)
    stop("the control sample size n must be odd (n = 2r+1).", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  za <- stats::qnorm(1 - alpha)
  draw <- m / 2 - za * sqrt(m * (m + n) / (4 * n))
  r <- (n - 1L) %/% 2L
  den <- choose(m + 2 * r + 1, m); j <- 0:m
  pmf <- choose(m + r - j, m - j) * choose(j + r, j) / den
  dex <- NaN; aex <- 0; acc <- 0
  for (k in 0:m) {
    acc <- acc + pmf[k + 1L]
    if (acc <= alpha) { dex <- as.numeric(k); aex <- acc } else break
  }
  list(d = floor(draw), d_raw = draw, d_exact = dex, alpha_exact = aex,
       z_alpha = za, m = m, n = n)
}

#' Control median test power -- Sec. 6.5.2, p. 258
#' @noRd
Ctrlmedpow <- function(m, n, d, h, nodes = 2001) {
  m <- as.integer(m); n <- as.integer(n); d <- as.integer(d)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (n %% 2 == 0)
    stop("the control sample size n must be odd (n = 2r+1).", call. = FALSE)
  r <- (n - 1L) %/% 2L
  coef <- factorial(n) / (factorial(r) * factorial(r))
  pmf <- vapply(0:m, function(j) {
    f <- function(v) {
      hv <- min(1, max(0, as.numeric(h(v))))
      hv^j * (1 - hv)^(m - j) * v^r * (1 - v)^r
    }
    choose(m, j) * coef * .gbSimpson(f, nodes)
  }, 0)
  power <- if (d >= 0L) sum(pmf[1:min(d + 1L, m + 1L)]) else 0
  list(power = power, pmf = pmf, q = as.numeric(h(0.5)), r = r,
       m = m, n = n, d = d)
}

#' Exact null counts of U / shifted W by the (6.6.14) recursion
#' @noRd
.gbRankCounts <- function(m, n) {
  total <- m * n
  counts <- numeric(total + 1); counts[1] <- 1
  for (i in seq_len(m)) {
    new <- numeric(total + 1); run <- 0
    for (k in 0:total) {
      run <- run + counts[k + 1]
      if (k - n - 1 >= 0) run <- run - counts[k - n]
      new[k + 1] <- run
    }
    counts <- new
  }
  counts
}

#' Mann-Whitney U -- Sec. 6.6, eqs. (6.6.1), (6.6.14), p. 260
#' @noRd
Mwu <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  u <- sum(outer(xs, ys, ">")) + 0.5 * sum(outer(xs, ys, "=="))
  total <- m * n
  pmf <- .gbRankCounts(m, n) / choose(m + n, m)
  ui <- as.integer(round(u)); ui <- max(0L, min(total, ui))
  lower <- sum(pmf[1:(ui + 1L)]); upper <- sum(pmf[(ui + 1L):(total + 1L)])
  mean <- m * n / 2; var <- m * n * (m + n + 1) / 12
  z <- (u - mean) / sqrt(var)
  list(statistic = u, p_value = min(1, 2 * min(lower, upper)), z = z,
       p_normal = 2 * (1 - stats::pnorm(abs(z))), mean = mean, var = var,
       m = m, n = n)
}

#' Mann-Whitney shift CI -- Sec. 6.6.2, p. 267
#' @noRd
Mwuci <- function(x, y, k) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys); k <- as.integer(k)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  d <- sort(as.numeric(outer(xs, ys, "-"))); nd <- length(d)
  if (k < 0L || k >= nd) stop("k must lie in 0..mn-1.", call. = FALSE)
  list(lower = d[k + 1L], upper = d[nd - k], estimate = stats::median(d),
       k = k, ndiff = nd, m = m, n = n)
}

#' Mann-Whitney sample size -- eq. (6.6.18), p. 269 (Noether 1987)
#' @noRd
Mwun <- function(p, c = 0.5, alpha = 0.05, beta = 0.10, twosided = FALSE) {
  p <- as.numeric(p); c <- as.numeric(c)
  alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  if (p == 0.5) stop("p must differ from 0.5.", call. = FALSE)
  if (c <= 0 || c >= 1) stop("c must lie strictly inside (0, 1).", call. = FALSE)
  a <- if (twosided) alpha / 2 else alpha
  za <- stats::qnorm(1 - a); zb <- stats::qnorm(1 - beta)
  nraw <- (za + zb)^2 / (12 * c * (1 - c) * (p - 0.5)^2)
  ntot <- as.integer(ceiling(nraw)); mm <- as.integer(round(c * ntot))
  list(n = ntot, n_raw = nraw, m = mm, n_y = ntot - mm, z_alpha = za,
       z_beta = zb, c = c, p = p)
}

#' Wilcoxon rank-sum W_N -- Sec. 8.2, pp. 290-291
#' @noRd
Wrs <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  rk <- rank(c(xs, ys), ties.method = "average")
  w <- sum(rk[seq_len(m)])
  wmin <- m * (m + 1) / 2; wmax <- m * (2 * nn - m + 1) / 2
  total <- m * n
  pmf <- .gbRankCounts(m, n) / choose(nn, m)
  shifted <- as.integer(round(w - wmin)); shifted <- max(0L, min(total, shifted))
  lower <- sum(pmf[1:(shifted + 1L)])
  upper <- sum(pmf[(shifted + 1L):(total + 1L)])
  mean <- m * (nn + 1) / 2; var <- m * n * (nn + 1) / 12
  z <- (w - mean) / sqrt(var)
  list(statistic = w, p_value = min(1, 2 * min(lower, upper)), z = z,
       p_normal = 2 * (1 - stats::pnorm(abs(z))), mean = mean, var = var,
       wmin = wmin, wmax = wmax, m = m, n = n)
}

#' Rank-sum shift CI -- Sec. 8.2, p. 292
#' @noRd
Wrsci <- function(x, y, wcrit) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  k <- as.integer(round(as.numeric(wcrit) - m * (m + 1) / 2))
  d <- sort(as.numeric(outer(xs, ys, "-"))); nd <- length(d)
  if (k < 0L || k >= nd)
    stop("wcrit implies an index outside 0..mn-1.", call. = FALSE)
  list(lower = d[k + 1L], upper = d[nd - k], k = k,
       estimate = stats::median(d), ndiff = nd, m = m, n = n)
}

#' Rank-sum normal approximation -- Sec. 8.2, p. 291
#' @noRd
Wrsz <- function(w, m, n, alternative = "two-sided", correct = FALSE,
                 ties = NULL) {
  m <- as.integer(m); n <- as.integer(n); w <- as.numeric(w)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  nn <- m + n
  mean <- m * (nn + 1) / 2
  v0 <- m * n * (nn + 1) / 12
  var <- v0
  if (!is.null(ties) && length(ties)) {
    tv <- as.numeric(ties)
    var <- m * n / 12 * ((nn + 1) - sum(tv * (tv^2 - 1)) / (nn * (nn - 1)))
  }
  d <- w - mean
  if (correct) { if (d > 0) d <- d - 0.5 else if (d < 0) d <- d + 0.5 }
  z <- d / sqrt(var)
  pv <- switch(alternative,
    "greater" = 1 - stats::pnorm(z), "less" = stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(z = z, p_value = min(1, pv), mean = mean, var = var,
       var_uncorrected = v0, m = m, n = n)
}

#' Expected standard-normal order statistic, fixed-grid Simpson
#' @noRd
.gbEnos <- function(i, n, lo = -8, hi = 8, nodes = 4001) {
  if (nodes %% 2 == 0) nodes <- nodes + 1
  h <- (hi - lo) / (nodes - 1); k <- 0:(nodes - 1); z <- lo + k * h
  w <- ifelse(k == 0 | k == nodes - 1, 1, ifelse(k %% 2 == 1, 4, 2))
  coef <- exp(lgamma(n + 1) - lgamma(i) - lgamma(n - i + 1))
  p <- stats::pnorm(z)
  coef * sum(w * z * p^(i - 1) * (1 - p)^(n - i) * stats::dnorm(z)) * h / 3
}

#' Terry-Hoeffding normal-scores test -- Sec. 8.3.1, p. 299
#' @noRd
Normscores <- function(x, y, nodes = 4001) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  lab <- c(rep(0L, m), rep(1L, n))
  o <- order(c(xs, ys), lab)
  tag <- lab[o]
  scores <- vapply(seq_len(nn), function(i) .gbEnos(i, nn, nodes = nodes), 0)
  stat <- sum(scores[tag == 0L])
  var <- m * n * sum(scores^2) / (nn * (nn - 1))
  z <- stat / sqrt(var)
  list(statistic = stat, z = z, p_value = 2 * (1 - stats::pnorm(abs(z))),
       mean = 0, var = var, scores = scores, m = m, n = n)
}

#' van der Waerden test -- Sec. 8.3.2, p. 301
#' @noRd
Vdw <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  lab <- c(rep(0L, m), rep(1L, n))
  o <- order(c(xs, ys), lab); tag <- lab[o]
  scores <- stats::qnorm(seq_len(nn) / (nn + 1))
  stat <- sum(scores[tag == 0L])
  abar <- mean(scores); ss <- sum((scores - abar)^2)
  mean <- m * abar; var <- m * n * ss / (nn * (nn - 1))
  z <- (stat - mean) / sqrt(var)
  list(statistic = stat, z = z, p_value = 2 * (1 - stats::pnorm(abs(z))),
       mean = mean, var = var, scores = scores, m = m, n = n)
}

#' Percentile modified rank test for location -- eqs. (8.3.5)-(8.3.6), p. 304
#' @noRd
Pctrankloc <- function(x, y, s = 0.5, r = NULL) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  s <- as.numeric(s); r <- if (is.null(r)) s else as.numeric(r)
  if (s <= 0 || s > 1 || r <= 0 || r > 1)
    stop("s and r must lie in (0, 1].", call. = FALSE)
  nn <- m + n
  lab <- c(rep(0L, m), rep(1L, n))
  o <- order(c(xs, ys), lab); z <- as.numeric(lab[o] == 0L)
  S <- min(as.integer(floor(nn * s)) + 1L, nn)
  R <- min(as.integer(floor(nn * r)) + 1L, nn)
  half <- if (nn %% 2 == 0) 0.5 else 0
  a <- numeric(nn)
  for (i in seq_len(R)) a[i] <- a[i] - (R - i + 1 - half)
  for (i in (nn - S + 1L):nn) a[i] <- a[i] + (i - (nn - S) - half)
  blower <- sum(vapply(seq_len(R), function(i) (R - i + 1 - half) * z[i], 0))
  tupper <- sum(vapply((nn - S + 1L):nn,
                       function(i) (i - (nn - S) - half) * z[i], 0))
  abar <- mean(a); ss <- sum((a - abar)^2)
  mean <- m * abar; var <- m * n * ss / (nn * (nn - 1))
  vb <- if (nn %% 2 == 0 && S == R)
    m * n * S * (4 * S^2 - 1) / (6 * nn * (nn - 1)) else NaN
  stat <- tupper - blower
  zz <- if (var > 0) (stat - mean) / sqrt(var) else NaN
  list(statistic = stat, tupper = tupper, blower = blower, var = var,
       var_book = vb, z = zz, p_value = 2 * (1 - stats::pnorm(abs(zz))),
       S = S, R = R, m = m, n = n)
}

#' Covariance of two linear rank statistics -- Theorem 7.3.3, p. 279
#' @noRd
Lrankcov <- function(a, b, m, n) {
  av <- as.numeric(a); bv <- as.numeric(b)
  m <- as.integer(m); n <- as.integer(n); nn <- m + n
  if (length(av) != nn || length(bv) != nn)
    stop("score vectors must have length m + n.", call. = FALSE)
  if (nn < 2L) stop("N must be at least 2.", call. = FALSE)
  k <- m * n / (nn^2 * (nn - 1))
  cv <- k * (nn * sum(av * bv) - sum(av) * sum(bv))
  va <- k * (nn * sum(av^2) - sum(av)^2)
  vb <- k * (nn * sum(bv^2) - sum(bv)^2)
  list(cov = cv, corr = if (va > 0 && vb > 0) cv / sqrt(va * vb) else NaN,
       var_a = va, var_b = vb, N = nn, m = m, n = n)
}

#' Linear rank statistic properties -- Theorem 7.3.7, p. 283
#' @noRd
Lrankprop <- function(a, z) {
  av <- as.numeric(a); zv <- as.numeric(z); nn <- length(av)
  if (length(zv) != nn) stop("a and z must have the same length.", call. = FALSE)
  if (nn < 1L) stop("a must be non-empty.", call. = FALSE)
  t <- sum(av * zv)
  trev <- sum(av * rev(zv))
  tcon <- sum(av * (1 - zv))
  pal <- all(abs(av - rev(av)) < 1e-12)
  list(t = t, t_reversed = trev, t_conjugate = tcon, sum_a = sum(av),
       palindromic = as.integer(pal), resid1 = t - trev,
       resid2 = t + tcon - sum(av), N = nn)
}

#' Chernoff-Savage null moments -- Theorem 7.3.8 / Corollary 7.3.1, p. 285
#' @noRd
Lrankasymp <- function(j, jprime, lam, n, nodes = 2001) {
  lam <- as.numeric(lam); n <- as.integer(n); nodes <- as.integer(nodes)
  if (lam <= 0 || lam >= 1)
    stop("lam must lie strictly inside (0, 1).", call. = FALSE)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  if (nodes < 3L) stop("nodes must be at least 3.", call. = FALSE)
  h <- 1 / (nodes - 1); us <- (0:(nodes - 1)) * h; eps <- 1e-9
  uu <- pmin(1 - eps, pmax(eps, us))
  jv <- vapply(uu, function(u) as.numeric(j(u)), 0)
  jp <- vapply(uu, function(u) as.numeric(jprime(u)), 0)
  mu <- sum(0.5 * h * (jv[-nodes] + jv[-1]))
  incr <- 0.5 * h * (us[-nodes] * jp[-nodes] + us[-1] * jp[-1])
  cum <- c(0, cumsum(incr))
  f <- (1 - us) * jp * cum
  integ <- sum(0.5 * h * (f[-nodes] + f[-1]))
  var <- 2 * (1 - lam) * integ / (n * lam)
  list(mean = mu, var = var, sd = if (var > 0) sqrt(var) else NaN,
       integral = integ, lam = lam, n = n)
}

#' Rank-test inversion interval -- Secs. 5.7.5, 6.4.2, 6.6.2
#' @noRd
Rankci <- function(values, k, level = NULL) {
  v <- sort(as.numeric(values)); mm <- length(v); k <- as.integer(k)
  if (mm < 2L) stop("need at least 2 values.", call. = FALSE)
  if (k < 0L || k >= mm) stop("k must lie in 0..M-1.", call. = FALSE)
  list(lower = v[k + 1L], upper = v[mm - k], estimate = stats::median(v),
       k = k, m = mm, level = if (is.null(level)) NaN else as.numeric(level))
}

#' Pooled indicator vector: 1 where the i-th smallest is an X
#' @noRd
.gbTagged <- function(xs, ys) {
  lab <- c(rep(0L, length(xs)), rep(1L, length(ys)))
  o <- order(c(xs, ys), lab)
  as.numeric(lab[o] == 0L)
}

#' Theorem 7.3.2 moments of sum a_i Z_i under H0
#' @noRd
.gbLrMoments <- function(a, m, n) {
  nn <- m + n; abar <- mean(a); ss <- sum((a - abar)^2)
  list(mean = m * abar, var = m * n * ss / (nn * (nn - 1)))
}

#' Mood scale test -- eqs. (9.2.1)-(9.2.3), pp. 314-316
#' @noRd
Moodscale <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  z <- .gbTagged(xs, ys)
  a <- (seq_len(nn) - (nn + 1) / 2)^2
  stat <- sum(a * z)
  mean <- m * (nn^2 - 1) / 12
  var <- m * n * (nn + 1) * (nn^2 - 4) / 180
  vg <- .gbLrMoments(a, m, n)$var
  zz <- if (var > 0) (stat - mean) / sqrt(var) else NaN
  list(statistic = stat, mean = mean, var = var, var_general = vg, z = zz,
       p_value = 2 * (1 - stats::pnorm(abs(zz))), m = m, n = n)
}

#' Mood null moments -- eqs. (9.2.2)-(9.2.3), pp. 315-316
#' @noRd
Moodmom <- function(m, n) {
  m <- as.integer(m); n <- as.integer(n)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  nn <- m + n
  mean <- m * (nn^2 - 1) / 12
  var <- m * n * (nn + 1) * (nn^2 - 4) / 180
  list(mean = mean, var = var, sd = sqrt(var), N = nn, m = m, n = n)
}

#' Freund-Ansari-Bradley-David-Barton test -- eq. (9.3.1), p. 316
#' @noRd
Ansbrad <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  z <- .gbTagged(xs, ys)
  a <- abs(seq_len(nn) - (nn + 1) / 2)
  stat <- sum(a * z)
  mv <- .gbLrMoments(a, m, n)
  zz <- if (mv$var > 0) (stat - mv$mean) / sqrt(mv$var) else NaN
  list(statistic = stat, mean = mv$mean, var = mv$var, z = zz,
       p_value = 2 * (1 - stats::pnorm(abs(zz))), scores = a, m = m, n = n)
}

#' Siegel-Tukey scale test -- Sec. 9.4, p. 320
#' @noRd
Sgltukey <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  if (length(xs) < 1L || length(ys) < 1L)
    stop("both samples must be non-empty.", call. = FALSE)
  lab <- c(rep(0L, length(xs)), rep(1L, length(ys)))
  o <- order(c(xs, ys), lab); tag <- lab[o]
  dropped <- 0L
  if (length(tag) %% 2L == 1L) {
    tag <- tag[-((length(tag) %/% 2L) + 1L)]; dropped <- 1L
  }
  nn <- length(tag); m <- sum(tag == 0L); n <- nn - m
  if (m < 1L || n < 1L)
    stop("dropping the middle value emptied a sample.", call. = FALSE)
  a <- numeric(nn); lo <- 1L; hi <- nn; nxt <- 1
  repeat {
    if (lo > hi) break
    a[lo] <- nxt; nxt <- nxt + 1
    if (lo == hi) break
    a[hi] <- nxt; nxt <- nxt + 1; hi <- hi - 1L
    if (lo + 1L > hi) break
    a[hi] <- nxt; nxt <- nxt + 1; hi <- hi - 1L; lo <- lo + 1L
    if (lo > hi) break
    a[lo] <- nxt; nxt <- nxt + 1; lo <- lo + 1L
  }
  stat <- sum(a[tag == 0L])
  mv <- .gbLrMoments(a, m, n)
  zz <- if (mv$var > 0) (stat - mv$mean) / sqrt(mv$var) else NaN
  list(statistic = stat, mean = mv$mean, var = mv$var, z = zz,
       p_value = 2 * (1 - stats::pnorm(abs(zz))), scores = a,
       dropped = dropped, m = m, n = n)
}

#' Klotz normal-scores scale test -- eq. (9.5.1), p. 322
#' @noRd
Klotzsc <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  nn <- m + n
  z <- .gbTagged(xs, ys)
  a <- stats::qnorm(seq_len(nn) / (nn + 1))^2
  stat <- sum(a * z)
  mv <- .gbLrMoments(a, m, n)
  zz <- if (mv$var > 0) (stat - mv$mean) / sqrt(mv$var) else NaN
  list(statistic = stat, mean = mv$mean, var = mv$var, z = zz,
       p_value = 2 * (1 - stats::pnorm(abs(zz))), scores = a, m = m, n = n)
}

#' Percentile modified rank test for scale -- Sec. 9.6, p. 323
#' @noRd
Pctranksc <- function(x, y, s = 0.5, r = NULL) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  s <- as.numeric(s); r <- if (is.null(r)) s else as.numeric(r)
  if (s <= 0 || s > 1 || r <= 0 || r > 1)
    stop("s and r must lie in (0, 1].", call. = FALSE)
  nn <- m + n
  z <- .gbTagged(xs, ys)
  S <- min(as.integer(floor(nn * s)) + 1L, nn)
  R <- min(as.integer(floor(nn * r)) + 1L, nn)
  half <- if (nn %% 2 == 0) 0.5 else 0
  a <- numeric(nn)
  for (i in seq_len(R)) a[i] <- a[i] + (R - i + 1 - half)
  for (i in (nn - S + 1L):nn) a[i] <- a[i] + (i - (nn - S) - half)
  blower <- sum(vapply(seq_len(R), function(i) (R - i + 1 - half) * z[i], 0))
  tupper <- sum(vapply((nn - S + 1L):nn,
                       function(i) (i - (nn - S) - half) * z[i], 0))
  abar <- mean(a); ss <- sum((a - abar)^2)
  mean <- m * abar; var <- m * n * ss / (nn * (nn - 1))
  mb <- NaN; vb <- NaN
  if (nn %% 2 == 0 && S == R) {
    mb <- m * S^2 / nn
    vb <- m * n * S * (4 * nn * S^2 - nn - 6 * S^3) / (6 * nn^2 * (nn - 1))
  }
  stat <- tupper + blower
  zz <- if (var > 0) (stat - mean) / sqrt(var) else NaN
  list(statistic = stat, tupper = tupper, blower = blower, mean = mean,
       var = var, mean_book = mb, var_book = vb, z = zz,
       p_value = 2 * (1 - stats::pnorm(abs(zz))), S = S, R = R, m = m, n = n)
}

#' Sukhatme scale test -- eq. (9.7.1), p. 323
#' @noRd
Sukhatme <- function(x, y, alternative = "two-sided") {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  t <- 0L
  for (xi in xs) for (yj in ys) {
    if ((yj < xi && xi < 0) || (0 < xi && xi < yj)) t <- t + 1L
  }
  nn <- m + n
  mean <- m * n / 4; var <- m * n * (nn + 7) / 48
  z <- (t - mean) / sqrt(var)
  pv <- switch(alternative,
    "less" = stats::pnorm(z), "greater" = 1 - stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, less or greater.", call. = FALSE))
  list(statistic = t, mean = mean, var = var, z = z, p_value = min(1, pv),
       phat = t / (m * n), m = m, n = n)
}

#' Scale-ratio CI from Sukhatme -- eqs. (9.8.1)-(9.8.2), p. 328
#' @noRd
Scaleci <- function(x, y, alpha = 0.05, k = NULL) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys); alpha <- as.numeric(alpha)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  rr <- as.numeric(outer(xs, ys, "/"))
  rr <- sort(rr[is.finite(rr) & rr > 0])
  npos <- length(rr)
  if (npos < 2L) stop("need at least 2 positive ratios x_i / y_j.", call. = FALSE)
  nn <- m + n
  za <- stats::qnorm(1 - alpha / 2)
  kraw <- m * n / 4 + 0.5 - za * sqrt(m * n * (nn + 7) / 48)
  kk <- if (is.null(k)) as.integer(floor(kraw)) else as.integer(k)
  kk <- max(1L, min(npos, kk))
  kp <- as.integer(m * n / 2 - kk + 1)
  kp <- max(1L, min(npos, kp))
  lo <- min(kk, kp); hi <- max(kk, kp)
  list(lower = rr[lo], upper = rr[hi], k = kk, kprime = kp, k_raw = kraw,
       npos = npos, estimate = stats::median(rr), m = m, n = n)
}

#' Westenberg interquartile scale test -- eq. (9.9.1), p. 329
#' @noRd
Wstnbrg <- function(x, y) {
  xs <- as.numeric(x); ys <- sort(as.numeric(y))
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 2L) stop("need m >= 1 and n >= 2.", call. = FALSE)
  nn <- m + n
  qf <- function(p) {
    h <- (n - 1) * p; lo <- floor(h); hi <- min(lo + 1, n - 1)
    ys[lo + 1] + (h - lo) * (ys[hi + 1] - ys[lo + 1])
  }
  q1 <- qf(0.25); q3 <- qf(0.75)
  u <- sum(xs >= q1 & xs <= q3)
  half <- nn %/% 2L; den <- choose(nn, half); kk <- 0:m
  pmf <- ifelse(half - kk >= 0 & half - kk <= n,
                choose(m, kk) * choose(n, half - kk) / den, 0)
  list(statistic = u, p_value = min(1, sum(pmf[1:(u + 1L)])), pmf = pmf,
       q1 = q1, q3 = q3, mean = m * half / nn,
       var = m * n * half * (nn - half) / (nn^2 * (nn - 1)), m = m, n = n)
}

#' Rosenbaum outside-extremes scale test -- eq. (9.9.2), p. 329
#' @noRd
Rosenbm <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  m <- length(xs); n <- length(ys)
  if (m < 1L || n < 2L) stop("need m >= 1 and n >= 2.", call. = FALSE)
  ymin <- min(ys); ymax <- max(ys)
  r <- sum(xs < ymin | xs > ymax)
  kk <- 0:m
  pmf <- n * (n - 1) * choose(m, kk) * beta(m + n - 1 - kk, kk + 2)
  list(statistic = r, p_value = min(1, sum(pmf[(r + 1L):(m + 1L)])),
       pmf = pmf, ymin = ymin, ymax = ymax, mean = sum(kk * pmf),
       m = m, n = n)
}

#' k-sample median test -- Sec. 10.2, pp. 344-346
#' @noRd
Kmedtest <- function(samples) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  if (any(vapply(ss, length, 0L) < 1L))
    stop("every sample must be non-empty.", call. = FALSE)
  pooled <- sort(unlist(ss)); nn <- length(pooled)
  d <- if (nn %% 2 == 1) pooled[(nn %/% 2) + 1] else
    (pooled[nn %/% 2] + pooled[(nn %/% 2) + 1]) / 2
  u <- vapply(ss, function(s) sum(s < d), 0)
  t <- sum(u); ns <- vapply(ss, length, 0L)
  if (t == 0 || t == nn) stop("no split at the combined median.", call. = FALSE)
  q <- (nn^2 / (t * (nn - t))) * sum((u - ns * t / nn)^2 / ns)
  prob <- prod(choose(ns, u)) / choose(nn, t)
  list(statistic = q, df = k - 1L,
       p_value = stats::pchisq(q, k - 1L, lower.tail = FALSE),
       u = u, t = t, median = d, prob = prob, k = k, n = nn)
}

#' k-sample control median test -- Sec. 10.3, eq. (10.3.1), pp. 350-351
#' @noRd
Kctrlmed <- function(samples, p = c(0.5)) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  ps <- sort(as.numeric(p))
  if (!length(ps) || any(ps <= 0 | ps >= 1))
    stop("p must lie strictly inside (0, 1).", call. = FALSE)
  ctrl <- sort(ss[[1]]); n1 <- length(ctrl)
  r <- as.integer(floor(n1 * ps)) + 1L
  if (any(r > n1))
    stop("a quantile index exceeds the control sample size.", call. = FALSE)
  cuts <- ctrl[r]; q <- length(cuts)
  counts <- lapply(ss[-1], function(s) {
    row <- integer(q + 1L)
    for (v in s) {
      j <- 1L
      while (j <= q && v > cuts[j]) j <- j + 1L
      row[j] <- row[j] + 1L
    }
    row
  })
  edges <- c(0L, r, n1 + 1L)
  pcell <- (edges[-1] - edges[-length(edges)]) / (n1 + 1)
  stat <- 0
  for (row in counts) {
    ni <- sum(row)
    e <- ni * pcell
    stat <- stat + sum(ifelse(e > 0, (row - e)^2 / e, 0))
  }
  df <- (k - 1L) * q
  list(counts = counts, cuts = cuts, r = r, pcell = pcell, statistic = stat,
       df = df,
       p_value = if (df > 0) stats::pchisq(stat, df, lower.tail = FALSE) else 1,
       k = k)
}

#' Control-vector covariance -- Theorem 10.7.1, p. 375
#' @noRd
Kctrlasymp <- function(lam, dens, pval) {
  lam <- as.numeric(lam); dens <- as.numeric(dens); fv <- as.numeric(pval)
  k <- length(lam)
  if (k < 2L) stop("need at least 2 populations.", call. = FALSE)
  if (length(dens) != k || length(fv) != k)
    stop("lam, dens and pval must have equal length k.", call. = FALSE)
  if (any(lam <= 0 | lam >= 1))
    stop("every lambda must lie strictly inside (0, 1).", call. = FALSE)
  if (any(dens <= 0)) stop("densities must be strictly positive.", call. = FALSE)
  p <- fv[1]; qs <- dens[-1] / dens[1]
  sig <- outer(qs, qs) * p * (1 - p) / lam[1]
  diag(sig) <- diag(sig) + fv[-1] * (1 - fv[-1]) / lam[-1]
  list(sigma = sig, q = qs, p = p, k = k)
}

#' Kruskal-Wallis H -- eqs. (10.4.2)/(10.4.5)/(10.4.7), pp. 354-358
#' @noRd
Kwh <- function(samples, correct = TRUE) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  ns <- vapply(ss, length, 0L)
  if (any(ns < 1L)) stop("every sample must be non-empty.", call. = FALSE)
  v <- unlist(ss); grp <- rep(seq_len(k), ns); nn <- length(v)
  rk <- rank(v, ties.method = "average")
  rs <- vapply(seq_len(k), function(i) sum(rk[grp == i]), 0)
  h <- 12 / (nn * (nn + 1)) * sum(rs^2 / ns) - 3 * (nn + 1)
  tb <- as.numeric(table(v)); tb <- tb[tb > 1]
  corr <- 1
  if (correct && length(tb)) corr <- 1 - sum(tb * (tb^2 - 1)) / (nn * (nn^2 - 1))
  hc <- if (corr > 0) h / corr else NaN
  list(statistic = hc, h_raw = h, correction = corr, df = k - 1L,
       p_value = stats::pchisq(hc, k - 1L, lower.tail = FALSE),
       rank_sums = rs, rank_means = rs / ns, k = k, n = nn)
}

#' Kruskal-Wallis defining form -- eqs. (10.4.2)/(10.4.7), pp. 354, 357
#' @noRd
Kwalt <- function(rank_sums, ns) {
  rs <- as.numeric(rank_sums); nv <- as.integer(ns); k <- length(rs)
  if (k < 2L || length(nv) != k)
    stop("need at least 2 samples and matching sizes.", call. = FALSE)
  if (any(nv < 1L)) stop("sample sizes must be at least 1.", call. = FALSE)
  nn <- sum(nv)
  h1 <- 12 / (nn * (nn + 1)) * sum((rs - nv * (nn + 1) / 2)^2 / nv)
  h2 <- 12 / (nn * (nn + 1)) * sum(rs^2 / nv) - 3 * (nn + 1)
  list(statistic = h1, h_computing = h2, resid = h1 - h2, df = k - 1L,
       k = k, n = nn)
}

#' Chi-square approximation to H -- Sec. 10.4.1, p. 357
#' @noRd
Kwchi <- function(h, k, ns = NULL) {
  h <- as.numeric(h); k <- as.integer(k)
  if (k < 2L) stop("k must be at least 2.", call. = FALSE)
  df <- k - 1L; flag <- 0L
  if (!is.null(ns)) {
    nv <- as.integer(ns)
    if (k == 3L && all(nv <= 5L)) flag <- 1L
  }
  list(statistic = h, df = df,
       p_value = stats::pchisq(h, df, lower.tail = FALSE),
       table_k = flag, k = k)
}

#' Kruskal-Wallis multiple comparisons -- eq. (10.4.8), p. 357
#' @noRd
Kwmc <- function(rank_means, ns, alpha = 0.20) {
  rm <- as.numeric(rank_means); nv <- as.integer(ns)
  k <- length(rm); alpha <- as.numeric(alpha)
  if (k < 2L || length(nv) != k)
    stop("need at least 2 samples and matching sizes.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  nn <- sum(nv)
  zstar <- stats::qnorm(1 - alpha / (k * (k - 1)))
  bounds <- matrix(0, k, k); diffs <- matrix(0, k, k); sig <- list()
  for (i in seq_len(k)) for (j in seq_len(k)) {
    b <- zstar * sqrt(nn * (nn + 1) / 12 * (1 / nv[i] + 1 / nv[j]))
    d <- abs(rm[i] - rm[j])
    bounds[i, j] <- b; diffs[i, j] <- d
    if (i < j && d >= b) sig[[length(sig) + 1L]] <- c(i - 1L, j - 1L)
  }
  eq <- if (length(unique(nv)) == 1L) zstar * sqrt(k * (nn + 1) / 6) else NaN
  list(bound = eq, bounds = bounds, diffs = diffs, significant = sig,
       zstar = zstar, k = k, n = nn)
}

#' General k-sample rank statistic -- eqs. (10.5.1)-(10.5.2), pp. 362-363
#' @noRd
Krankstat <- function(samples, scores = NULL) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  ns <- vapply(ss, length, 0L)
  v <- unlist(ss); grp <- rep(seq_len(k), ns); nn <- length(v)
  o <- order(v); gs <- grp[o]
  a <- if (is.null(scores)) as.numeric(seq_len(nn)) else as.numeric(scores)
  if (length(a) != nn) stop("scores must have length N.", call. = FALSE)
  abar <- mean(a)
  sums <- vapply(seq_len(k), function(i) sum(a[gs == i]), 0)
  q <- sum((sums - ns * abar)^2 / ns)
  ssq <- sum((a - abar)^2)
  stat <- if (ssq > 0) (nn - 1) * q / ssq else NaN
  list(q = q, statistic = stat, df = k - 1L,
       p_value = stats::pchisq(stat, k - 1L, lower.tail = FALSE),
       abar = abar, score_sums = sums, k = k, n = nn)
}

#' Jonckheere-Terpstra B -- Sec. 10.6, eqs. (10.6.2)-(10.6.3), p. 365
#' @noRd
Jtstat <- function(samples, alternative = "greater") {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  ns <- vapply(ss, length, 0L)
  if (any(ns < 1L)) stop("every sample must be non-empty.", call. = FALSE)
  b <- 0
  for (i in seq_len(k - 1L)) for (j in (i + 1L):k) {
    b <- b + sum(outer(ss[[i]], ss[[j]], "<")) +
      0.5 * sum(outer(ss[[i]], ss[[j]], "=="))
  }
  nn <- sum(ns)
  mean <- (nn^2 - sum(ns^2)) / 4
  var <- (nn^2 * (2 * nn + 3) - sum(ns^2 * (2 * ns + 3))) / 72
  z <- (b - mean) / sqrt(var)
  pv <- switch(alternative,
    "greater" = 1 - stats::pnorm(z), "less" = stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be greater, less or two-sided.", call. = FALSE))
  list(statistic = b, mean = mean, var = var, z = z, p_value = min(1, pv),
       k = k, n = nn)
}

#' JT null moments -- eqs. (10.6.2)-(10.6.3), pp. 365-366
#' @noRd
Jtmom <- function(ns) {
  nv <- as.integer(ns); k <- length(nv)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  if (any(nv < 1L)) stop("sample sizes must be at least 1.", call. = FALSE)
  nn <- sum(nv)
  mean <- (nn^2 - sum(nv^2)) / 4
  pair <- 0
  for (i in seq_len(k - 1L)) for (j in (i + 1L):k) pair <- pair + nv[i] * nv[j] / 2
  var <- (nn^2 * (2 * nn + 3) - sum(nv^2 * (2 * nv + 3))) / 72
  list(mean = mean, mean_pairwise = pair, var = var, sd = sqrt(var),
       k = k, n = nn)
}

#' JT as the pairwise U matrix -- Sec. 10.6, eq. (10.6.1), p. 365
#' @noRd
Jtsum <- function(samples) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  u <- matrix(0, k, k)
  for (i in seq_len(k - 1L)) for (j in (i + 1L):k) {
    u[i, j] <- sum(outer(ss[[i]], ss[[j]], "<")) +
      0.5 * sum(outer(ss[[i]], ss[[j]], "=="))
  }
  list(u = u, statistic = sum(u[upper.tri(u)]),
       npairs = as.integer(k * (k - 1) / 2), k = k,
       n = sum(vapply(ss, length, 0L)))
}

#' Treatments-vs-control precedence test -- eq. (10.7.3), p. 373
#' @noRd
Ctrltree <- function(samples, r = NULL) {
  ss <- lapply(samples, as.numeric); k <- length(ss)
  if (k < 2L) stop("need at least 2 samples.", call. = FALSE)
  ctrl <- sort(ss[[1]]); n1 <- length(ctrl)
  if (n1 < 1L) stop("the control sample must be non-empty.", call. = FALSE)
  rr <- if (is.null(r)) (n1 %/% 2L) + 1L else as.integer(r)
  if (rr < 1L || rr > n1) stop("r must lie in 1..n1.", call. = FALSE)
  t <- ctrl[rr]
  treat <- unlist(ss[-1]); mt <- length(treat)
  if (mt < 1L) stop("need at least one treatment observation.", call. = FALSE)
  w <- sum(treat < t)
  den <- choose(n1 + mt, mt); j <- 0:mt
  pmf <- choose(n1 + mt - rr - j, mt - j) * choose(rr + j - 1, j) / den
  list(statistic = w, p_value = min(1, sum(pmf[1:(w + 1L)])), pmf = pmf,
       t = t, r = rr, mtreat = mt, mean = sum(j * pmf), k = k)
}

#' Null distribution of S = P - Q -- Sec. 11.2.1, p. 395
#' @noRd
Taunull <- function(n, s = NULL) {
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  maxinv <- (n * (n - 1L)) %/% 2L
  counts <- numeric(maxinv + 1L); counts[1] <- 1
  for (i in 2:n) {
    new <- numeric(maxinv + 1L); run <- 0
    for (k in 0:maxinv) {
      run <- run + counts[k + 1L]
      if (k - i >= 0) run <- run - counts[k - i + 1L]
      new[k + 1L] <- run
    }
    counts <- new
  }
  pmf <- counts / sum(counts)
  support <- maxinv - 2 * (0:maxinv)
  var_tau <- 2 * (2 * n + 5) / (9 * n * (n - 1))
  pmf_s <- NaN; cdf_s <- NaN; sf_s <- NaN
  if (!is.null(s)) {
    sv <- as.integer(s)
    if ((maxinv - sv) %% 2 != 0 || sv < -maxinv || sv > maxinv)
      stop("s is outside the support of S.", call. = FALSE)
    idx <- (maxinv - sv) %/% 2L
    pmf_s <- pmf[idx + 1L]
    cdf_s <- sum(pmf[(idx + 1L):length(pmf)])
    sf_s <- sum(pmf[1:(idx + 1L)])
  }
  list(support = support, pmf = pmf, pmf_s = pmf_s, cdf_s = cdf_s, sf_s = sf_s,
       var_tau = var_tau, var_s = var_tau * maxinv^2, n = n)
}

#' Kendall tau trend test -- Sec. 11.2.5, p. 406
#' @noRd
Tautrend <- function(y, alternative = "two-sided") {
  ys <- as.numeric(y); n <- length(ys)
  if (n < 3L) stop("need at least 3 observations.", call. = FALSE)
  d <- outer(ys, ys, "-")
  p <- sum(d[upper.tri(d)] < 0); q <- sum(d[upper.tri(d)] > 0)
  npairs <- n * (n - 1) / 2
  s <- p - q; tau <- s / npairs
  var <- 2 * (2 * n + 5) / (9 * n * (n - 1))
  z <- tau / sqrt(var)
  pv <- switch(alternative,
    "greater" = 1 - stats::pnorm(z), "less" = stats::pnorm(z),
    "two-sided" = 2 * (1 - stats::pnorm(abs(z))),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(tau = tau, statistic = s, P = p, Q = q, z = z,
       p_value = min(1, pv), var = var, n = n)
}

#' Spearman rank correlation -- Sec. 11.3, eq. (11.3.2), p. 407
#' @noRd
Spearrho <- function(x, y) {
  xs <- as.numeric(x); ys <- as.numeric(y); n <- length(xs)
  if (length(ys) != n) stop("x and y must have the same length.", call. = FALSE)
  if (n < 3L) stop("need at least 3 pairs.", call. = FALSE)
  rx <- rank(xs, ties.method = "average"); ry <- rank(ys, ties.method = "average")
  tied <- as.integer(length(unique(xs)) < n || length(unique(ys)) < n)
  d2 <- sum((rx - ry)^2)
  short <- 1 - 6 * d2 / (n * (n^2 - 1))
  full <- stats::cor(rx, ry)
  list(statistic = full, r_shortcut = short, sumd2 = d2, tied = tied,
       var = 1 / (n - 1), n = n)
}

#' Test of zero Spearman correlation -- Secs. 11.3.2-11.3.3, pp. 412-413
#' @noRd
Rhotest <- function(r, n, alternative = "two-sided") {
  r <- as.numeric(r); n <- as.integer(n)
  if (n < 3L) stop("n must be at least 3.", call. = FALSE)
  if (r < -1 || r > 1) stop("r must lie in [-1, 1].", call. = FALSE)
  z <- r * sqrt(n - 1)
  t <- if (abs(r) >= 1) sign(r) * Inf else r * sqrt((n - 2) / (1 - r^2))
  if (alternative == "greater") {
    pn <- 1 - stats::pnorm(z); pt <- stats::pt(t, n - 2, lower.tail = FALSE)
  } else if (alternative == "less") {
    pn <- stats::pnorm(z); pt <- stats::pt(t, n - 2)
  } else if (alternative == "two-sided") {
    pn <- 2 * (1 - stats::pnorm(abs(z)))
    pt <- 2 * stats::pt(abs(t), n - 2, lower.tail = FALSE)
  } else {
    stop("alternative must be two-sided, greater or less.", call. = FALSE)
  }
  list(z = z, p_normal = min(1, pn), t = t, df = n - 2L,
       p_value = min(1, pt), var = 1 / (n - 1), n = n)
}

#' Fieller-Hartley-Pearson normal-scores correlation -- Sec. 11.5, p. 422
#' @noRd
Normcorr <- function(x, y, rho = 0, nodes = 4001) {
  xs <- as.numeric(x); ys <- as.numeric(y); n <- length(xs)
  if (length(ys) != n) stop("x and y must have the same length.", call. = FALSE)
  if (n < 4L) stop("need at least 4 pairs.", call. = FALSE)
  xi <- vapply(seq_len(n), function(i) .gbEnos(i, n, nodes = nodes), 0)
  ox <- order(xs); ry <- rank(ys, ties.method = "first")
  num <- sum(xi * xi[ry[ox]])
  den <- sum(xi^2)
  rf <- num / den
  rf <- min(1 - 1e-15, max(-1 + 1e-15, rf))
  zf <- atanh(rf)
  mz <- atanh(as.numeric(rho)) * (1 - 0.6 / (n + 8))
  vz <- 1 / (n - 3)
  z <- (zf - mz) / sqrt(vz)
  list(statistic = rf, zf = zf, mean_zf = mz, var_zf = vz, z = z,
       p_value = 2 * (1 - stats::pnorm(abs(z))), scores = xi, n = n)
}

#' Kendall partial tau -- Sec. 12.6, eq. (12.6.1), p. 467
#' @noRd
Taupartial <- function(x, y, z) {
  xs <- as.numeric(x); ys <- as.numeric(y); zs <- as.numeric(z); n <- length(xs)
  if (length(ys) != n || length(zs) != n)
    stop("x, y and z must have the same length.", call. = FALSE)
  if (n < 3L) stop("need at least 3 subjects.", call. = FALSE)
  x11 <- 0L; x12 <- 0L; x21 <- 0L; x22 <- 0L; dropped <- 0L
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    sx <- sign(xs[j] - xs[i]); sy <- sign(ys[j] - ys[i]); sz <- sign(zs[j] - zs[i])
    if (sx == 0 || sy == 0 || sz == 0) { dropped <- dropped + 1L; next }
    xc <- sx * sz > 0; yc <- sy * sz > 0
    if (yc && xc) x11 <- x11 + 1L
    else if (yc && !xc) x12 <- x12 + 1L
    else if (!yc && xc) x21 <- x21 + 1L
    else x22 <- x22 + 1L
  }
  c1 <- x11 + x21; c2 <- x12 + x22; r1 <- x11 + x12; r2 <- x21 + x22
  den <- c1 * c2 * r1 * r2
  stat <- if (den > 0) (x11 * x22 - x12 * x21) / sqrt(den) else NaN
  list(statistic = stat, x11 = x11, x12 = x12, x21 = x21, x22 = x22,
       dropped = dropped, npairs = as.integer(n * (n - 1) / 2), n = n)
}

#' Within-block midranks and tie sum for the Friedman family
#' @noRd
.gbFriedRanks <- function(rows) {
  k <- length(rows); n <- length(rows[[1]])
  rsum <- numeric(n); tiesum <- 0
  for (r in rows) {
    if (length(r) != n) stop("every block must have n observations.", call. = FALSE)
    rk <- rank(r, ties.method = "average")
    tb <- as.numeric(table(r)); tb <- tb[tb > 1]
    if (length(tb)) tiesum <- tiesum + sum(tb * (tb^2 - 1))
    rsum <- rsum + rk
  }
  list(rsum = rsum, tiesum = tiesum, k = k, n = n)
}

#' Friedman two-way ANOVA by ranks -- eqs. (12.2.8)/(12.2.12), pp. 441-445
#' @noRd
Friedq <- function(data, correct = TRUE) {
  rows <- lapply(data, as.numeric); k <- length(rows)
  if (k < 2L) stop("need at least 2 blocks.", call. = FALSE)
  n <- length(rows[[1]])
  if (n < 2L) stop("need at least 2 treatments.", call. = FALSE)
  fr <- .gbFriedRanks(rows); rsum <- fr$rsum; tiesum <- fr$tiesum
  q <- 12 / (k * n * (n + 1)) * sum(rsum^2) - 3 * k * (n + 1)
  s <- sum((rsum - k * (n + 1) / 2)^2)
  qc <- q
  if (correct && tiesum > 0) qc <- 12 * (n - 1) * s / (k * n * (n^2 - 1) - tiesum)
  list(statistic = qc, q_raw = q, s = s, df = n - 1L,
       p_value = stats::pchisq(qc, n - 1L, lower.tail = FALSE),
       rank_sums = rsum, k = k, n = n)
}

#' Tie-corrected Friedman Q -- eq. (12.2.12), p. 445
#' @noRd
Friedties <- function(data) {
  rows <- lapply(data, as.numeric); k <- length(rows)
  if (k < 2L) stop("need at least 2 blocks.", call. = FALSE)
  n <- length(rows[[1]])
  if (n < 2L) stop("need at least 2 treatments.", call. = FALSE)
  fr <- .gbFriedRanks(rows); rsum <- fr$rsum; tiesum <- fr$tiesum
  s <- sum((rsum - k * (n + 1) / 2)^2)
  q0 <- 12 * s / (k * n * (n + 1))
  den <- k * n * (n^2 - 1) - tiesum
  qc <- if (den > 0) 12 * (n - 1) * s / den else NaN
  list(statistic = qc, q_raw = q0, tiesum = tiesum,
       factor = if (q0 != 0) qc / q0 else NaN, s = s, rank_sums = rsum,
       k = k, n = n)
}

#' Chi-square approximation to Friedman Q -- Sec. 12.2, p. 442
#' @noRd
Friedchi <- function(q, k, n) {
  q <- as.numeric(q); k <- as.integer(k); n <- as.integer(n)
  if (k < 2L || n < 2L)
    stop("need k >= 2 blocks and n >= 2 treatments.", call. = FALSE)
  df <- n - 1L
  ve <- 2 * (n - 1) * (k - 1) / k; vc <- 2 * (n - 1)
  list(statistic = q, df = df,
       p_value = stats::pchisq(q, df, lower.tail = FALSE),
       mean = n - 1, var_exact = ve, var_chi2 = vc, ratio = ve / vc,
       k = k, n = n)
}

#' Friedman S and Q moments -- eq. (12.2.7), p. 442
#' @noRd
Friedvar <- function(k, n) {
  k <- as.integer(k); n <- as.integer(n)
  if (k < 2L || n < 2L)
    stop("need k >= 2 blocks and n >= 2 treatments.", call. = FALSE)
  ms <- k * n * (n^2 - 1) / 12
  vs <- n^2 * k * (k - 1) * (n + 1)^2 / 72
  vq <- 2 * (n - 1) * (k - 1) / k; vc <- 2 * (n - 1)
  list(mean_s = ms, var_s = vs, mean_q = n - 1, var_q = vq, var_chi2 = vc,
       deficit = vc - vq, k = k, n = n)
}

#' Friedman multiple comparisons -- eq. (12.2.13), p. 445
#' @noRd
Friedmc <- function(rank_sums, k, alpha = 0.20) {
  rs <- as.numeric(rank_sums); n <- length(rs)
  k <- as.integer(k); alpha <- as.numeric(alpha)
  if (n < 2L) stop("need at least 2 treatments.", call. = FALSE)
  if (k < 2L) stop("need at least 2 blocks.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  zstar <- stats::qnorm(1 - alpha / (n * (n - 1)))
  bound <- zstar * sqrt(k * n * (n + 1) / 6)
  diffs <- matrix(0, n, n); sig <- list()
  for (i in seq_len(n)) for (j in seq_len(n)) {
    d <- abs(rs[i] - rs[j]); diffs[i, j] <- d
    if (i < j && d >= bound) sig[[length(sig) + 1L]] <- c(i - 1L, j - 1L)
  }
  list(bound = bound, zstar = zstar, diffs = diffs, significant = sig,
       k = k, n = n)
}

#' Page's L test -- eqs. (12.3.1)-(12.3.2), pp. 448-449
#' @noRd
Pagel <- function(data, weights = NULL) {
  rows <- lapply(data, as.numeric); k <- length(rows)
  if (k < 2L) stop("need at least 2 blocks.", call. = FALSE)
  n <- length(rows[[1]])
  if (n < 2L) stop("need at least 2 treatments.", call. = FALSE)
  w <- if (is.null(weights)) as.numeric(seq_len(n)) else as.numeric(weights)
  if (length(w) != n) stop("weights must have length n.", call. = FALSE)
  rsum <- .gbFriedRanks(rows)$rsum
  ell <- sum(w * rsum)
  z <- (12 * (ell - 0.5) - 3 * k * n * (n + 1)^2) /
    (n * (n + 1) * sqrt(k * (n - 1)))
  rav <- 12 * ell / (k * (n^3 - n)) - 3 * (n + 1) / (n - 1)
  list(statistic = ell, z = z, p_value = 1 - stats::pnorm(z), rav = rav,
       rank_sums = rsum, k = k, n = n)
}

#' Exact null distribution of Page's L -- Sec. 12.3, p. 448
#' @noRd
Pageexact <- function(k, n, ell = NULL) {
  k <- as.integer(k); n <- as.integer(n)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  if (n < 2L || n > 8L)
    stop("n must lie in 2..8 for exact enumeration.", call. = FALSE)
  lo <- sum(seq_len(n) * rev(seq_len(n)))
  hi <- sum(seq_len(n)^2)
  span <- hi - lo
  block <- numeric(span + 1L)
  rec <- function(rem, acc, pos) {
    if (pos == n) { block[acc - lo + 1L] <<- block[acc - lo + 1L] + 1; return(invisible()) }
    for (v in rem) rec(rem[rem != v], acc + (pos + 1L) * v, pos + 1L)
  }
  rec(seq_len(n), 0L, 0L)
  block <- block / sum(block)
  cur <- 1
  for (i in seq_len(k)) {
    new <- numeric(length(cur) + span)
    for (a in seq_along(cur)) {
      if (cur[a] == 0) next
      for (b in seq_along(block)) new[a + b - 1L] <- new[a + b - 1L] + cur[a] * block[b]
    }
    cur <- new
  }
  support <- lo * k + (0:(length(cur) - 1L))
  mu <- sum(support * cur); e2 <- sum(support^2 * cur)
  pmf_l <- NaN; sf_l <- NaN
  if (!is.null(ell)) {
    li <- as.integer(round(as.numeric(ell))) - lo * k
    if (li >= 0L && li < length(cur)) {
      pmf_l <- cur[li + 1L]; sf_l <- sum(cur[(li + 1L):length(cur)])
    } else {
      pmf_l <- 0; sf_l <- if (li >= length(cur)) 0 else 1
    }
  }
  list(support = support, pmf = cur, pmf_l = pmf_l, sf_l = sf_l,
       mean = mu, var = e2 - mu^2, k = k, n = n)
}

#' Page's L normal approximation -- eq. (12.3.2), p. 449
#' @noRd
Pageasymp <- function(ell, k, n, correct = TRUE) {
  ell <- as.numeric(ell); k <- as.integer(k); n <- as.integer(n)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  e <- if (correct) ell - 0.5 else ell
  z <- (12 * e - 3 * k * n * (n + 1)^2) / (n * (n + 1) * sqrt(k * (n - 1)))
  list(z = z, p_value = 1 - stats::pnorm(z),
       mean = k * n * (n + 1)^2 / 4,
       var = k * n^2 * (n + 1)^2 * (n - 1) / 144,
       statistic = ell, k = k, n = n)
}

#' Concordance W significance test -- Sec. 12.4.2, p. 455
#' @noRd
Wsignif <- function(w, k, n) {
  w <- as.numeric(w); k <- as.integer(k); n <- as.integer(n)
  if (w < 0 || w > 1) stop("w must lie in [0, 1].", call. = FALSE)
  if (k < 2L || n < 2L)
    stop("need k >= 2 rankings of n >= 2 objects.", call. = FALSE)
  q <- k * (n - 1) * w
  list(statistic = q, w = w, df = n - 1L,
       p_value = stats::pchisq(q, n - 1L, lower.tail = FALSE),
       s = q * k * n * (n + 1) / 12,
       table_n = as.integer(k <= 5L || n <= 5L), k = k, n = n)
}

#' ARE from derivatives and variances -- Theorem 13.2.2, eq. (13.2.1), p. 485
#' @noRd
Arepitman <- function(deriv, var, deriv_star, var_star) {
  d <- as.numeric(deriv); v <- as.numeric(var)
  ds <- as.numeric(deriv_star); vs <- as.numeric(var_star)
  if (v <= 0 || vs <= 0) stop("variances must be strictly positive.", call. = FALSE)
  if (ds == 0) stop("the reference derivative must be non-zero.", call. = FALSE)
  e1 <- d^2 / v; e2 <- ds^2 / vs
  list(are = (d / ds)^2 * vs / v, check = e1 / e2, efficacy = e1,
       efficacy_star = e2)
}

#' Efficacy -- eq. (13.2.4), p. 486
#' @noRd
Efficacy <- function(deriv, var) {
  d <- as.numeric(deriv); v <- as.numeric(var)
  if (v <= 0) stop("var must be strictly positive.", call. = FALSE)
  list(efficacy = d^2 / v, deriv = d, var = v)
}

#' Sign test efficacy -- eq. (13.3.3), p. 489
#' @noRd
Effsign <- function(n, fmed) {
  n <- as.integer(n); f <- as.numeric(fmed)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (f <= 0) stop("fmed must be strictly positive.", call. = FALSE)
  e <- 4 * n * f^2
  list(efficacy = e, per_obs = e / n, n = n, fmed = f)
}

#' One-sample t efficacy -- eq. (13.3.2), p. 488
#' @noRd
Efft <- function(n, sigma2) {
  n <- as.integer(n); s2 <- as.numeric(sigma2)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (s2 <= 0) stop("sigma2 must be strictly positive.", call. = FALSE)
  list(efficacy = n / s2, per_obs = 1 / s2, n = n, sigma2 = s2)
}

#' Signed-rank efficacy -- eq. (13.3.4), p. 490
#' @noRd
Effwsr <- function(n, f0, integral) {
  n <- as.integer(n); f0 <- as.numeric(f0); ii <- as.numeric(integral)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  e <- 24 * (f0 / (n - 1) + ii)^2 * n * (n - 1)^2 / ((n + 1) * (2 * n + 1))
  list(efficacy = e, limit = 12 * n * ii^2, integral = ii, n = n)
}

#' Mann-Whitney / rank-sum efficacy -- eq. (13.3.10), p. 494
#' @noRd
Effwrs <- function(m, n, integral) {
  m <- as.integer(m); n <- as.integer(n); ii <- as.numeric(integral)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  list(efficacy = 12 * m * n * ii^2 / (m + n + 1), integral = ii, m = m, n = n)
}

#' Two-sample t efficacy -- eq. (13.3.9), p. 494
#' @noRd
Efft2 <- function(m, n, sigma2) {
  m <- as.integer(m); n <- as.integer(n); s2 <- as.numeric(sigma2)
  if (m < 1L || n < 1L) stop("m and n must be at least 1.", call. = FALSE)
  if (s2 <= 0) stop("sigma2 must be strictly positive.", call. = FALSE)
  list(efficacy = m * n / (s2 * (m + n)), m = m, n = n, sigma2 = s2)
}

#' Chi-square test of independence -- Sec. 14.2, p. 505
#' @noRd
Chiindep <- function(table, correct = FALSE) {
  tb <- as.matrix(table); storage.mode(tb) <- "double"
  r <- nrow(tb); c <- ncol(tb)
  if (r < 2L) stop("need at least 2 rows.", call. = FALSE)
  if (c < 2L) stop("need at least 2 columns.", call. = FALSE)
  rs <- rowSums(tb); cs <- colSums(tb); nn <- sum(rs)
  if (nn <= 0) stop("the table must contain positive counts.", call. = FALSE)
  exp <- outer(rs, cs) / nn
  if (any(exp <= 0)) stop("an expected frequency is zero.", call. = FALSE)
  yates <- correct && r == 2L && c == 2L
  d <- abs(tb - exp)
  if (yates) d <- pmax(0, d - 0.5)
  q <- sum(d^2 / exp)
  df <- (r - 1L) * (c - 1L)
  list(statistic = q, df = df,
       p_value = stats::pchisq(q, df, lower.tail = FALSE),
       expected = exp, n = nn, r = r, c = c)
}

#' k x 2 equal-proportions test -- eq. (14.3.2), p. 514
#' @noRd
Chik2 <- function(successes, ns) {
  y <- as.numeric(successes); nv <- as.numeric(ns); k <- length(y)
  if (k < 2L || length(nv) != k)
    stop("need at least 2 groups and matching sizes.", call. = FALSE)
  if (any(nv <= 0)) stop("group sizes must be positive.", call. = FALSE)
  nn <- sum(nv); ph <- sum(y) / nn
  if (ph <= 0 || ph >= 1)
    stop("the pooled proportion must lie inside (0, 1).", call. = FALSE)
  q <- sum(y^2 / nv) / (ph * (1 - ph)) - nn * ph / (1 - ph)
  df <- k - 1L
  list(statistic = q, df = df,
       p_value = stats::pchisq(q, df, lower.tail = FALSE),
       phat = ph, props = y / nv, k = k, n = nn)
}

#' Hypergeometric point probability for a 2 x 2 table
#' @noRd
.gbHyper <- function(a, r1, r2, c1) {
  nn <- r1 + r2
  if (a < max(0, c1 - r2) || a > min(r1, c1)) return(0)
  choose(r1, a) * choose(r2, c1 - a) / choose(nn, c1)
}

#' Fisher exact test -- Sec. 14.4, p. 517
#' @noRd
Fisherex <- function(table, alternative = "two-sided") {
  tb <- round(as.matrix(table))
  if (nrow(tb) != 2L || ncol(tb) != 2L) stop("table must be 2 x 2.", call. = FALSE)
  a <- tb[1, 1]; b <- tb[1, 2]; cc <- tb[2, 1]; d <- tb[2, 2]
  if (min(a, b, cc, d) < 0) stop("counts must be non-negative.", call. = FALSE)
  r1 <- a + b; r2 <- cc + d; c1 <- a + cc
  lo <- max(0, c1 - r2); hi <- min(r1, c1)
  if (hi < lo) stop("degenerate margins.", call. = FALSE)
  ks <- lo:hi
  probs <- vapply(ks, function(k) .gbHyper(k, r1, r2, c1), 0)
  pobs <- probs[ks == a]
  pg <- sum(probs[ks >= a]); pl <- sum(probs[ks <= a])
  pv <- switch(alternative,
    "greater" = pg, "less" = pl,
    "two-sided" = sum(probs[probs <= pobs * (1 + 1e-12)]),
    stop("alternative must be two-sided, greater or less.", call. = FALSE))
  list(p_value = min(1, pv), p_greater = pg, p_less = pl, prob = pobs,
       statistic = a, support = c(lo, hi))
}

#' One-sided Fisher exact test -- Sec. 14.4, p. 517
#' @noRd
Fisherex1 <- function(table, alternative = "greater") {
  tb <- round(as.matrix(table))
  if (nrow(tb) != 2L || ncol(tb) != 2L) stop("table must be 2 x 2.", call. = FALSE)
  a <- tb[1, 1]; b <- tb[1, 2]; cc <- tb[2, 1]; d <- tb[2, 2]
  if (min(a, b, cc, d) < 0) stop("counts must be non-negative.", call. = FALSE)
  r1 <- a + b; r2 <- cc + d; c1 <- a + cc; nn <- r1 + r2
  ks <- max(0, c1 - r2):min(r1, c1)
  probs <- vapply(ks, function(k) .gbHyper(k, r1, r2, c1), 0)
  pg <- sum(probs[ks >= a]); pl <- sum(probs[ks <= a])
  pv <- switch(alternative, "greater" = pg, "less" = pl,
               stop("alternative must be greater or less.", call. = FALSE))
  list(p_value = min(1, pv), p_greater = pg, p_less = pl,
       prob = probs[ks == a], statistic = a, mean = r1 * c1 / nn)
}

#' McNemar test -- eq. (14.5.1), p. 523
#' @noRd
Mcnemarq <- function(table, correct = FALSE) {
  tb <- as.matrix(table); storage.mode(tb) <- "double"
  if (nrow(tb) != 2L || ncol(tb) != 2L) stop("table must be 2 x 2.", call. = FALSE)
  x12 <- tb[1, 2]; x21 <- tb[2, 1]; nd <- x12 + x21
  if (nd <= 0) stop("there are no discordant pairs.", call. = FALSE)
  d <- abs(x12 - x21)
  if (correct) d <- max(0, d - 1)
  q <- d^2 / nd
  k <- as.integer(round(min(x12, x21))); ni <- as.integer(round(nd))
  pex <- min(1, 2 * sum(choose(ni, 0:k)) * 0.5^ni)
  list(statistic = q, df = 1L,
       p_value = stats::pchisq(q, 1, lower.tail = FALSE), p_exact = pex,
       x12 = x12, x21 = x21, ndisc = nd)
}

#' McNemar CI -- Sec. 14.5, eq. (14.5.2), p. 523
#' @noRd
Mcnemarci <- function(table, alpha = 0.05) {
  tb <- as.matrix(table); storage.mode(tb) <- "double"
  if (nrow(tb) != 2L || ncol(tb) != 2L) stop("table must be 2 x 2.", call. = FALSE)
  alpha <- as.numeric(alpha)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  nn <- sum(tb)
  if (nn <= 0) stop("the table must contain positive counts.", call. = FALSE)
  p12 <- tb[1, 2] / nn; p21 <- tb[2, 1] / nn
  est <- p12 - p21
  se <- sqrt(max(0, (p12 + p21 - est^2) / nn))
  sen <- sqrt((p12 + p21) / nn)
  z <- stats::qnorm(1 - alpha / 2)
  list(estimate = est, lower = est - z * se, upper = est + z * se,
       se = se, se_null = sen, n = nn)
}

#' Multinomial goodness of fit -- Sec. 14.6, p. 528
#' @noRd
Multgof <- function(observed, probs, ddof = 0) {
  o <- as.numeric(observed); p <- as.numeric(probs); k <- length(o)
  if (k < 2L || length(p) != k)
    stop("need at least 2 matching categories.", call. = FALSE)
  if (abs(sum(p) - 1) > 1e-9) stop("probs must sum to 1.", call. = FALSE)
  if (any(p <= 0)) stop("probs must be strictly positive.", call. = FALSE)
  nn <- sum(o); exp <- nn * p
  q <- sum((o - exp)^2 / exp)
  df <- k - 1L - as.integer(ddof)
  if (df < 1L) stop("degrees of freedom must be at least 1.", call. = FALSE)
  ni <- as.integer(round(nn)); ci <- as.integer(round(o))
  lp <- lgamma(ni + 1) + sum(ci * log(p) - lgamma(ci + 1))
  list(statistic = q, df = df,
       p_value = stats::pchisq(q, df, lower.tail = FALSE),
       expected = exp, prob = exp(lp), n = nn, k = k)
}

#' Linear rank test for ordered categories -- Sec. 14.6.1, p. 531
#' @noRd
Linbylin <- function(table, scores = NULL) {
  tb <- as.matrix(table); storage.mode(tb) <- "double"
  if (nrow(tb) != 2L) stop("table must have exactly 2 rows.", call. = FALSE)
  c <- ncol(tb)
  if (c < 2L) stop("both rows must have the same length, >= 2.", call. = FALSE)
  cs <- colSums(tb); n1 <- sum(tb[1, ]); n2 <- sum(tb[2, ]); nn <- n1 + n2
  if (nn < 2) stop("the table must contain at least 2 observations.", call. = FALSE)
  if (is.null(scores)) {
    w <- cumsum(c(0, cs[-c])) + (cs + 1) / 2
  } else {
    w <- as.numeric(scores)
    if (length(w) != c) stop("scores must have length c.", call. = FALSE)
  }
  t <- sum(w * tb[1, ])
  wbar <- sum(cs * w) / nn
  mean <- n1 * wbar
  var <- n1 * n2 / (nn * (nn - 1)) * sum(cs * (w - wbar)^2)
  sd <- if (var > 0) sqrt(var) else NaN
  z <- if (var > 0) (t - mean) / sd else NaN
  list(statistic = t, mean = mean, var = var, sd = sd, z = z,
       p_value = 1 - stats::pnorm(z),
       p_twosided = 2 * (1 - stats::pnorm(abs(z))), scores = w, n = nn)
}

#' Odds ratio by Woolf's logit method (Woolf 1955) -- NOT from Gibbons
#' @noRd
Oddsrat <- function(table, alpha = 0.05, cc = 0) {
  tb <- as.matrix(table); storage.mode(tb) <- "double"
  tb <- tb + as.numeric(cc)
  if (nrow(tb) != 2L || ncol(tb) != 2L) stop("table must be 2 x 2.", call. = FALSE)
  a <- tb[1, 1]; b <- tb[1, 2]; c2 <- tb[2, 1]; d <- tb[2, 2]
  if (min(a, b, c2, d) <= 0)
    stop(paste("every cell must be positive for the logit method;",
               "pass cc=0.5 to add a continuity constant."), call. = FALSE)
  alpha <- as.numeric(alpha)
  if (alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly inside (0, 1).", call. = FALSE)
  orr <- a * d / (b * c2); lor <- log(orr)
  var <- 1 / a + 1 / b + 1 / c2 + 1 / d
  se <- sqrt(var); z <- stats::qnorm(1 - alpha / 2)
  chi <- lor^2 / var
  list(estimate = orr, log_or = lor, se = se,
       lower = exp(lor - z * se), upper = exp(lor + z * se),
       statistic = chi, df = 1L,
       p_value = stats::pchisq(chi, 1, lower.tail = FALSE))
}
