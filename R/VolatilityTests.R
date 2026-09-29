.vt_mtw <- function(n, d) {
  k <- floor(n * d) + 1
  m <- 2 * k - 1
  h <- k - n * d
  H <- matrix(0, m, m)
  for (i in 1:m) for (j in 1:m) if (i - j + 1 >= 0) H[i, j] <- 1
  for (i in 1:m) {
    H[i, 1] <- H[i, 1] - h^i
    H[m, i] <- H[m, i] - h^(m - i + 1)
  }
  if (2 * h - 1 > 0) H[m, 1] <- H[m, 1] + (2 * h - 1)^m
  for (i in 1:m) for (j in 1:m) if (i - j + 1 > 0) for (g in 1:(i - j + 1)) H[i, j] <- H[i, j] / g
  rescale <- function(A, e) if (A[k, k] > 1e140) list(A * 1e-140, e + 140) else list(A, e)
  Q <- NULL
  eQ <- 0
  P <- H
  eP <- 0
  nn <- n
  while (nn > 0) {
    if (nn %% 2 == 1) {
      if (is.null(Q)) {
        Q <- P
        eQ <- eP
      } else {
        r <- rescale(Q %*% P, eQ + eP)
        Q <- r[[1]]
        eQ <- r[[2]]
      }
    }
    nn <- nn %/% 2
    if (nn > 0) {
      r <- rescale(P %*% P, 2 * eP)
      P <- r[[1]]
      eP <- r[[2]]
    }
  }
  s <- Q[k, k]
  for (i in 1:n) {
    s <- s * i / n
    if (s < 1e-140) {
      s <- s * 1e140
      eQ <- eQ - 140
    }
  }
  max(0, min(1, s * 10^eQ))
}

.vt_ks_sf_asym <- function(d, n) {
  lam <- d * (sqrt(n) + 0.12 + 0.11 / sqrt(n))
  if (lam < 0.04) return(1)
  s <- 0
  for (j in 1:100) {
    term <- 2 * (-1)^(j - 1) * exp(-2 * j * j * lam * lam)
    s <- s + term
    if (abs(term) < 1e-12) break
  }
  max(0, min(1, s))
}

.vt_kstwo_sf <- function(d, n) {
  if (d <= 0) return(1)
  if (d >= 1) return(0)
  if (floor(n * d) + 1 <= 64) 1 - .vt_mtw(n, d) else .vt_ks_sf_asym(d, n)
}

.vt_ks <- function(z, cdf) {
  n <- length(z)
  max(max(seq_len(n) / n - cdf), max(cdf - (seq_len(n) - 1) / n))
}

.vt_fitted_ks <- function(z) .vt_ks(z, stats::pnorm(z, mean(z), stats::sd(z)))

#' Volatility diagnostics: ARCH-LM and multi-horizon distributional accuracy
#'
#' `vol_engle_lagrange` is Engle's (1982) ARCH-LM test: `m R^2` of the squared
#' (demeaned) series on a constant and `q` of its lags, `m = n - q`, referred
#' to chi-square with `q` df.  `vol_corradi_swan_persistence` compares the
#' distribution of non-overlapping `h`-period sums with a model CDF by the
#' Kolmogorov distance at several horizons (Corradi and Swanson 2006): with
#' a supplied `cdf(x, h)` the exact finite-n Kolmogorov distribution
#' (Marsaglia, Tsang and Wang 2003) gives the p-value; without one a
#' Gaussian is fitted per horizon and the null is simulated (Lilliefors),
#' replicate `k` of horizon `i` using Philox stream `i * n_mc + k` as in the
#' Python arm.  The joint p-value is Bonferroni.
#'
#' @param r Return series.
#' @param q Number of ARCH lags.
#' @param demean Subtract the mean before squaring.
#' @param horizons Aggregation horizons.
#' @param cdf Optional model CDF `function(x, h)`.
#' @param n_mc Monte Carlo replicates.
#' @param seed Philox seed.
#' @return Lists with `statistic`, `p_value` and method-specific parts.
#' @references Engle, R. F. (1982). Autoregressive conditional
#'   heteroscedasticity with estimates of the variance of United Kingdom
#'   inflation. Econometrica 50, 987-1007. Corradi, V. and Swanson, N. R.
#'   (2006). Predictive density and conditional confidence interval accuracy
#'   tests. Journal of Econometrics 135, 187-228. Lilliefors, H. W. (1967).
#'   On the Kolmogorov-Smirnov test for normality with mean and variance
#'   unknown. JASA 62, 399-402. Marsaglia, G., Tsang, W. W. and Wang, J.
#'   (2003). Evaluating Kolmogorov's distribution. Journal of Statistical
#'   Software 8(18).
#' @examples
#' r <- sin(1.3 * (0:79)) * (1 + 0.8 * sin(0.2 * (0:79)))
#' vol_engle_lagrange(r, q = 2)$statistic
#' @export
vol_engle_lagrange <- function(r, q = 1, demean = TRUE) {
  r <- as.numeric(r)
  n <- length(r)
  if (q < 1) stop("q must be at least 1")
  if (n < q + 2) stop("Need at least q + 2 observations")
  e2 <- (if (demean) r - mean(r) else r)^2
  Y <- e2[(q + 1):n]
  X <- cbind(1, vapply(seq_len(q), function(j) e2[(q + 1 - j):(n - j)], numeric(n - q)))
  res <- stats::lm.fit(X, Y)$residuals
  tss <- sum((Y - mean(Y))^2)
  if (tss <= 0) stop("squared series has zero variance; LM test undefined.")
  r2 <- 1 - sum(res^2) / tss
  lm <- (n - q) * r2
  list(statistic = lm, p_value = stats::pchisq(lm, q, lower.tail = FALSE), df = q, r2 = r2, n = n, q = q,
       method = paste0("Engle ARCH-LM test (q=", q, ")"))
}

#' @rdname vol_engle_lagrange
#' @export
vol_corradi_swan_persistence <- function(r, horizons = c(1, 5, 20), cdf = NULL, n_mc = 500, seed = 0) {
  r <- as.numeric(r)
  n <- length(r)
  per <- lapply(seq_along(horizons), function(i) {
    h <- horizons[i]
    m <- n %/% h
    if (m < 3) stop("horizon ", h, " leaves fewer than 3 aggregates")
    agg <- sort(colSums(matrix(r[seq_len(m * h)], h)))
    if (is.null(cdf)) {
      d <- .vt_fitted_ks(agg)
      cnt <- sum(vapply(seq_len(n_mc) - 1, function(k) {
        z <- sort(.morie_random_normal(m, seed = seed, stream = (i - 1) * n_mc + k))
        .vt_fitted_ks(z) >= d
      }, TRUE))
      p <- (1 + cnt) / (1 + n_mc)
    } else {
      d <- .vt_ks(agg, vapply(agg, function(x) cdf(x, h), 0))
      p <- .vt_kstwo_sf(d, m)
    }
    list(h = h, n_h = m, statistic = d, p_value = p)
  })
  st <- vapply(per, function(e) e$statistic, 0)
  pv <- vapply(per, function(e) e$p_value, 0)
  list(statistic = max(st), p_value = min(1, length(per) * min(pv)), per_horizon = per, horizons = horizons, n = n,
       method = "Multi-horizon KS-type distributional accuracy (Corradi-Swanson type)")
}
