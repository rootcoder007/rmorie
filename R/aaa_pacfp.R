# R arm of morie/fn/pacfP.py -- partial autocorrelation, Levinson-Durbin.
#
# The Python body previously read stats.spearmanr(y[:n], y[:n]) -- the
# series correlated with itself, so the statistic was identically 1.0 for
# every input at every lag, under a docstring describing Levinson-Durbin.
# One of the generator's pasted templates; the same one was found in
# acsamp, joacf and jopacf.
#
# Autocorrelations use the divide-by-n convention, matching R's own acf()
# and pacf(), which keeps the autocovariance sequence positive
# semi-definite so the recursion cannot divide by zero.
#
# Box, Jenkins, Reinsel & Ljung (2015) sec. 3.2.6; Durbin (1960),
# Revue de l'Institut International de Statistique 28(3):233-244.

#' @noRd
morie_partial_autocorrelation <- function(y, lag_max) {
  v <- as.numeric(y)
  n <- length(v)
  k_max <- as.integer(lag_max)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (k_max < 1L || k_max >= n) {
    stop("lag_max must satisfy 1 <= lag_max < length(y).", call. = FALSE)
  }

  dev <- v - mean(v)
  c0 <- sum(dev * dev) / n
  if (c0 <= 0) stop("series is constant; the PACF is undefined.", call. = FALSE)

  r <- numeric(k_max + 1L)
  r[1L] <- 1
  for (k in seq_len(k_max)) {
    ck <- sum(dev[seq_len(n - k)] * dev[(k + 1L):n]) / n
    r[k + 1L] <- ck / c0
  }

  pacf <- numeric(k_max)
  phi_prev <- numeric(0)
  for (k in seq_len(k_max)) {
    if (k == 1L) {
      phi_kk <- r[2L]
      phi_cur <- phi_kk
    } else {
      num <- r[k + 1L]
      den <- 1
      for (j in seq_len(k - 1L)) {
        # parenthesised term by term: R folds a + b + c as (a + b) + c
        # while Python's += folds a + (b + c), and a 1-ULP difference
        # here is amplified by the division below
        num <- num - (phi_prev[j] * r[k - j + 1L])
        den <- den - (phi_prev[j] * r[j + 1L])
      }
      if (abs(den) < 1e-300) {
        stop(sprintf(
          "Levinson-Durbin denominator vanished at lag %d; series is degenerate.",
          k
        ), call. = FALSE)
      }
      phi_kk <- num / den
      phi_cur <- numeric(k)
      for (j in seq_len(k - 1L)) {
        phi_cur[j] <- phi_prev[j] - phi_kk * phi_prev[k - j]
      }
      phi_cur[k] <- phi_kk
    }
    pacf[k] <- phi_kk
    phi_prev <- phi_cur
  }

  list(
    pacf = pacf,
    acf = r,
    phi = phi_prev,
    lag_max = k_max,
    n = n,
    method = "Partial autocorrelation, Levinson-Durbin recursion"
  )
}

#' @noRd
Pacfp <- morie_partial_autocorrelation
