# Standardized Precipitation Index.
# Source: McKee, Doesken & Kleist (1993), 8th Conf. Applied
# Climatology, 179-184; Edwards & McKee (1997), Climatology Report
# 97-2, Colorado State University, Eqs. 3.6-3.18; Thom (1958),
# Monthly Weather Review 86(4), 117-122; WMO-No. 1090 (2012).
# All four local in fetched-wave3/ (delivered by Vee 2026-08-11).
# Mirrors Python morie.fn.droSPI exactly.

.spi_c <- c(2.515517, 0.802853, 0.010328)   # Eq. 3.17
.spi_d <- c(1.432788, 0.189269, 0.001308)   # Eq. 3.18

.spi_as_z <- function(h) {
  if (h <= 0 || h >= 1) stop("cumulative probability out of (0, 1)")
  if (h <= 0.5) {
    t_ <- sqrt(log(1 / (h * h)))
    s <- -1
  } else {
    t_ <- sqrt(log(1 / ((1 - h)^2)))
    s <- 1
  }
  num <- .spi_c[1] + .spi_c[2] * t_ + .spi_c[3] * t_^2
  den <- 1 + .spi_d[1] * t_ + .spi_d[2] * t_^2 + .spi_d[3] * t_^3
  s * (t_ - num / den)
}

.spi_fit_thom <- function(xs) {
  pos <- xs[xs > 0]
  q <- (length(xs) - length(pos)) / length(xs)
  if (length(pos) < 3) stop("need at least three positive totals to fit")
  xbar <- mean(pos)
  a_stat <- log(xbar) - mean(log(pos))
  if (a_stat <= 0) stop("degenerate sample (all values equal)")
  alpha <- (1 + sqrt(1 + 4 * a_stat / 3)) / (4 * a_stat)
  beta <- xbar / alpha
  c(q, alpha, beta)
}

#' Standardized Precipitation Index
#'
#' McKee et al. (1993) / Edwards & McKee (1997): running scale-month
#' totals are gamma-fitted by Thom's approximate ML estimates
#' (A = ln(xbar) - mean(ln x), alpha = (1 + sqrt(1 + 4A/3))/(4A),
#' beta = xbar/alpha), the mixed cumulative probability H = q +
#' (1 - q)G(x) accounts for zero months (q = m/n), and H is mapped to
#' the standard normal by the Abramowitz-Stegun approximation with
#' the report's printed constants.
#'
#' @param precip Non-negative monthly precipitation series
#'   (January-first for \code{by_month}).
#' @param scale Accumulation scale in months.
#' @param by_month Fit per calendar month (default) or pooled.
#' @return A list with elements \code{spi} (NA for the first
#'   scale-1 entries), \code{totals}, \code{params}, \code{scale},
#'   \code{by_month}, \code{method}.
#' @references McKee, T. B., Doesken, N. J. and Kleist, J. (1993).
#'   8th Conference on Applied Climatology, 179-184.  Edwards, D. C.
#'   and McKee, T. B. (1997). Climatology Report 97-2, Colorado State
#'   University.  Thom, H. C. S. (1958). Monthly Weather Review,
#'   86(4), 117-122.  WMO (2012). WMO-No. 1090.
#' @export
morie_drospi <- function(precip, scale = 3, by_month = TRUE) {
  x <- as.numeric(precip)
  if (any(x < 0)) stop("precipitation must be non-negative")
  n <- length(x)
  scale <- as.integer(scale)
  if (scale < 1 || n < scale + 5) stop("series too short for this scale")
  totals <- rep(NA_real_, n)
  for (i in scale:n) totals[i] <- sum(x[(i - scale + 1):i])
  keys <- if (by_month) ((seq_len(n) - 1) %% 12) else rep(-1L, n)
  params <- list()
  for (k in unique(keys[!is.na(totals)])) {
    vals <- totals[!is.na(totals) & keys == k]
    params[[as.character(k)]] <- .spi_fit_thom(vals)
  }
  spi <- rep(NA_real_, n)
  for (i in which(!is.na(totals))) {
    p <- params[[as.character(keys[i])]]
    g <- if (totals[i] > 0) pgamma(totals[i], shape = p[2],
                                   scale = p[3]) else 0
    h <- p[1] + (1 - p[1]) * g
    h <- min(max(h, 1e-9), 1 - 1e-9)
    spi[i] <- .spi_as_z(h)
  }
  list(spi = spi, totals = totals, params = params, scale = scale,
       by_month = by_month,
       method = "SPI (McKee 1993; Edwards-McKee 1997 Eqs. 3.6-3.18)")
}
