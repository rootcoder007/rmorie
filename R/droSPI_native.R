# Standardized Precipitation Index (McKee, Doesken & Kleist 1993).
# Sources: McKee, T. B., Doesken, N. J. & Kleist, J. (1993).
# The relationship of drought frequency and duration to time
# scales. *8th Conf. on Applied Climatology*, 179-184.
# Edwards, D. C. & McKee, T. B. (1997). *Characteristics of
# 20th Century Drought in the United States at Multiple Time
# Scales*. Climatology Report 97-2, Colorado State University,
# Eqs. 3.6-3.18.  Thom, H. C. S. (1958). A note on the gamma
# distribution. *Monthly Weather Review*, 86(4), 117-122.
# WMO (2012). *Standardized Precipitation Index User Guide*
# (WMO-No. 1090), Geneva.
#
# Native implementation mirroring Python morie.fn.droSPI exactly:
# the same running scale-month totals, the same Thom 1958
# approximate ML estimates A = ln(xbar) - mean(ln x), alpha =
# (1 + sqrt(1 + 4A/3)) / (4A), beta = xbar / alpha, the same
# q = m/n zero-precipitation fraction, the same H = q + (1 - q)
# G with G the gamma CDF, the same Edwards & McKee Eqs. 3.14-3.18
# Abramowitz-Stegun rational approximation to the standard-normal
# equiprobability transform with the printed c0..d3 constants, the
# same H clipped to (1e-9, 1 - 1e-9), the same per-calendar-month
# fitting when by_month is true, the same SPI = None for the first
# scale - 1 entries, and the same payload keys (with params keyed
# by the stringified group key, matching the Python payload).

.drospi_c0 <- 2.515517; .drospi_c1 <- 0.802853; .drospi_c2 <- 0.010328
.drospi_d1 <- 1.432788; .drospi_d2 <- 0.189269; .drospi_d3 <- 0.001308

.drospi_as_z <- function(h) {
  if (h <= 0.0 || h >= 1.0)
    stop("cumulative probability out of (0, 1)")
  if (h <= 0.5) {
    t <- sqrt(log(1.0 / (h * h)))
    sign <- -1.0
  } else {
    t <- sqrt(log(1.0 / ((1.0 - h)^2)))
    sign <- 1.0
  }
  num <- .drospi_c0 + .drospi_c1 * t + .drospi_c2 * t * t
  den <- 1.0 + .drospi_d1 * t + .drospi_d2 * t * t + .drospi_d3 * t^3
  sign * (t - num / den)
}

.drospi_fit_thom <- function(xs) {
  pos <- xs[xs > 0]
  n <- length(xs)
  m <- n - length(pos)
  q <- m / as.numeric(n)
  if (length(pos) < 3L)
    stop("need at least three positive totals to fit")
  xbar <- sum(pos) / length(pos)
  a_stat <- log(xbar) - sum(log(pos)) / length(pos)
  if (a_stat <= 0)
    stop("degenerate sample (all values equal)")
  alpha <- (1.0 + sqrt(1.0 + 4.0 * a_stat / 3.0)) / (4.0 * a_stat)
  beta <- xbar / alpha
  list(q = q, alpha = alpha, beta = beta)
}

morie_droSPI <- function(precip, scale = 3L, by_month = TRUE) {
  x <- as.numeric(precip)
  if (any(x < 0))
    stop("precipitation must be non-negative")
  n <- length(x)
  scale_i <- as.integer(scale)
  if (scale_i < 1L || n < scale_i + 5L)
    stop("series too short for this scale")
  totals <- vector("list", n)
  for (i in seq_len(n)) {
    if (i < scale_i) {
      totals[[i]] <- NA_real_
    } else {
      totals[[i]] <- sum(x[(i - scale_i + 1L):i])
    }
  }
  groups <- list()
  for (i in seq_len(n)) {
    tv <- totals[[i]]
    if (is.na(tv)) next
    key <- if (isTRUE(by_month)) as.character(((i - 1L) %% 12L))
           else "pooled"
    if (is.null(groups[[key]])) groups[[key]] <- numeric(0)
    groups[[key]] <- c(groups[[key]], tv)
  }
  params <- list()
  for (key in names(groups)) {
    fit <- .drospi_fit_thom(groups[[key]])
    params[[key]] <- c(fit$q, fit$alpha, fit$beta)
  }
  spi <- vector("list", n)
  for (i in seq_len(n)) {
    tv <- totals[[i]]
    if (is.na(tv)) { spi[[i]] <- NA_real_; next }
    key <- if (isTRUE(by_month)) as.character(((i - 1L) %% 12L))
           else "pooled"
    par <- params[[key]]
    q <- par[1L]; alpha <- par[2L]; beta <- par[3L]
    g <- if (tv > 0) pgamma(tv, shape = alpha, scale = beta) else 0.0
    h <- q + (1.0 - q) * g
    h <- min(max(h, 1e-9), 1.0 - 1e-9)
    spi[[i]] <- .drospi_as_z(h)
  }
  list(spi = spi,
       totals = totals,
       params = params,
       scale = scale_i,
       by_month = isTRUE(by_month),
       method = "SPI (McKee 1993; Edwards-McKee 1997 Eqs. 3.6-3.18)")
}

standardized_precipitation_index <- morie_droSPI
spi <- morie_droSPI

cheatsheet <- function() {
  "droSPI: gamma-fit totals (Thom MLE), H=q+(1-q)G, A-S normal transform"
}
