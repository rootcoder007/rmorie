# morie.fn -- function file (rootcoder007/morie)
# EARS: aberration detection on a short, moving baseline.
#
# Syndromic surveillance rarely has years of clean history for the thing
# being watched. EARS is built for that case: every method uses only the
# last few days, so it can start flagging almost immediately after a data
# feed is switched on.
#
# All three long-term methods are the same CUSUM with different
# baselines. The underlying recursion is
#
#   S_t = max(0, S_{t-1} + (X_t - (mu_0 + k*sigma_t)) / sigma_t),
#
# and the variants differ in two choices only:
#
# C1-MILD:  mean and sd from days t-7..t-1, with S_{t-1} = 0.
# C2-MEDIUM: baseline is days t-9..t-3.
# C3-ULTRA:  accumulates C2's current and two preceding statistics.
#
# References
# ----------
# Hutwagner, L., Thompson, W., Seeman, G. M. & Treadwell, T. (2003)
# "The Bioterrorism Preparedness and Response Early Aberration
# Reporting System (EARS)", Journal of Urban Health: Bulletin of the
# New York Academy of Medicine 80(2, Supplement 1), i89-i96.
#
# Hutwagner, L. C., Maloney, E. K., Bean, N. H., Slutsker, L. & Martin,
# S. M. (1997) "Using laboratory-based surveillance data for
# prevention: an algorithm for detecting Salmonella outbreaks",
# Emerging Infectious Diseases 3(3), 395-400.

.surepi_methods <- c("C1", "C2", "C3")
# (lag, width): baseline is the `width` days ending `lag` days before t.
# C1 is t-7..t-1 and C2/C3 are t-9..t-3 -- both SEVEN days, differing
# only in how far back they stop.
.surepi_windows <- list(C1 = c(1L, 7L), C2 = c(3L, 7L), C3 = c(3L, 7L))

.surepi_baseline <- function(counts, t, lag, width, sigma_floor) {
  # `t` is 0-based (Python convention); translate to 1-based R indices.
  lo <- t - lag - width + 2L
  hi <- t - lag + 1L
  if (lo < 1L) {
    return(list(m = NA_real_, s = NA_real_, used = 0L))
  }
  win <- as.numeric(counts[lo:hi])
  m <- sum(win) / length(win)
  if (length(win) < 2L) {
    return(list(m = m, s = sigma_floor, used = length(win)))
  }
  v <- sum((win - m)^2) / (length(win) - 1L)
  s <- max(sqrt(max(v, 0.0)), as.numeric(sigma_floor))
  list(m = m, s = s, used = length(win))
}

.surepi_stat <- function(counts, method, sigma_floor) {
  w <- .surepi_windows[[method]]
  lag <- w[1]
  width <- w[2]
  n <- length(counts)
  out <- rep(NA_real_, n)
  for (t in 0:(n - 1L)) {
    b <- .surepi_baseline(counts, t, lag, width, sigma_floor)
    if (is.na(b$m)) {
      out[t + 1L] <- NA_real_
    } else {
      out[t + 1L] <- (counts[t + 1L] - b$m) / b$s
    }
  }
  out
}

surepi_c1_mild <- function(counts, threshold = 3.0, sigma_floor = 1.0) {
  surepi_ears_detect(counts, method = "C1", threshold = threshold,
                     sigma_floor = sigma_floor)
}

surepi_c2_medium <- function(counts, threshold = 3.0, sigma_floor = 1.0) {
  surepi_ears_detect(counts, method = "C2", threshold = threshold,
                     sigma_floor = sigma_floor)
}

surepi_c3_ultra <- function(counts, threshold = 2.0, sigma_floor = 1.0) {
  surepi_ears_detect(counts, method = "C3", threshold = threshold,
                     sigma_floor = sigma_floor)
}

surepi_ears_detect <- function(counts, method = "C2", threshold = 3.0,
                               sigma_floor = 1.0) {
  if (!(method %in% .surepi_methods)) {
    stop(sprintf("surepi: method must be one of %s, got '%s'",
                 paste(.surepi_methods, collapse = ", "), method))
  }
  cv <- as.numeric(counts)
  if (any(cv < 0.0)) {
    stop("surepi: counts must be non-negative")
  }
  if (as.numeric(sigma_floor) <= 0.0) {
    stop("surepi: sigma_floor must be positive -- a flat baseline gives sigma = 0 and an undefined statistic")
  }
  w <- .surepi_windows[[method]]
  lag <- w[1]
  width <- w[2]
  need <- lag + width - 1L
  if (length(cv) <= need) {
    stop(sprintf("surepi: %s needs more than %d days of history, got %d",
                 method, need, length(cv)))
  }
  base_method <- if (method == "C3") "C2" else method
  base <- .surepi_stat(cv, base_method, sigma_floor)
  if (method == "C3") {
    stat <- rep(NA_real_, length(cv))
    for (t in seq_along(cv)) {
      idx <- c(t, t - 1L, t - 2L)
      idx <- idx[idx >= 1L & !is.na(base[idx])]
      # C3 accumulates only over days that HAVE a C2 statistic
      if (length(idx) == 3L) {
        stat[t] <- sum(base[idx])
      }
    }
  } else {
    stat <- base
  }
  flags <- stat > as.numeric(threshold)
  list(
    estimate = stat,
    statistic = stat,
    flag = as.logical(flags),
    n_flagged = sum(flags, na.rm = TRUE),
    method = method,
    threshold = as.numeric(threshold),
    baseline_lag = lag,
    baseline_width = width,
    sigma_floor = as.numeric(sigma_floor),
    n = length(cv),
    n_evaluable = sum(!is.na(stat)),
    reference = paste("Hutwagner, Thompson, Seeman & Treadwell (2003),",
                      "EARS long-term methods"),
    caveat = paste("a flag marks a count unusual against its own recent",
                   "history; it is not a test that an outbreak is occurring")
  )
}

surepi_salmonella_cusum <- function(counts, mu0, sigma, k_shift = 1.0,
                                    decision = 0.5, min_count = 5) {
  cv <- as.numeric(counts)
  n <- length(cv)
  if (length(mu0) == 1L) {
    mv <- rep(as.numeric(mu0), n)
  } else {
    mv <- as.numeric(mu0)
  }
  if (length(sigma) == 1L) {
    sv <- rep(as.numeric(sigma), n)
  } else {
    sv <- as.numeric(sigma)
  }
  if (!(length(mv) == length(sv) && length(mv) == n)) {
    stop(sprintf(paste("surepi: mu0 and sigma must be scalars or",
                       "match the series length (%d, %d, %d)"),
                 n, length(mv), length(sv)))
  }
  if (any(sv <= 0.0)) {
    stop("surepi: sigma must be positive everywhere")
  }
  S <- 0.0
  out <- rep(0.0, n)
  flags <- rep(FALSE, n)
  ks <- as.numeric(k_shift)
  dec <- as.numeric(decision)
  mc <- as.numeric(min_count)
  for (t in seq_len(n)) {
    S <- max(0.0, S + (cv[t] - (mv[t] + ks * sv[t])) / sv[t])
    out[t] <- S
    flags[t] <- (S >= dec) && (cv[t] >= mc)
  }
  list(
    cusum = out,
    flag = as.logical(flags),
    estimate = out,
    n_flagged = sum(flags),
    decision = dec,
    k = ks,
    min_count = mc,
    method = paste("Hutwagner et al. (2003) eq. (4); the Salmonella",
                   "Outbreak Detection Algorithm")
  )
}

surepi_compound_smoothing <- function(values, current,
                                      passes = c(4, 2, 5, 3),
                                      multiplier = 2.0) {
  v <- as.numeric(values)
  if (length(v) < max(passes) + 2L) {
    stop(sprintf(paste("surepi: the series is too short for the",
                       "smoothing passes %s (have %d)"),
                 paste(passes, collapse = ", "), length(v)))
  }
  s <- v
  for (w in passes) {
    s <- .surepi_runmed(s, as.integer(w))
  }
  # the H of 4253H
  n <- length(s)
  s2 <- s
  s2[1L] <- s[1L]
  s2[n] <- s[n]
  if (n >= 3L) {
    for (i in 2L:(n - 1L)) {
      s2[i] <- 0.25 * s[i - 1L] + 0.5 * s[i] + 0.25 * s[i + 1L]
    }
  }
  s <- s2
  resid <- v - s
  m <- sum(resid) / length(resid)
  sd <- sqrt(sum((resid - m)^2) / max(length(resid) - 1L, 1L))
  base <- s[n]
  mult <- as.numeric(multiplier)
  list(
    smoothed = s,
    baseline = base,
    sigma = sd,
    threshold = base + mult * sd,
    flag = as.logical(as.numeric(current) > base + mult * sd),
    current = as.numeric(current),
    method = paste("Hutwagner et al. (2003) eq. (5), 4253H compound",
                   "smoothing after Stern & Lightfoot")
  )
}

.surepi_runmed <- function(x, width) {
  # Running median of odd or even width, endpoints carried.
  n <- length(x)
  if (width <= 1L || width > n) {
    return(as.numeric(x))
  }
  half <- width %/% 2L
  out <- as.numeric(x)
  for (i in seq_len(n)) {
    lo <- i - half
    hi <- i + half
    if (width %% 2L == 0L) {
      hi <- i + half - 1L
    }
    if (lo < 1L || hi > n) {
      next
    }
    out[i] <- median(x[lo:hi])
  }
  out
}

surepi_cheatsheet <- function() {
  paste("surepi: EARS. C1 baseline = days t-7..t-1, C2 = t-9..t-3,",
        "C3 = sum of three consecutive C2s. S_{t-1} = 0 for C1 and",
        "C2, so both are just z-scores against a MOVING baseline;",
        "flag at mean + 3 sd. C2's two-day gap keeps a starting",
        "outbreak out of its own baseline. A flat baseline gives",
        "sigma = 0, so sigma_floor is explicit, not silent. A flag",
        "is an aberration, not an outbreak.")
}

# compact alias per ledger/NAMING.md
surepi_earssignal <- surepi_ears_detect

# public names resolved by fn/_lazy_map.json
surepi_surveillance_signal <- surepi_ears_detect

# entry point
morie_surepi <- surepi_ears_detect

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild

#' @rdname surepi_c1_mild
#' @export
morie_surepi <- surepi_c1_mild
