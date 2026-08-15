# SPDX-License-Identifier: AGPL-3.0-or-later
# SplitMix64 -> uniform/normal, the R mirror of
# src/morie/fn/_array_core.py::_SplitMix64.  Written so that
# `np.random.default_rng(seed)` in the Python arm and `.ghc_rng(seed)`
# here produce BIT-IDENTICAL streams; the gh_c* modules are all
# demonstrations run on simulated designs, so parity at 1e-9 is only
# meaningful if the draws themselves agree.
#
# R has no unsigned 64-bit type, so every 64-bit word is carried as a
# pair of double vectors (hi, lo), each in [0, 2^32).  All products are
# formed from 16-bit limbs, which keeps every intermediate below 2^53
# and therefore exact.  SplitMix64's state is a plain counter,
# state_i = seed + i * GOLDEN, so a whole block of draws is computed
# vectorised rather than one call at a time.

.ghc_M32 <- 4294967296

.ghc_xor32 <- function(a, b) {
  # bitwXor is signed-32-bit; split into 16-bit halves to stay in range.
  bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

.ghc_xor64 <- function(a, b)
  list(hi = .ghc_xor32(a$hi, b$hi), lo = .ghc_xor32(a$lo, b$lo))

.ghc_add64 <- function(a, b) {
  lo <- a$lo + b$lo
  carry <- lo %/% .ghc_M32
  list(hi = (a$hi + b$hi + carry) %% .ghc_M32, lo = lo %% .ghc_M32)
}

.ghc_shr64 <- function(a, k) {
  # logical right shift, 0 < k < 32 (the only widths SplitMix64 uses)
  p <- 2^k
  list(hi = floor(a$hi / p),
       lo = floor(a$lo / p) + (a$hi %% p) * 2^(32 - k))
}

.ghc_mul32 <- function(a, b) {
  # exact 32x32 -> 64 via 16-bit limbs; `a` a vector, `b` a scalar
  a0 <- a %% 65536; a1 <- a %/% 65536
  b0 <- b %% 65536; b1 <- b %/% 65536
  mid <- a0 * b1 + a1 * b0
  lo <- a0 * b0 + (mid %% 65536) * 65536
  list(hi = (a1 * b1 + mid %/% 65536 + lo %/% .ghc_M32) %% .ghc_M32,
       lo = lo %% .ghc_M32)
}

.ghc_mul64 <- function(a, b) {
  # (a * b) mod 2^64: only the low word of each cross term survives
  r <- .ghc_mul32(a$lo, b[2])
  list(hi = (r$hi + .ghc_mul32(a$hi, b[2])$lo +
               .ghc_mul32(a$lo, b[1])$lo) %% .ghc_M32,
       lo = r$lo)
}

.GHC_GOLDEN <- c(2654435769, 2135587861)   # 0x9E3779B97F4A7C15
.GHC_MIX1 <- c(3210233709, 484763065)      # 0xBF58476D1CE4E5B9
.GHC_MIX2 <- c(2496678331, 321982955)      # 0x94D049BB133111EB

#' @keywords internal
#' @noRd
.ghc_rng <- function(seed = 0) {
  s <- as.numeric(seed)
  e <- new.env(parent = emptyenv())
  e$s0 <- if (s == 0) .GHC_GOLDEN else
    c(floor(s / .ghc_M32) %% .ghc_M32, s %% .ghc_M32)
  e$i <- 0                                # draws consumed so far
  e
}

#' @keywords internal
#' @noRd
.ghc_unif <- function(e, n = 1L, low = 0, high = 1) {
  n <- as.integer(n)
  if (n < 1L) return(numeric(0))
  idx <- e$i + seq_len(n)
  e$i <- e$i + n
  # state_i = s0 + i * GOLDEN  (mod 2^64), the SplitMix64 counter
  z <- .ghc_add64(list(hi = rep(e$s0[1], n), lo = rep(e$s0[2], n)),
                  .ghc_mul64(list(hi = floor(idx / .ghc_M32),
                                  lo = idx %% .ghc_M32), .GHC_GOLDEN))
  z <- .ghc_mul64(.ghc_xor64(z, .ghc_shr64(z, 30)), .GHC_MIX1)
  z <- .ghc_mul64(.ghc_xor64(z, .ghc_shr64(z, 27)), .GHC_MIX2)
  z <- .ghc_xor64(z, .ghc_shr64(z, 31))
  # (z >> 11) / 2^53, split so the numerator never leaves the exact range
  u <- z$hi / .ghc_M32 + floor(z$lo / 2048) / 9007199254740992
  low + (high - low) * u
}

#' @keywords internal
#' @noRd
.ghc_norm <- function(e, n = 1L, loc = 0, scale = 1) {
  # Box-Muller, cosine branch only, exactly as the Python arm does it:
  # two uniforms are consumed per variate and the sine branch is discarded.
  n <- as.integer(n)
  if (n < 1L) return(numeric(0))
  uu <- .ghc_unif(e, 2L * n)
  u1 <- pmax(uu[seq(1L, 2L * n, by = 2L)], 1e-300)
  u2 <- uu[seq(2L, 2L * n, by = 2L)]
  loc + scale * sqrt(-2 * log(u1)) * cos(2 * pi * u2)
}

# Weighted draw with replacement, mirroring _array_core Generator.choice
# with `p=`: one uniform per draw, inverse-CDF on the UNNORMALISED weights.
#' @keywords internal
#' @noRd
.ghc_choice_p <- function(e, vals, w) {
  u <- .ghc_unif(e, 1L) * sum(w)
  i <- which(u <= cumsum(w))[1]
  if (is.na(i)) i <- length(w)
  vals[i]
}

# Exact log marginal likelihood of a normal-means model keeping the first
# K coordinates (theta_k ~ N(0, tau2)) and zeroing the rest, with
# y_k | theta ~ N(theta_k, 1 / n_prec).  Shared by the sec. 10.x
# adaptation demonstrations (GvdV 2017 chs. 10, 12).
#' @keywords internal
#' @noRd
.ghc_log_evidence_K <- function(y, n_prec, K, tau2 = 1) {
  v <- 1 / n_prec
  s2 <- v + ifelse(seq_along(y) <= K, tau2, 0)
  sum(-0.5 * log(2 * pi * s2) - 0.5 * y * y / s2)
}

# Marsaglia & Tsang (2000) squeeze-method gamma variate, mirroring
# _array_core._SplitMix64._gamma_variate: shape < 1 is boosted via
# Gamma(a+1) U^(1/a), and each rejection trial consumes one normal
# (two uniforms) plus one uniform, so the stream position matches the
# Python arm trial for trial.
#
# NOTE: _array_core defines gamma/beta TWICE inside _SplitMix64; the
# earlier pair (a non-squeeze Marsaglia-Tsang) is dead code shadowed by
# the later pair.  This mirrors the LIVE one.
#' @keywords internal
#' @noRd
.ghc_gamma1 <- function(e, shape, scale = 1) {
  a <- as.numeric(shape)
  if (a <= 0) stop("shape must be positive")
  if (a < 1) {
    u <- .ghc_unif(e, 1L)
    while (u <= 0) u <- .ghc_unif(e, 1L)
    return(.ghc_gamma1(e, a + 1) * u^(1 / a) * scale)
  }
  d <- a - 1 / 3
  cc <- 1 / sqrt(9 * d)
  repeat {
    x <- .ghc_norm(e, 1L)
    v <- (1 + cc * x)^3
    if (v <= 0) next
    u <- .ghc_unif(e, 1L)
    if (u < 1 - 0.0331 * x^4) return(d * v * scale)
    if (u > 0 && log(u) < 0.5 * x * x + d * (1 - v + log(v)))
      return(d * v * scale)
  }
}

#' @keywords internal
#' @noRd
.ghc_beta1 <- function(e, a, b) {
  x <- .ghc_gamma1(e, a)
  y <- .ghc_gamma1(e, b)
  x / (x + y)
}

# Moore-Penrose pseudo-inverse with numpy's default cutoff
# (rcond = 1e-15 times the largest singular value).
#' @keywords internal
#' @noRd
.ghc_pinv <- function(a, rcond = 1e-15) {
  a <- as.matrix(a)
  s <- svd(a)
  cutoff <- rcond * max(c(s$d, 0))
  inv <- ifelse(s$d > cutoff, 1 / s$d, 0)
  s$v %*% (inv * t(s$u))
}

# glibc's LCG, exactly. Several modules mirror a Python arm that does
# (1103515245 * st + 12345) % (1 << 31) in big integers. Written
# directly in R that product reaches 2.4e18, far past the 2^53 where a
# double stops being exact -- and with integer literals it overflows to
# NA outright. Split the state into 16-bit limbs and reduce each partial
# product before recombining, so every intermediate stays below 2^53.
.ghc_lcg31 <- function(st) {
  hi <- st %/% 65536
  lo <- st %% 65536
  a_hi <- (1103515245 * hi) %% 2147483648
  a_lo <- (1103515245 * lo) %% 2147483648
  ((a_hi * 65536) %% 2147483648 + a_lo + 12345) %% 2147483648
}

# The uniform the Python arms take from it: st / 2^31 AFTER the step.
.ghc_lcg31_unif <- function(env) {
  env$st <- .ghc_lcg31(env$st)
  env$st / 2147483648
}
