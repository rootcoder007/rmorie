# SPDX-License-Identifier: AGPL-3.0-or-later
# SplitMix64 -> uniform/normal, the R mirror of
# src/morie/fn/_array_core.py::_SplitMix64.  Written so that
# `np.random.default_rng(seed)` in the Python arm and `.ghc_rng(seed)`
# here produce BIT-IDENTICAL streams; the gh_c* modules are all
# demonstrations run on simulated designs, so parity at 1e-9 is only
# meaningful if the draws themselves agree.
#
# R has no unsigned 64-bit type, so every 64-bit word is carried as
# c(hi, lo) with each half an exact double in [0, 2^32).  All products
# are formed from 16-bit limbs, which keeps every intermediate below
# 2^53 and therefore exact.

.ghc_M32 <- 4294967296

.ghc_xor32 <- function(a, b) {
  # bitwXor is signed-32-bit; split into 16-bit halves to stay in range.
  bitwXor(a %/% 65536L, b %/% 65536L) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

.ghc_xor64 <- function(a, b) c(.ghc_xor32(a[1], b[1]), .ghc_xor32(a[2], b[2]))

.ghc_add64 <- function(a, b) {
  lo <- a[2] + b[2]
  carry <- if (lo >= .ghc_M32) 1 else 0
  c((a[1] + b[1] + carry) %% .ghc_M32, lo %% .ghc_M32)
}

.ghc_shr64 <- function(a, k) {
  # logical right shift, 0 < k < 32 (the only widths SplitMix64 uses)
  p <- 2^k
  c(floor(a[1] / p), floor(a[2] / p) + (a[1] %% p) * 2^(32 - k))
}

.ghc_mul32 <- function(a, b) {
  # exact 32x32 -> 64 via 16-bit limbs
  a0 <- a %% 65536; a1 <- a %/% 65536
  b0 <- b %% 65536; b1 <- b %/% 65536
  mid <- a0 * b1 + a1 * b0
  lo <- a0 * b0 + (mid %% 65536) * 65536
  c((a1 * b1 + mid %/% 65536 + lo %/% .ghc_M32) %% .ghc_M32, lo %% .ghc_M32)
}

.ghc_mul64 <- function(a, b) {
  # (a * b) mod 2^64: only the low word of each cross term survives
  r <- .ghc_mul32(a[2], b[2])
  hi <- (r[1] + .ghc_mul32(a[1], b[2])[2] + .ghc_mul32(a[2], b[1])[2]) %% .ghc_M32
  c(hi, r[2])
}

.GHC_GOLDEN <- c(2654435769, 2135587861)   # 0x9E3779B97F4A7C15
.GHC_MIX1 <- c(3210233709, 484763065)      # 0xBF58476D1CE4E5B9
.GHC_MIX2 <- c(2496678331, 321982955)      # 0x94D049BB133111EB

#' @keywords internal
#' @noRd
.ghc_rng <- function(seed = 0) {
  s <- as.numeric(seed)
  if (s == 0) {
    state <- .GHC_GOLDEN
  } else {
    state <- c(floor(s / .ghc_M32) %% .ghc_M32, s %% .ghc_M32)
  }
  e <- new.env(parent = emptyenv())
  e$state <- state
  e
}

#' @keywords internal
#' @noRd
.ghc_next <- function(e) {
  e$state <- .ghc_add64(e$state, .GHC_GOLDEN)
  z <- e$state
  z <- .ghc_mul64(.ghc_xor64(z, .ghc_shr64(z, 30)), .GHC_MIX1)
  z <- .ghc_mul64(.ghc_xor64(z, .ghc_shr64(z, 27)), .GHC_MIX2)
  .ghc_xor64(z, .ghc_shr64(z, 31))
}

#' @keywords internal
#' @noRd
.ghc_unif <- function(e, n = 1L, low = 0, high = 1) {
  out <- numeric(n)
  if (n < 1L) return(numeric(0))
  for (i in seq_len(n)) {
    z <- .ghc_next(e)
    # (z >> 11) / 2^53, split so the numerator never leaves the exact range
    u <- z[1] / .ghc_M32 + floor(z[2] / 2048) / 9007199254740992
    out[i] <- low + (high - low) * u
  }
  out
}

#' @keywords internal
#' @noRd
.ghc_norm <- function(e, n = 1L, loc = 0, scale = 1) {
  # Box-Muller, cosine branch only, exactly as the Python arm does it:
  # two uniforms are consumed per variate and the sine branch is discarded.
  out <- numeric(n)
  if (n < 1L) return(numeric(0))
  for (i in seq_len(n)) {
    u1 <- max(.ghc_unif(e, 1L), 1e-300)
    u2 <- .ghc_unif(e, 1L)
    out[i] <- loc + scale * sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  out
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

# Weighted draw with replacement, mirroring _array_core Generator.choice
# with `p=`: one uniform per draw, inverse-CDF on the UNNORMALISED weights.
#' @keywords internal
#' @noRd
.ghc_choice_p <- function(e, vals, w) {
  u <- .ghc_unif(e, 1L) * sum(w)
  cs <- cumsum(w)
  i <- which(u <= cs)[1]
  if (is.na(i)) i <- length(w)
  vals[i]
}
