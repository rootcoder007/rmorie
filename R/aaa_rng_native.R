# SPDX-License-Identifier: AGPL-3.0-or-later
#
# A native random number generator: Philox4x32-10 and Wichura's AS 241.
#
# Two published algorithms, implemented rather than delegated, so that the R
# and Python arms of morie draw the SAME numbers rather than merely
# statistically similar ones. The previous arrangement derived a seed by
# SHA-256 and handed it to set.seed(), which leaves the two sides agreeing
# only "to Monte-Carlo tolerance" -- fine for a smoke test, useless for
# cross-language verification of a simulation.
#
# Uniforms -- Philox4x32-10. Salmon, Moraes, Dror and Shaw (2011), "Parallel
# Random Numbers: As Easy as 1, 2, 3", Proc. SC'11. Counter-based: the n-th
# output is a keyed bijection of the integer n, so there is no evolving state
# to keep in step, streams can be indexed at any offset, and the arithmetic
# is integer-only, which is what makes it reproduce bit for bit across
# languages. Period 2^130 per key; passes BigCrush.
#
# This replaces the 32-bit linear congruential generator copied between the
# gr* modules, s = (1664525 s + 1013904223) mod 2^32, whose period is
# exhausted by a few billion draws and whose tuples fall on a small number of
# lattice hyperplanes (Marsaglia, 1968) -- the failure mode that matters most
# for simulating spatial fields, which are built from exactly such tuples.
#
# Normals -- Wichura (1988), "Algorithm AS 241: The Percentage Points of the
# Normal Distribution", Applied Statistics 37(3), 477-484. The rational
# approximation R's qnorm uses, accurate to about 1e-16. Normals come from
# the inverse CDF rather than Box-Muller: one uniform per normal keeps the
# two implementations in step, and it avoids log and cos, whose last-bit
# behaviour differs between platforms' libm.
#
# R has no unsigned 32-bit type, so every word is carried as a double (exact
# to 2^53) and the 32-bit operations are built from 16-bit halves.
#
# Internal; `aaa_` collates it before its callers.

.MORIE_PHILOX_M0 <- 3528531795 # 0xD2511F53
.MORIE_PHILOX_M1 <- 3449720151 # 0xCD9E8D57
.MORIE_PHILOX_W0 <- 2654435769 # 0x9E3779B9, golden ratio
.MORIE_PHILOX_W1 <- 3144134277 # 0xBB67AE85, sqrt(3) - 1
.MORIE_PHILOX_ROUNDS <- 10L

.morie_xor32 <- function(a, b) {
  # XOR of two 32-bit words held as doubles. bitwXor() only accepts values
  # inside the signed 32-bit range, so split into 16-bit halves first.
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  bitwXor(ah, bh) * 65536 + bitwXor(al, bl)
}

.morie_mulhilo32 <- function(a, b) {
  # Exact 32x32 -> 64 bit product, returned as (hi, lo) 32-bit halves. The
  # full product can reach 2^64, past the 2^53 where doubles stop being exact
  # integers, so it is assembled from 16-bit partial products.
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  ll <- al * bl
  lh <- al * bh
  hl <- ah * bl
  hh <- ah * bh
  mid <- lh + hl + (ll %/% 65536) # < 2^33, still exact
  lo <- (mid %% 65536) * 65536 + (ll %% 65536)
  hi <- hh + (mid %/% 65536)
  list(hi = hi %% 4294967296, lo = lo %% 4294967296)
}

.morie_philox4x32 <- function(counter, key, rounds = .MORIE_PHILOX_ROUNDS) {
  # counter: an (n x 4) numeric matrix of 32-bit words; key: length-2 numeric.
  ctr <- matrix(as.numeric(counter), ncol = 4)
  k0 <- key[1] %% 4294967296
  k1 <- key[2] %% 4294967296
  for (r in seq_len(rounds)) {
    p0 <- .morie_mulhilo32(.MORIE_PHILOX_M0, ctr[, 1])
    p1 <- .morie_mulhilo32(.MORIE_PHILOX_M1, ctr[, 3])
    out <- ctr
    out[, 1] <- .morie_xor32(.morie_xor32(p1$hi, ctr[, 2]), k0)
    out[, 2] <- p1$lo
    out[, 3] <- .morie_xor32(.morie_xor32(p0$hi, ctr[, 4]), k1)
    out[, 4] <- p0$lo
    ctr <- out
    if (r < rounds) { # bump the key between rounds
      k0 <- (k0 + .MORIE_PHILOX_W0) %% 4294967296
      k1 <- (k1 + .MORIE_PHILOX_W1) %% 4294967296
    }
  }
  ctr
}

.morie_random_uniform <- function(n, seed = 0, stream = 0) {
  # n uniforms in the OPEN interval (0, 1). The open interval matters: a
  # normal quantile at 0 or 1 is infinite, and (w + 0.5)/2^32 reaches
  # neither endpoint.
  n <- as.integer(n)
  if (n < 0L) stop("`n` must be non-negative", call. = FALSE)
  if (n == 0L) {
    return(numeric(0))
  }
  blocks <- (n + 3L) %/% 4L
  idx <- seq_len(blocks) - 1
  ctr <- cbind(
    idx %% 4294967296, idx %/% 4294967296,
    rep(stream %% 4294967296, blocks), rep(0, blocks)
  )
  words <- .morie_philox4x32(ctr, c(
    seed %% 4294967296,
    (seed %/% 4294967296) %% 4294967296
  ))
  as.numeric(t(words))[seq_len(n)] / 4294967296 + 0.5 / 4294967296
}

# --- Wichura (1988) AS 241, the PPND16 coefficients -----------------------
.MORIE_AS241_A <- c(
  3.3871328727963666080, 1.3314166789178437745e2,
  1.9715909503065514427e3, 1.3731693765509461125e4,
  4.5921953931549871457e4, 6.7265770927008700853e4,
  3.3430575583588128105e4, 2.5090809287301226727e3
)
.MORIE_AS241_B <- c(
  1, 4.2313330701600911252e1, 6.8718700749205790830e2,
  5.3941960214247511077e3, 2.1213794301586595867e4,
  3.9307895800092710610e4, 2.8729085735721942674e4,
  5.2264952788528545610e3
)
.MORIE_AS241_C <- c(
  1.42343711074968357734, 4.63033784615654529590,
  5.76949722146069140550, 3.64784832476320460504,
  1.27045825245236838258, 2.41780725177450611770e-1,
  2.27238449892691845833e-2, 7.74545014278341407640e-4
)
.MORIE_AS241_D <- c(
  1, 2.05319162663775882187, 1.67638483018380384940,
  6.89767334985100004550e-1, 1.48103976427480074590e-1,
  1.51986665636164571966e-2, 5.47593808499534494600e-4,
  1.05075007164441684324e-9
)
.MORIE_AS241_E <- c(
  6.65790464350110377720, 5.46378491116411436990,
  1.78482653991729133580, 2.96560571828504891230e-1,
  2.65321895265761230930e-2, 1.24266094738807843860e-3,
  2.71155556874348757815e-5, 2.01033439929228813265e-7
)
.MORIE_AS241_F <- c(
  1, 5.99832206555887937690e-1, 1.36929880922735805310e-1,
  1.48753612908506148525e-2, 7.86869131145613259100e-4,
  1.84631831751005468180e-5, 1.42151175831644588870e-7,
  2.04426310338993978564e-15
)

.morie_as241_poly <- function(coef, x) {
  out <- rep(coef[length(coef)], length(x))
  for (i in (length(coef) - 1L):1L) out <- out * x + coef[i]
  out
}

.morie_normal_quantile <- function(p) {
  # Wichura's AS 241 (PPND16): split at |p - 1/2| <= 0.425, then at r <= 5.
  p <- as.numeric(p)
  if (any(p <= 0 | p >= 1)) stop("`p` must lie strictly inside (0, 1)", call. = FALSE)
  q <- p - 0.5
  out <- numeric(length(p))
  central <- abs(q) <= 0.425
  if (any(central)) {
    r <- 0.180625 - q[central]^2
    out[central] <- q[central] * .morie_as241_poly(.MORIE_AS241_A, r) /
      .morie_as241_poly(.MORIE_AS241_B, r)
  }
  tail <- !central
  if (any(tail)) {
    qt <- q[tail]
    r <- ifelse(qt < 0, p[tail], 1 - p[tail])
    r <- sqrt(-log(r))
    near <- r <= 5
    rr <- ifelse(near, r - 1.6, r - 5)
    val <- ifelse(near,
      .morie_as241_poly(.MORIE_AS241_C, rr) /
        .morie_as241_poly(.MORIE_AS241_D, rr),
      .morie_as241_poly(.MORIE_AS241_E, rr) /
        .morie_as241_poly(.MORIE_AS241_F, rr)
    )
    out[tail] <- ifelse(qt < 0, -val, val)
  }
  out
}

.morie_random_normal <- function(n, seed = 0, stream = 0) {
  .morie_normal_quantile(.morie_random_uniform(n, seed = seed, stream = stream))
}

.morie_random_multivariate_normal <- function(mean, cov, seed = 0, stream = 0,
                                              jitter = 1e-10) {
  # Z = mean + L e with L L' = cov, the construction Schabenberger & Gotway
  # use for simulating a Gaussian random field.
  mean <- as.numeric(mean)
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!all(dim(cov) == c(n, n))) {
    stop("`cov` must be square and match `mean`", call. = FALSE)
  }
  ch <- tryCatch(chol(cov), error = function(e) chol(cov + jitter * diag(n)))
  mean + as.numeric(t(ch) %*% .morie_random_normal(n, seed = seed, stream = stream))
}
