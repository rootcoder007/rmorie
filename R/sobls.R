# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sobol quasi-random sequence (Sobol 1967)
#'
#' Native gray-code Sobol sequence with Joe-Kuo direction numbers
#' (low-discrepancy in the unit cube, d <= 10); unscrambled it matches
#' \code{randtoolbox::sobol}'s output exactly. With \code{scramble = TRUE}
#' (the default) the points are randomised by a linear matrix scramble
#' plus a random digital shift (Matousek 1998), the same "LMS + shift"
#' family \code{scipy.stats.qmc.Sobol(scramble = TRUE)} uses, so the
#' sequence keeps its net structure while every point is randomised.
#'
#' @param N integer; default 128.
#' @param d integer; default 1.
#' @param f optional integrand; returns scalar.
#' @param scramble logical; \code{TRUE} applies the linear matrix scramble
#'   and digital shift, \code{FALSE} returns the raw sequence.
#' @param seed integer seed for the scramble (the unscrambled sequence is
#'   deterministic and ignores it). The R and Python arms draw their
#'   scrambles from different generators, so their scrambled point sets
#'   differ; the unscrambled sequences agree.
#' @return list: sample, estimate (if f given), se, N, d,
#'   \code{scrambled} and method.
#' @importFrom utils getFromNamespace
#' @examples
#' morie_sobol_sequence(N = 128L, d = 2L)
#' @keywords internal
#' @export
sobls <- function(N = 128L, d = 1L, f = NULL, scramble = TRUE, seed = 42L) {
  N <- as.integer(N)
  d <- as.integer(d)
  sample <- .morie_sobol(N, d)
  scramble <- isTRUE(scramble)
  if (scramble) sample <- .morie_sobol_scramble(sample, seed)
  out <- list(
    sample = sample, N = N, d = d,
    scrambled = scramble,
    method = if (scramble) {
      "Sobol QMC (Sobol 1967), linear matrix scramble + digital shift (Matousek 1998)"
    } else {
      "Sobol QMC (Sobol 1967), unscrambled"
    }
  )
  if (!is.null(f)) {
    fv <- apply(sample, 1, f)
    out$estimate <- mean(fv)
    out$se <- stats::sd(fv) / sqrt(N)
  }
  out
}

# Matousek (1998) linear matrix scrambling with a random digital shift in
# base 2. The native generator carries nbits = 31 bits, so every point is
# an exact multiple of 2^-31 and the bit matrix below is exact. For each
# dimension a random lower-triangular 0/1 matrix L with unit diagonal maps
# the bit vector x (most significant bit first) to y = L x (mod 2), which
# preserves the (t, m, s)-net property; the digital shift then XORs a
# random bit vector so the origin is no longer a sample point.
.morie_sobol_scramble <- function(x, seed, nbits = 31L) {
  .rmorie_local_seed(seed)
  n <- nrow(x)
  d <- ncol(x)
  ints <- round(x * 2^nbits)
  pow <- 2^(nbits - seq_len(nbits)) # bit weights, MSB first
  out <- matrix(0, n, d)
  for (j in seq_len(d)) {
    bits <- outer(ints[, j], pow, function(a, b) floor(a / b)) %% 2
    L <- matrix(stats::rbinom(nbits * nbits, 1L, 0.5), nbits, nbits)
    L[upper.tri(L)] <- 0
    diag(L) <- 1
    y <- (bits %*% t(L)) %% 2
    shift <- stats::rbinom(nbits, 1L, 0.5)
    y <- (y + matrix(shift, n, nbits, byrow = TRUE)) %% 2
    out[, j] <- as.numeric(y %*% pow) / 2^nbits
  }
  out
}

# CANONICAL TEST
# r <- sobls(N = 128, d = 2, f = function(u) u[1] * u[2], seed = 0)
# stopifnot(abs(r$estimate - 0.25) < 0.05)

#' @rdname sobls
#' @keywords internal
#' @export
morie_sobol_sequence <- sobls
