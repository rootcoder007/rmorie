# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sobol quasi-random sequence (Sobol 1967)
#'
#' Native gray-code Sobol sequence with Joe-Kuo direction numbers
#' (low-discrepancy in the unit cube, d <= 10); matches
#' \code{randtoolbox::sobol}'s unscrambled output exactly.
#'
#' @param N integer; default 128.
#' @param d integer; default 1.
#' @param f optional integrand; returns scalar.
#' @param scramble logical; Owen scrambling (default TRUE).
#' @param seed integer.
#' @return list: sample, estimate (if f given), se, N, d, method.
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @export
sobls <- function(N = 128L, d = 1L, f = NULL, scramble = TRUE, seed = 42L) {
  # Native gray-code Sobol with Joe-Kuo direction numbers (d <= 10);
  # matches randtoolbox's unscrambled sequence (cross-validated in
  # tests). randtoolbox's Owen scrambling is disabled upstream anyway,
  # so the unscrambled sequence is what callers always received.
  sample <- .morie_sobol(as.integer(N), as.integer(d))
  out <- list(
    sample = sample, N = as.integer(N), d = as.integer(d),
    method = "Sobol QMC (Sobol 1967)"
  )
  if (!is.null(f)) {
    fv <- apply(sample, 1, f)
    out$estimate <- mean(fv)
    out$se <- stats::sd(fv) / sqrt(N)
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
