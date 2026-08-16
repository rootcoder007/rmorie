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
#' @param scramble logical; ignored by this arm, which has no Owen
#'   scrambling. Asking for it warns; the sequence returned is
#'   always the unscrambled one, and \code{scrambled} in the
#'   result says so.
#' @param seed integer; seeds nothing in this arm, since the
#'   sequence returned is deterministic and unscrambled.
#' @return list: sample, estimate (if f given), se, N, d,
#'   \code{scrambled} (always \code{FALSE} in this arm) and method.
#' @importFrom utils getFromNamespace
#' @examples
#' morie_sobol_sequence(N = 128L, d = 2L)
#' @keywords internal
#' @export
sobls <- function(N = 128L, d = 1L, f = NULL, scramble = TRUE, seed = 42L) {
  # Native gray-code Sobol with Joe-Kuo direction numbers (d <= 10);
  # matches randtoolbox's unscrambled sequence (cross-validated in
  # tests). randtoolbox's Owen scrambling is disabled upstream anyway,
  # so the unscrambled sequence is what callers always received.
  if (isTRUE(scramble)) {
    warning("sobls(): Owen scrambling is not implemented in the R arm; ",
            "returning the unscrambled Sobol sequence. The Python arm ",
            "scrambles through scipy.stats.qmc, so the two point sets ",
            "differ. Pass scramble = FALSE to ask for this explicitly.",
            call. = FALSE)
  }
  sample <- .morie_sobol(as.integer(N), as.integer(d))
  out <- list(
    sample = sample, N = as.integer(N), d = as.integer(d),
    scrambled = FALSE,
    method = "Sobol QMC (Sobol 1967), unscrambled"
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
