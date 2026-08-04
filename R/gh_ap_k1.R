# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fano lower bound on the error probability of an M-ary test.
#'
#' The bound is informative only once log M exceeds I + log 2.
#'
#' Formula: P_err >= 1 - (I(theta; X) + log 2) / log M
#'
#' @param M Number of hypotheses, M >= 2.
#' @param mutual_info I(theta; X), non-negative.
#' @param base_e TRUE for nats, FALSE for bits.
#' @return List with \code{bound}, \code{raw_bound}, \code{log_M},
#'   \code{informative}, \code{M}.
#' @references Fano (1961), Transmission of Information, MIT Press, and
#'   Cover & Thomas (2006), Elements of Information Theory, 2nd edition,
#'   Theorem 2.10.1. The worklist filed this under "Ghosal Appendix K";
#'   the copy of Ghosal & van der Vaart (2017) held in the corpus was
#'   searched in full and the word "Fano" does NOT occur in it, so the
#'   attribution could not be confirmed and the primary sources are cited.
#' @export
Fano <- function(M, mutual_info, base_e = TRUE) {
  M <- as.integer(M); I <- as.numeric(mutual_info)
  if (M < 2L) stop("M must be at least 2")
  if (I < 0) stop("the mutual information must be non-negative")
  lg <- if (isTRUE(base_e)) log(M) else log(M, base = 2)
  l2 <- if (isTRUE(base_e)) log(2) else 1
  raw <- 1 - (I + l2) / lg
  .t1_result(bound = min(1, max(0, raw)), raw_bound = raw, log_M = lg,
             informative = as.numeric(raw > 0), M = as.numeric(M),
             method = "Fano inequality lower bound")
}
