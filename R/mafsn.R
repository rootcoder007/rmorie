# SPDX-License-Identifier: AGPL-3.0-or-later
#' How many missing null studies would sink a significant result
#'
#' The file-drawer count answers a question about publication bias with a
#' number, which is its appeal and also its flaw: it assumes the missing
#' studies average exactly zero effect, which no plausible selection
#' mechanism produces. Read it as an upper bound on fragility, never as
#' evidence that bias is absent.
#'
#' Formula: \code{N_fs = (sum z_i)^2 / z_alpha^2 - k} with \code{z_alpha}
#' the one-tailed critical value -- Rosenthal (1979) eq. (2).
#'
#' @param z_scores One-tailed z statistics of the included studies.
#' @param alpha One-tailed level.
#' @return List with \code{Nfs}, \code{z_combined}, \code{z_alpha},
#'   \code{k}.
#' @references Rosenthal, R. (1979). Psychological Bulletin 86(3):638-641.
#'   \doi{10.1037/0033-2909.86.3.638}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mafsn(V)
Mafsn <- function(z_scores, alpha = 0.05) {
  z <- as.numeric(z_scores)
  k <- length(z)
  if (k == 0L) stop("no studies")
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1) stop("alpha must lie strictly between 0 and 1")
  za <- .s03qnorm(1 - a)
  s <- sum(z)
  .t1_result(Nfs = s^2 / za^2 - k, z_combined = s / sqrt(k), z_alpha = za,
             k = k, method = "Rosenthal's fail-safe N")
}
