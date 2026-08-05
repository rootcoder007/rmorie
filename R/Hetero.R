# SPDX-License-Identifier: AGPL-3.0-or-later
#' Heterotrait-monotrait ratio of correlations (HTMT)
#'
#' The averages run over distinct indicator pairs.  Discriminant
#' validity is questioned when the ratio exceeds 0.85 (or the more
#' lenient 0.90); the criterion is a decision, so the tests report a
#' confusion matrix rather than an average.
#'
#' Formula: HTMT = mean heterotrait-heteromethod correlation /
#'   sqrt(mean within-i correlation * mean within-j correlation).
#'
#' @param X Observations by indicators matrix.
#' @param construct_assignment One construct label per indicator.
#' @param threshold Ratio above which validity is questioned.
#' @return List with \code{estimate} (largest ratio), \code{htmt},
#'   \code{pair_first}, \code{flagged}, \code{threshold},
#'   \code{discriminant_validity}, \code{n}, \code{method}.
#' @references Henseler, Ringle and Sarstedt (2015), A new criterion for
#'   assessing discriminant validity in variance-based structural
#'   equation modeling, Journal of the Academy of Marketing Science
#'   43(1):115-135, eq. (6). \doi{10.1007/s11747-014-0403-8}
#' @export
Hetero <- function(X, construct_assignment, threshold = 0.85) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n < 3L) stop("htmt_ratio: need at least three observations")
  p <- ncol(M)
  g <- as.integer(.s03vec(construct_assignment))
  if (length(g) != p) stop("htmt_ratio: one construct label per indicator is required")
  groups <- sort(unique(g))
  if (length(groups) < 2L) stop("htmt_ratio: need at least two constructs")
  for (lab in groups) if (sum(g == lab) < 2L) stop("htmt_ratio: every construct needs at least two indicators")
  R <- matrix(0, p, p)
  for (i in seq_len(p)) for (j in seq_len(p)) R[i, j] <- .s03corr(M[, i], M[, j])
  ratios <- numeric(0); flags <- integer(0); pair1 <- NULL
  for (ai in seq_len(length(groups) - 1L)) for (bi in seq(ai + 1L, length(groups))) {
    A <- which(g == groups[ai]); B <- which(g == groups[bi])
    het <- abs(as.numeric(R[A, B]))
    wa <- abs(R[A, A][upper.tri(diag(length(A)))])
    wb <- abs(R[B, B][upper.tri(diag(length(B)))])
    den <- sqrt(mean(wa) * mean(wb))
    v <- if (den == 0) Inf else mean(het) / den
    if (is.null(pair1)) pair1 <- c(groups[ai], groups[bi])
    ratios <- c(ratios, v); flags <- c(flags, as.integer(v > as.numeric(threshold)))
  }
  worst <- max(ratios)
  .t1_result(estimate = worst, htmt = ratios, pair_first = pair1, flagged = flags,
             threshold = as.numeric(threshold),
             discriminant_validity = as.integer(worst <= as.numeric(threshold)),
             n = n, method = "HTMT eq. (6) of Henseler, Ringle & Sarstedt (2015)")
}
