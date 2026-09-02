# SPDX-License-Identifier: AGPL-3.0-or-later
#' Criterion-function set estimate for moment inequalities.
#'
#' Q_n(theta) = sum_j \[max(mbar_j/sigma_j, 0)\]^2 and
#' C_n = {theta : n Q_n(theta) <= cutoff}.
#'
#' @param mbar Sample moment means, one row per candidate theta.
#' @param se Moment standard deviations, length J or the shape of mbar.
#' @param n Sample size behind the moment means.
#' @param cutoff Level-set cutoff; NULL uses min(n Q_n).
#'
#' @return List with Q, nQ, argmin (zero-based), minQ, inset, nin,
#'   cutoff, g, J.
#' @references Chernozhukov, Hong and Tamer (2007), Econometrica 75(5),
#'   1243-1284, Sect. 2.  Standard published form; the article is not in
#'   the local corpus and was not read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Qcritset(V)
Qcritset <- function(mbar, se = NULL, n = 1, cutoff = NULL) {
  M <- .t1_mat(mbar); g <- nrow(M); J <- ncol(M)
  if (is.null(se)) {
    S <- matrix(1, g, J)
  } else {
    sv <- .t1_mat(se)
    if (nrow(sv) == 1L && ncol(sv) == J) {
      S <- matrix(rep(as.numeric(sv), each = g), nrow = g)
    } else if (nrow(sv) == g && ncol(sv) == J) {
      S <- sv
    } else if (ncol(sv) == 1L && nrow(sv) == J) {
      S <- matrix(rep(as.numeric(sv), each = g), nrow = g)
    } else stop("se must be length J or the shape of mbar")
  }
  if (any(S <= 0)) stop("standard deviations must be strictly positive")
  n <- as.numeric(n)
  if (n <= 0) stop("n must be positive")
  Q <- rowSums(pmax(M / S, 0)^2)
  nQ <- n * Q
  mn <- min(nQ); am <- which.min(nQ) - 1L
  cut <- if (is.null(cutoff)) mn else as.numeric(cutoff)
  ins <- as.integer(nQ <= cut)
  .t1_result(Q = Q, nQ = nQ, argmin = am, minQ = mn, inset = ins,
             nin = sum(ins), cutoff = cut, g = g, J = J,
             method = "Criterion-function set estimate (Chernozhukov-Hong-Tamer 2007)")
}
