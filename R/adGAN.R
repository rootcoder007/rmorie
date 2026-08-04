# SPDX-License-Identifier: AGPL-3.0-or-later
#' GANomaly anomaly score from encoder-decoder-encoder latents.
#'
#' A(x) = || G_E(x) - E(G(x)) ||_1 (eq. 5), rescaled over the test set as
#' s' = (s - min(S)) / (max(S) - min(S)) (eq. 6).
#'
#' @param z Bottleneck codes G_E(x), matrix (m x d) or vector.
#' @param zhat Codes E(G(x)) of the reconstructions, same shape.
#' @param threshold phi applied to the scaled scores.
#'
#' @return List with score, scaled, smin, smax, flagged, nflag, m, d.
#' @references Akcay, Atapour-Abarghouei and Breckon (2018),
#'   arXiv:1805.06725, equations (5) and (6), read from the ar5iv
#'   rendering of the arXiv source.
#' @export
Ganomscore <- function(z, zhat, threshold = 0.5) {
  Z <- .t1_mat(z); H <- .t1_mat(zhat)
  if (nrow(Z) != nrow(H) || ncol(Z) != ncol(H))
    stop("z and zhat must have the same shape")
  m <- nrow(Z); d <- ncol(Z)
  s <- rowSums(abs(Z - H))
  lo <- min(s); hi <- max(s); rng <- hi - lo
  sc <- if (rng == 0) rep(0, m) else (s - lo) / rng
  flg <- as.integer(sc > as.numeric(threshold))
  .t1_result(score = s, scaled = sc, smin = lo, smax = hi,
             flagged = flg, nflag = sum(flg), m = m, d = d,
             method = "GANomaly anomaly score (Akcay et al. 2018 eqs. 5-6)")
}
