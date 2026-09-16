# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample (empirical) semivariogram
#'
#' gammahat(h_j) = (1/(2 N_h)) sum_i (Z(s_i) - Z(s_i + h))^2 over the
#' pairs whose separation falls in distance class j.
#'
#' @param coords Locations, one row per observation.
#' @param z Observed values (residuals for a varying-mean model).
#' @param breaks Increasing distance-class boundaries, or NULL.
#' @param nbins Number of classes when breaks is NULL.
#' @param cutoff Largest separation; NULL uses one third of the maximum
#'   interpoint distance.
#'
#' @return List with gamma, np, dist, breaks, cutoff, n, npair.
#' @references Bivand, Pebesma and Gomez-Rubio (2013), Applied Spatial
#'   Data Analysis with R, 2nd edn, Equation (8.4), p. 218.  Read from
#'   the corpus PDF.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Svariog(V, V)
Svariog <- function(coords, z, breaks = NULL, nbins = 10, cutoff = NULL) {
  P <- .t1_mat(coords)
  z <- .t1_vec(z)
  n <- nrow(P)
  if (length(z) != n) stop("z must have one value per location")
  if (n < 2) stop("need at least two observations")
  k <- ncol(P)
  ii <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  d <- sqrt(rowSums((P[ii[, 1], , drop = FALSE] -
                     P[ii[, 2], , drop = FALSE])^2))
  g <- (z[ii[, 1]] - z[ii[, 2]])^2
  cut <- if (is.null(cutoff)) max(d) / 3 else as.numeric(cutoff)
  if (is.null(breaks)) {
    nb <- as.integer(nbins)
    if (nb < 1L) stop("nbins must be positive")
    br <- cut * (0:nb) / nb
  } else {
    br <- .t1_vec(breaks)
    if (any(diff(br) <= 0)) stop("breaks must be strictly increasing")
    nb <- length(br) - 1L
  }
  ssq <- numeric(nb)
  sdi <- numeric(nb)
  cnt <- integer(nb)
  keep <- which(d > br[1] & d <= br[nb + 1L])
  for (t in keep) {
    b <- 1L
    while (b < nb && d[t] > br[b + 1L]) b <- b + 1L
    ssq[b] <- ssq[b] + g[t]
    sdi[b] <- sdi[b] + d[t]
    cnt[b] <- cnt[b] + 1L
  }
  .t1_result(gamma = ifelse(cnt > 0L, ssq / (2 * cnt), NA_real_),
             np = cnt, dist = ifelse(cnt > 0L, sdi / cnt, NA_real_),
             breaks = br, cutoff = cut, n = n, npair = length(d),
             method = "Sample semivariogram (Bivand et al. 2013 eq. 8.4)")
}
