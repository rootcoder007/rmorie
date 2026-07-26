#' Correlation dimension (Grassberger-Procaccia) -- Rangayyan Ch 7
#'
#' Slope of \eqn{\log C(r)}{log C(r)} vs \eqn{\log r}{log r} in the middle scaling region.
#'
#' @param x Numeric vector.
#' @param m Embedding dimension (default 3).
#' @param tau Embedding lag (default 1).
#' @param n_r Number of radii (default 20).
#' @return Named list `D2`, `log_r`, `log_C`, `m`, `tau`.
#' @references Grassberger & Procaccia (1983), Physica D 9:189.
#' @export
#' @examples
#' set.seed(0)
#' rgcrl(rnorm(200), m = 3, tau = 1, n_r = 15)$D2
rgcrl <- function(x, m = 3L, tau = 1L, n_r = 20L) {
  N <- length(x)
  M <- N - (m - 1L) * tau
  if (M < 10) stop("Series too short for embedding.")
  Y <- matrix(0, nrow = M, ncol = m)
  for (i in seq_len(m)) Y[, i] <- x[((i - 1L) * tau + 1L):((i - 1L) * tau + M)]
  dist <- as.numeric(stats::dist(Y))
  if (length(dist) == 0) stop("No pairwise distances.")
  pos <- dist[dist > 0]
  rmin <- max(if (length(pos)) min(pos) else 1e-12, 1e-12)
  rmax <- max(dist)
  rs <- 10^seq(log10(rmin), log10(rmax), length.out = n_r)
  C <- vapply(rs, function(r) mean(dist <= r), numeric(1))
  ## Require a minimum pair count per radius, not merely C > 0. rmin is the
  ## smallest pairwise distance, so C at the low end rests on one or two
  ## pairs -- noise, not an estimate. It also made D2 depend on the SCALE of
  ## the input: an affine rescale perturbs the boundary pair in or out,
  ## changing how many radii pass the mask, shifting the fit window and
  ## moving the slope. Measured in Python: 2.856 -> 2.753 under x -> 100x,
  ## on a quantity that is a dimension and must be invariant.
  n_pairs <- C * length(dist)
  mask <- n_pairs >= 10 & is.finite(C)
  log_r <- log(rs[mask])
  log_C <- log(C[mask])
  if (length(log_r) < 3) {
    D2 <- NA_real_
  } else {
    n <- length(log_r)
    ## lo/hi are computed on Python's 0-based, half-open convention so the two
    ## languages regress over the SAME radii. R's `lo:hi` is 1-based and
    ## closed, so the direct translation would start one radius lower and take
    ## one extra point -- measured divergence D2 2.7380 (R) vs 2.7055 (Python)
    ## on 600 Gaussian samples. `(lo + 1L):hi` is the exact equivalent.
    lo <- max(1L, n %/% 5L)
    hi <- max(lo + 2L, n - n %/% 5L)
    win <- (lo + 1L):hi
    D2 <- unname(stats::coef(stats::lm(log_C[win] ~ log_r[win]))[2])
  }
  list(D2 = D2, log_r = log_r, log_C = log_C, m = m, tau = tau)
}

#' @rdname rgcrl
#' @keywords internal
#' @export
morie_rangayyan_correlation_dimension <- rgcrl
