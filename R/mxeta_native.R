# Max-stable process simulation (de Haan spectral representation).
# Source: de Haan (1984), Ann. Probab. 12(4), 1194-1204, Sec. 1
# constructive example (fetched-wave3/A spectral representation for
# max-stable processes.pdf; read from the rendered scan).  Mirrors
# Python morie.fn.mxetA exactly: identical SplitMix64 uniform stream
# (gamma increments then site picks), identical exact truncation.

#' de Haan spectral max-stable simulation
#'
#' Y_t = sup_k f_t(T_k) X_k with X_k = 1/Gamma_k (Poisson intensity
#' dx/x^2) and T_k uniform; margins P(Y_t <= y) =
#' exp(-mean_s F[t,s]/y).  Simulation truncates exactly when
#' 1/Gamma_k max F < min_t Y_t.
#'
#' @param F Non-negative matrix (n_t x m): spectral functions on m
#'   uniform sites of [0, 1].
#' @param n_sim Number of independent realizations.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param max_points Safety cap on points per realization.
#' @return A list with elements \code{fields} (n_sim x n_t matrix),
#'   \code{scales}, \code{n_points}, \code{frechet_uniform},
#'   \code{seed}, \code{method}.
#' @references de Haan, L. (1984). A spectral representation for
#'   max-stable processes. Annals of Probability, 12(4), 1194-1204.
#' @export
morie_mxeta <- function(F, n_sim = 1, seed = 0, max_points = 100000) {
  Fm <- as.matrix(F)
  nt <- nrow(Fm)
  m <- ncol(Fm)
  if (any(Fm < 0)) stop("spectral functions must be non-negative")
  fmax <- max(Fm)
  if (fmax <= 0) stop("F must not be identically zero")
  scales <- rowMeans(Fm)
  e <- .ghc_rng(seed)
  fields <- matrix(0, n_sim, nt)
  counts <- integer(n_sim)
  for (r in seq_len(n_sim)) {
    y <- rep(0, nt)
    gamma <- 0
    k <- 0L
    while (k < max_points) {
      u <- .ghc_unif(e, 1)
      while (u <= 0) u <- .ghc_unif(e, 1)
      gamma <- gamma - log(u)
      x <- 1 / gamma
      if (x * fmax <= min(y) && min(y) > 0) break
      site <- min(floor(.ghc_unif(e, 1) * m), m - 1) + 1
      v <- Fm[, site] * x
      y <- pmax(y, v)
      k <- k + 1L
    }
    fields[r, ] <- y
    counts[r] <- k
  }
  fu <- exp(-sweep(1 / fields, 2, scales, "*"))
  list(fields = fields, scales = scales, n_points = counts,
       frechet_uniform = fu, seed = seed,
       method = "de Haan (1984) spectral max-stable simulation")
}
