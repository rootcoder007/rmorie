# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adaptive choice of the resample size m in the m-out-of-n bootstrap
#'
#' Bickel, P. J. and Sakov, A. (2008), "On the choice of m in the m out of n
#' bootstrap and confidence bounds for extrema", Statistica Sinica 18(3),
#' 967-985.  The rule is stated verbatim on page 971 and was read from the
#' journal PDF: (1) consider m_j = ceil(q^j n), j = 0, 1, 2, ..., 0 < q < 1;
#' (2) for each m_j find the bootstrap law L*_(m_j, n); (3) with rho a metric
#' consistent with convergence in law, m_hat = argmin_(m_j) rho(L*_(m_j,n),
#' L*_(m_(j+1),n)), and "if the difference is minimized for a few values of
#' m_j, then pick the LARGEST among them"; (4) estimate L by L*_(m_hat, n).
#' The paper's own choice of rho, and the one its proofs are for, is the
#' Kolmogorov sup distance sup_x |F(x) - G(x)|; that is what is used here.
#'
#' The stub this replaces labelled the method "min-volatility", which is a
#' different rule (Politis, Romano and Wolf 1999, the standard deviation of
#' interval endpoints over a window of neighbouring m).  The citation on the
#' stub is Bickel and Sakov, so the Bickel-Sakov rule is what is implemented;
#' the vol_curve key is kept and carries the KS discrepancies
#' rho(L_j, L_(j+1)), which is the quantity the rule minimises.
#'
#' The law compared is that of the root sqrt(m)(theta*_m - theta_hat).  Ties
#' are broken toward the largest m exactly as the paper directs.  Anchor: on a
#' constant sample every root is zero at every m, so every KS discrepancy is
#' zero, the argmin is a full tie, and the rule must return the LARGEST grid
#' value.  The stream is restarted at the same seed for every grid point so
#' that the comparison across m is not confounded by different noise.
#'
#' @param x the observed sample.
#' @param stat statistic of a sample; NULL uses the mean.
#' @param m_grid the grid, largest first; NULL builds ceil(q^j n).
#' @param B bootstrap replicates per grid point.
#' @param seed seed for the shared deterministic stream.
#' @param q grid ratio, 0 < q < 1; ignored when m_grid is given.
#' @return list: m_star, vol_curve, m_grid, min_ks, theta_hat, se_star, n, B,
#'   estimate, method.
#' @keywords internal
#' @examples
#' Btsubrho(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), B = 40)$m_star
#' @export
Btsubrho <- function(x, stat = NULL, m_grid = NULL, B = 200, seed = 1, q = 0.75) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_subsample_rate: need at least two observations")
  if (as.integer(B) < 2L) stop("boot_subsample_rate: need at least two replicates")
  if (is.null(m_grid)) {
    qq <- as.numeric(q)
    if (!(qq > 0 && qq < 1)) stop("boot_subsample_rate: q must lie strictly between 0 and 1")
    m_grid <- integer(0)
    j <- 0L
    pw <- 1
    repeat {
      mj <- as.integer(ceiling(pw * n))
      if (mj < 2L) break
      if (length(m_grid) == 0L || mj != m_grid[length(m_grid)]) m_grid <- c(m_grid, mj)
      pw <- pw * qq
      j <- j + 1L
      if (j > 200L) break
    }
  }
  m_grid <- as.integer(m_grid)
  if (length(m_grid) < 2L) stop("boot_subsample_rate: need at least two grid values")
  for (mj in m_grid) if (!(mj >= 1L && mj <= n))
    stop("boot_subsample_rate: every grid value must lie in 1..n")
  f <- if (is.null(stat)) .s03mean else stat
  th <- as.numeric(f(xx))
  laws <- vector("list", length(m_grid))
  for (k in seq_along(m_grid)) {
    mj <- m_grid[k]
    g <- .t1_lcg(seed)
    r <- numeric(as.integer(B))
    for (b in seq_len(as.integer(B))) {
      idx <- .btmoutn_idx(g, n, mj)
      r[b] <- sqrt(mj) * (as.numeric(f(xx[idx])) - th)
    }
    laws[[k]] <- r
  }
  vol <- vapply(seq_len(length(m_grid) - 1L),
                function(i) .btsubrho_ks(laws[[i]], laws[[i + 1L]]), 0)
  best <- min(vol)
  k <- which(vol <= best)[1]
  ms <- m_grid[k]
  list(m_star = ms, vol_curve = vol, m_grid = m_grid, min_ks = best,
       theta_hat = th, se_star = .s03sd(laws[[k]], 1L) / sqrt(n),
       n = n, B = as.integer(B), estimate = th,
       method = "Bickel and Sakov (2008) Statist. Sinica 18(3):967-985, rule on p.971")
}

#' @noRd
.btsubrho_ks <- function(a, b) {
  pts <- sort(unique(c(a, b)))
  na <- length(a)
  nb <- length(b)
  sa <- sort(a)
  sb <- sort(b)
  d <- 0
  ia <- 0L
  ib <- 0L
  for (t in pts) {
    while (ia < na && sa[ia + 1L] <= t) ia <- ia + 1L
    while (ib < nb && sb[ib + 1L] <= t) ib <- ib + 1L
    e <- abs(ia / na - ib / nb)
    if (e > d) d <- e
  }
  d
}
