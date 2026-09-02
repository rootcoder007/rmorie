# SPDX-License-Identifier: AGPL-3.0-or-later
#' How many missing studies would drag the effect down to trivial
#'
#' Rosenthal's count asks how many nulls would make the result
#' non-significant, which ties the answer to the sample size rather than
#' to the effect. Orwin's asks how many would push the pooled effect below
#' whatever magnitude the reader considers trivial, and it lets the
#' missing studies carry a non-zero effect of their own -- both of which
#' make the number mean something substantive rather than procedural.
#'
#' Formula: \code{N_fs = k (d_obs - d_crit)/(d_crit - d_fill)} -- Orwin
#' (1983) eq. (2).
#'
#' @param d_obs Observed pooled effect.
#' @param d_crit Effect size considered trivial.
#' @param d_filldraw Mean effect assumed for the unretrieved studies.
#' @param k Number of studies in the meta-analysis.
#' @return List with \code{Nfs}, \code{Nfs_ceiling}, \code{d_obs},
#'   \code{d_crit}, \code{d_fill}, \code{k}.
#' @references Orwin, R. G. (1983). Journal of Educational Statistics
#'   8(2):157-159. \doi{10.2307/1164923}.
#' @export
Maorw <- function(d_obs, d_crit, d_filldraw, k) {
  do <- as.numeric(d_obs)
  dc <- as.numeric(d_crit)
  df <- as.numeric(d_filldraw)
  kk <- as.numeric(k)
  if (kk < 1) stop("k must be at least one")
  if (abs(dc - df) < 1e-15) stop("d_crit and d_filldraw must differ")
  n <- kk * (do - dc) / (dc - df)
  .t1_result(Nfs = n, Nfs_ceiling = ceiling(n), d_obs = do, d_crit = dc,
             d_fill = df, k = kk, method = "Orwin's fail-safe N")
}
