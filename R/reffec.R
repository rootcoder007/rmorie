# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effective reproduction number from susceptible depletion
#'
#' This is the susceptible-depletion Rt, not the renewal-equation Rt.
#' The two are routinely confused: the stub this replaces cited Cori et
#' al. (2013) for \code{R0 S / N}, which is not the estimator that paper
#' defines. Cori's Rt comes from the incidence curve and a
#' serial-interval distribution and lives elsewhere in this package
#' (\code{Rtrenew}). DUPMAP.tsv lists this module as a duplicate of
#' \code{epirf}; it is not, for the same reason.
#'
#' The herd-immunity threshold falls out: Rt crosses one when the
#' susceptible fraction reaches \code{1 / R0}.
#'
#' Formula: \code{Rt = R0 S / N}; growing iff \code{Rt > 1}.
#'
#' @param R0 Basic reproduction number, non-negative.
#' @param S Current susceptible count in [0, N].
#' @param N Population size, positive.
#' @return List with \code{estimate}, \code{Rt}, \code{growing},
#'   \code{susceptible_fraction}, \code{herd_immunity_threshold}.
#' @references Diekmann, O., Heesterbeek, J. A. P. & Metz, J. A. J.
#'   (1990). On the definition and the computation of the basic
#'   reproduction ratio R0 in models for infectious diseases in
#'   heterogeneous populations. Journal of Mathematical Biology
#'   28(4):365-382. \doi{10.1007/BF00178324}.
#' @export
Reffec <- function(R0, S, N) {
  R0 <- as.numeric(R0); S <- as.numeric(S); N <- as.numeric(N)
  if (R0 < 0) stop("Reffec: R0 must be non-negative")
  if (N <= 0) stop("Reffec: N must be positive")
  if (S < 0 || S > N) stop("Reffec: S must lie in [0, N]")
  frac <- S / N
  rt <- R0 * frac
  .t1_result(estimate = rt, Rt = rt, growing = if (rt > 1) 1 else 0,
             susceptible_fraction = frac,
             herd_immunity_threshold = if (R0 > 0) 1 - 1 / R0 else NA_real_,
             method = "Effective reproduction number Rt = R0 S / N")
}
