# SPDX-License-Identifier: AGPL-3.0-or-later
#' Experimental selection correction: an RCT-validated control function
#'
#' The coefficients of the secondary outcome are estimated on the
#' experimental sample, where treatment is randomised, and the residual
#' \code{alpha = Y^S - W tau^S - X gamma^S} is then formed for the
#' observational units; the primary outcome is regressed there on
#' treatment, covariates and that residual, and the coefficient on
#' treatment is the selection-corrected effect.  Fitting both stages on
#' one sample makes \code{alpha} orthogonal to the design by
#' construction, so the correction is then identically zero.
#'
#' @param y_obs Primary outcome.
#' @param y_rct Secondary outcome, observed in both samples.
#' @param D Binary treatment.
#' @param X Optional pre-treatment covariates.
#' @param G 1 for experimental rows, 0 for observational rows;
#'   \code{NULL} puts every row in both.
#' @return List with \code{estimate}, \code{tau_esc}, \code{tau_naive},
#'   \code{correction}, \code{delta}, \code{tau_secondary},
#'   \code{alpha_sd}, \code{n_exp}, \code{n_obs}, \code{n}.
#' @references Athey, S., Chetty, R. and Imbens, G. W. (2020/2025). The
#'   experimental selection correction estimator. arXiv:2006.09676,
#'   equations (2.3) and (2.4), page 10.
#' @export
#' @examples
#' set.seed(1)
#' r <- Drrct(y_obs = rnorm(10), y_rct = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Drrct <- function(y_obs, y_rct, D, X = NULL, G = NULL) {
  yp <- .s03vec(y_obs); ys <- .s03vec(y_rct); dv <- .s03vec(D)
  n <- length(yp)
  if (n == 0L) stop("Drrct: empty input, y_obs has no observations")
  if (length(ys) != n || length(dv) != n)
    stop("Drrct: y_obs, y_rct and D must have the same length")
  Z <- .s03design(X, n)
  W <- cbind(1, dv, Z[, -1L, drop = FALSE])
  if (is.null(G)) {
    ie <- seq_len(n); io <- seq_len(n)
  } else {
    gv <- .s03vec(G)
    if (length(gv) != n) stop("Drrct: G must have the same length as y_obs")
    ie <- which(gv >= 0.5); io <- which(gv < 0.5)
    if (!length(ie) || !length(io))
      stop("Drrct: G must mark both an experimental and an observational subsample")
  }
  for (idx in list(ie, io)) {
    s <- sum(dv[idx])
    if (s <= 0 || s >= length(idx))
      stop("Drrct: each subsample must contain both arms")
  }
  bs <- .s03lstsq(W[ie, , drop = FALSE], ys[ie])
  alpha <- ys[io] - .s03matvec(W[io, , drop = FALSE], bs)
  bn <- .s03lstsq(W[io, , drop = FALSE], yp[io])
  Wa <- cbind(W[io, , drop = FALSE], alpha)
  be <- .s03lstsq(Wa, yp[io])
  .t1_result(estimate = be[2L], tau_esc = be[2L], tau_naive = bn[2L],
             correction = be[2L] - bn[2L], delta = be[length(be)],
             tau_secondary = bs[2L],
             alpha_sd = if (length(alpha) > 1L) .s03sd(alpha) else 0,
             n_exp = length(ie), n_obs = length(io), n = n,
             method = "DR-DiD with RCT side data")
}
