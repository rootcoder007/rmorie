# SPDX-License-Identifier: AGPL-3.0-or-later
#' Direct and spillover ATT under an exposure mapping
#'
#' With a binary own-treatment \code{D} and a binary neighbourhood
#' exposure \code{E} the three exposure conditions are direct
#' (\code{D = 1}), spillover (\code{D = 0, E = 1}) and control
#' (\code{D = 0, E = 0}).  Each contrast against the shared control
#' condition is estimated by the doubly robust moment of Sant'Anna and
#' Zhao (2020), equation (2.6), restricted to the two conditions being
#' compared, so the two contrasts are on the same scale.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Own binary treatment.
#' @param X Optional baseline covariates.
#' @param exposure Neighbourhood exposure; any strictly positive value
#'   counts as exposed.
#' @return List with \code{estimate}, \code{att_direct},
#'   \code{att_spillover}, \code{se_direct}, \code{se_spillover},
#'   \code{total}, \code{n_direct}, \code{n_spill}, \code{n_control},
#'   \code{n}.
#' @references Aronow, P. M. and Samii, C. (2017). Annals of Applied
#'   Statistics 11(4), 1912-1947.  Sant'Anna, P. H. C. and Zhao, J.
#'   (2020). Journal of Econometrics 219(1), 101-122.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Drspr(V, V)
Drspr <- function(y, D, X = NULL, exposure = NULL) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drspr: empty input, y has no observations")
  if (length(dv) != n) stop("Drspr: y and D must have the same length")
  ex <- if (is.null(exposure)) rep(0, n) else .s03vec(exposure)
  if (length(ex) != n) stop("Drspr: exposure must have the same length as y")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  e <- as.numeric(ex > 0)
  ctrl <- which(dv < 0.5 & e < 0.5)
  dirt <- which(dv >= 0.5)
  spil <- which(dv < 0.5 & e >= 0.5)
  fitpair <- function(a, b) {
    if (!length(a) || !length(b) || length(a) + length(b) < 3L)
      return(c(NaN, NaN))
    idx <- c(a, b); lab <- c(rep(1, length(a)), rep(0, length(b)))
    xm <- if (is.null(Xr)) NULL else Xr[idx, , drop = FALSE]
    f <- .s03drdid(yv[idx], lab, xm)
    c(f$tau, f$se)
  }
  d1 <- fitpair(dirt, ctrl); s1 <- fitpair(spil, ctrl)
  tot <- if (is.na(d1[1]) || is.na(s1[1])) NaN else d1[1] + s1[1]
  .t1_result(estimate = d1[1], att_direct = d1[1], att_spillover = s1[1],
             se_direct = d1[2], se_spillover = s1[2], total = tot,
             n_direct = length(dirt), n_spill = length(spil),
             n_control = length(ctrl), n = n,
             method = "DR-DiD with spillover")
}
