# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kalman forward recursion driven by a model specification
#'
#' Identical recursion to the matrix-argument form in \code{Kalmf}; only
#' the interface differs, the system being passed as one object with
#' entries F, H, Q, R and optionally x0 and P0 -- the shape a fitted
#' model is usually kept in.  This function DELEGATES to \code{Kalmf}
#' rather than carrying a second copy of the recursion, because a second
#' copy would agree with the first at any tolerance and so could never
#' be told apart from correct work.
#'
#' Formula: see \code{Kalmf}; predict x_pred = F x, P_pred = F P F' + Q,
#'   then update with gain K = P_pred H' (H P_pred H' + R)^{-1}.
#'
#' @param y Observation matrix, one row per time point.
#' @param model Named list with F, H, Q, R and optionally x0, P0.
#' @return List with \code{estimate}, \code{state}, \code{loglik},
#'   \code{n}, \code{method}.
#' @references Kalman (1960), Transactions of the ASME, Journal of Basic
#'   Engineering 82(1):35-45. \doi{10.1115/1.3662552}
#' @export
Klmflt <- function(y, model) {
  Y <- .s03mat(y)
  if (nrow(Y) == 0L) stop("kalman_filter: y is empty")
  need <- function(k) {
    if (is.null(model[[k]])) stop(paste("kalman_filter: model is missing entry", k))
    model[[k]]
  }
  Fm <- .s03mat(need("F"))
  d <- nrow(Fm)
  x0 <- if (is.null(model$x0)) rep(0, d) else model$x0
  P0 <- if (is.null(model$P0)) diag(1, d) else model$P0
  res <- Kalmf(Y, Fm, need("H"), need("Q"), need("R"), x0, P0)
  .t1_result(estimate = res$estimate, state = res$state,
             cov = res$cov, predicted = res$predicted,
             predicted_cov = res$predicted_cov, loglik = res$loglik,
             n = res$n,
             method = "forward predict/update recursion, Kalman (1960); delegates to Kalmf")
}
