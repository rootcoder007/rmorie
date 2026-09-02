# SPDX-License-Identifier: AGPL-3.0-or-later
#' Huber loss function
#'
#' The two branches agree in value and in slope at |r| = k, both giving
#' k^2/2 and slope k, which is what makes the loss convex and
#' continuously differentiable.
#'
#' Formula: rho(r) = r^2/2 if |r| <= k, else k(|r| - k/2).
#'
#' @param r Residuals.
#' @param k Tuning constant, positive.
#' @return List with \code{estimate} (total loss), \code{loss},
#'   \code{psi}, \code{mean_loss}, \code{k}, \code{n}, \code{method}.
#' @references Huber (1964), Annals of Mathematical Statistics
#'   35(1):73-101. \doi{10.1214/aoms/1177703732}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Hubrho(V)
Hubrho <- function(r, k = 1.345) {
  v <- .s03vec(r)
  if (length(v) == 0L) stop("huber_loss: r is empty")
  kv <- as.numeric(k)
  if (kv <= 0) stop("huber_loss: k must be positive")
  rho <- ifelse(abs(v) <= kv, v * v / 2, kv * (abs(v) - kv / 2))
  .t1_result(estimate = sum(rho), loss = rho,
             psi = pmax(-kv, pmin(kv, v)), mean_loss = mean(rho),
             k = kv, n = length(v),
             method = "rho(r) = r^2/2 for |r| <= k else k(|r| - k/2), Huber (1964)")
}
