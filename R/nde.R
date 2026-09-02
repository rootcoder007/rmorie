# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pearl's natural direct effect, estimated from a linear SEM
#'
#' \code{NDE = E[Y(1, M(0))] - E[Y(0, M(0))]}. The contrast is delegated
#' to \code{Ndeff} (module \code{tmlnde}); what this module adds is the
#' step \code{tmlnde} deliberately refuses to take -- producing the two
#' cross-world quantities from data, which requires a model.
#'
#' The model is the standard two-equation linear SEM with an
#' exposure-mediator interaction (VanderWeele 2015, eq. 2.9-2.11):
#' \code{M = b0 + b1 X + e_M} and
#' \code{Y = c0 + c1 X + c2 M + c3 X M + e_Y}. Under sequential
#' ignorability \code{E[M(0)] = b0}, so
#' \code{E[Y(x, M(0))] = c0 + c1 x + c2 b0 + c3 x b0},
#' \code{NDE = c1 + c3 b0} and \code{NIE = b1 (c2 + c3)} contrasting
#' \code{x = 1} against \code{x = 0}. \code{NDE + NIE} is the total effect
#' exactly. With no interaction the NDE collapses to the coefficient on X
#' in the regression of Y on X and M.
#'
#' No standard error is reported: the two cross-world means are point
#' predictions from the fitted SEM, not a sample of contrasts.
#'
#' @param X Exposure.
#' @param M Mediator.
#' @param Y Outcome.
#' @return List with estimate (NDE), se, nde, nie, total, mean_y10,
#'   mean_y00, b0, b1, c0, c1, c2, c3, n.
#' @references Pearl (2001), Proc. 17th UAI, 411-420; VanderWeele (2015),
#'   Explanation in Causal Inference, OUP. Standard published form of the
#'   linear-SEM decomposition; neither source was in the local corpus, so
#'   the equations are stated in full above.
#' @export
#' @examples
#' Nde(X = c(1, 2, 3, 4, 5, 6, 7, 8), M = c(1, 2, 3, 4, 5, 6, 7, 8), Y = c(1, 2, 3, 4, 5, 6, 7, 8))
Nde <- function(X, M, Y) {
  x <- .t1_vec(X); m <- .t1_vec(M); y <- .t1_vec(Y); n <- length(x)
  if (n == 0L) stop("X is empty")
  if (length(m) != n || length(y) != n) {
    stop("X, M and Y must have the same length")
  }
  if (n < 4L) stop("need at least 4 observations to fit the SEM")
  b <- .t1_lstsq(cbind(1, x), m)$beta
  b0 <- b[1]; b1 <- b[2]
  cf <- .t1_lstsq(cbind(1, x, m, x * m), y)$beta
  c0 <- cf[1]; c1 <- cf[2]; c2 <- cf[3]; c3 <- cf[4]
  y10 <- c0 + c1 + c2 * b0 + c3 * b0
  y00 <- c0 + c2 * b0
  r <- Ndeff(y10, y00)
  nie <- b1 * (c2 + c3)
  .t1_result(estimate = r$estimate, se = r$se, nde = r$estimate,
             nie = nie, total = r$estimate + nie,
             mean_y10 = r$mean_y10, mean_y00 = r$mean_y00,
             b0 = b0, b1 = b1, c0 = c0, c1 = c1, c2 = c2, c3 = c3,
             n = n, method = "Natural direct effect (linear SEM, Pearl 2001)")
}
