# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pick the next item: maximum Fisher information at the current theta.
#'
#' Maximum-information selection is optimal for measurement and terrible
#' for item security. \code{exposure} applies a multiplicative control and
#' \code{information} is returned for every candidate so the cost is
#' visible. \code{administered} holds one-based indices; ties break on the
#' lowest index.
#'
#' Formula: I_j(theta) = dP^2 / (P (1 - P)) with
#'   P = c + (d - c) e/(1 + e), dP = D a e (d - c)/(1 + e)^2;
#'   choose argmax_j exposure_j I_j(theta) over unused j
#'
#' @param items Item parameters (a, b, c, d), one row per item.
#' @param theta Current ability estimate.
#' @param administered One-based indices already administered.
#' @param exposure Per-item multiplier in \[0, 1\].
#' @param D Scaling constant.
#' @return List with \code{next_item}, \code{information},
#'   \code{weighted}, \code{max_information}, \code{n_available},
#'   \code{J}.
#' @references Verified against the reference implementation in the CRAN
#'   package catR 3.17 (Magis & Raiche), functions Pi and Ii. catR
#'   implements the procedures of van der Linden & Glas (eds.), Elements
#'   of Adaptive Testing (2010), which this row cites; that volume was NOT
#'   obtainable, so the package source is used as the reference.
#'   Maximum-information selection is Birnbaum (1968); the exposure-control
#'   multiplier is the idea of Sympson & Hetter (1985), applied here in its
#'   simplest multiplicative form.
#' @export
#' @examples
#' items <- matrix(c(1, 0, 0.2, 0.95, 1.2, -0.5, 0.1, 0.98, 0.8, 1, 0.15, 0.9),
#'                 3, 4, byrow = TRUE)
#' Catnext(items, theta = 0.5)
Catnext <- function(items, theta, administered = NULL, exposure = NULL,
                    D = 1) {
  It <- as.matrix(items); J <- nrow(It)
  if (J < 1L) stop("the item bank must be non-empty")
  if (ncol(It) != 4L) stop("item rows must be (a, b, c, d)")
  theta <- as.numeric(theta); D <- as.numeric(D)
  used <- integer(0)
  if (!is.null(administered)) {
    used <- as.integer(.t1_vec(administered))
    if (any(used < 1L | used > J))
      stop("administered indices must lie in 1..J")
  }
  ex <- if (is.null(exposure)) rep(1, J) else .t1_vec(exposure)
  if (length(ex) != J) stop("exposure must have one entry per item")
  if (any(ex < 0 | ex > 1)) stop("exposure multipliers must lie in [0, 1]")
  e <- exp(D * It[, 1] * (theta - It[, 2]))
  p <- It[, 3] + (It[, 4] - It[, 3]) * e / (1 + e)
  p <- pmin(1 - 1e-10, pmax(1e-10, p))
  dp <- D * It[, 1] * e * (It[, 4] - It[, 3]) / (1 + e)^2
  info <- dp^2 / (p * (1 - p))
  wt <- ex * info
  avail <- setdiff(seq_len(J), used)
  if (!length(avail)) stop("every item has been administered")
  best <- avail[which.max(wt[avail])]
  .t1_result(next_item = as.numeric(best), information = info,
             weighted = wt, max_information = wt[best],
             n_available = as.numeric(length(avail)), J = as.numeric(J),
             method = "Maximum-information item selection with exposure control")
}
