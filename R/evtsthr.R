# SPDX-License-Identifier: AGPL-3.0-or-later

#' Threshold selection by local variance of the GPD estimates
#'
#' Formula: argmin_u Var(log sigma*_u, xi_u)
#'
#' A GPD fitted above u0 implies, for any higher u,
#' sigma_u = sigma_u0 + xi (u - u0), so the MODIFIED scale
#' sigma*_u = sigma_u - xi u and the shape xi are both constant in u
#' once the model holds.  The threshold chosen is the one whose forward
#' window of fits has the smallest combined variance of log sigma* and
#' xi -- the first place the parameters stop drifting.
#'
#' @param x Sample.
#' @param u_grid Candidate thresholds, or NULL for the 50th to 90th
#'   sample percentiles in nine steps.
#' @param window Number of consecutive thresholds per variance.
#' @return List with \code{u_star}, \code{score}, \code{estimate},
#'   \code{u}, \code{scores}, \code{xi}, \code{mod_scale}, \code{n},
#'   \code{method}.
#' @references Northrop & Coleman (2014), Extremes 17(2):289-303.
#' @export
#' @examples
#' set.seed(1)
#' Evtsthr(rexp(200))
Evtsthr <- function(x, u_grid = NULL, window = 3) {
  x <- .s03vec(x)
  n <- length(x)
  if (n < 10L) stop("need at least ten observations to select a threshold")
  if (is.null(u_grid)) {
    u_grid <- vapply(0:8, function(i) .s03quantile7(x, 0.5 + 0.05 * i), 0)
  } else u_grid <- .s03vec(u_grid)
  window <- as.integer(window)
  if (window < 2L) stop("window must be at least 2")
  if (length(u_grid) < window) stop("u_grid is shorter than the window")
  us <- c(); xis <- c(); mods <- c()
  for (u in u_grid) {
    if (sum(x > u) < 5L) next
    f <- Evpot(x, u)
    us <- c(us, u); xis <- c(xis, f$xi); mods <- c(mods, f$modified_scale)
  }
  if (length(us) < window) stop("too few usable thresholds after filtering")
  scores <- numeric(length(us) - window + 1L)
  for (i in seq_along(scores)) {
    xw <- xis[i:(i + window - 1L)]
    mw <- mods[i:(i + window - 1L)]
    if (any(mw <= 0)) { scores[i] <- Inf; next }
    scores[i] <- .s03var(log(mw), 1L) + .s03var(xw, 1L)
  }
  best <- 1L
  if (length(scores) > 1L) for (i in 2:length(scores))
    if (scores[i] < scores[best]) best <- i
  .t1_result(u_star = us[best], score = scores[best], estimate = us[best],
             u = us, scores = scores, xi = xis, mod_scale = mods, n = n,
             method = "threshold selection by local variance of GPD estimates")
}
