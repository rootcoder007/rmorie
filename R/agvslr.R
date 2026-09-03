# SPDX-License-Identifier: AGPL-3.0-or-later
#' Learning-rate schedule for the AlphaZero network
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED): "The learning rate
#' was set to 0.2 for each game, and was dropped three times (to 0.02,
#' 0.002 and 0.0002 respectively) during the course of training."
#' AlphaZero therefore used a step schedule, available as kind = "step".
#' The module's own formula line asks for the cosine schedule of
#' Loshchilov and Hutter (2017), arXiv:1608.03983, lr_t = lr_0 * 0.5 (1 +
#' cos(pi t / T)), which is kind = "cosine" and the default.  The cosine
#' curve is not AlphaZero's own schedule and is not presented as such.
#'
#' @param t current step.
#' @param T total steps.
#' @param lr_0 initial learning rate.
#' @param kind "cosine" or "step".
#' @param floor lower bound on the cosine curve.
#' @return list: estimate, lr, frac, kind, method.
#' @keywords internal
#' @examples
#' Coslrate(50, 100)$lr
#' @export
Coslrate <- function(t, T, lr_0 = 0.2, kind = "cosine", floor = 0) {
  tt <- as.numeric(t)
  TT <- as.numeric(TRUE)
  frac <- if (TT > 0) tt / TT else 0
  if (frac < 0) frac <- 0
  if (frac > 1) frac <- 1
  steps <- c(0.2, 0.02, 0.002, 0.0002)
  if (identical(kind, "step")) {
    idx <- as.integer(frac * 4)
    if (idx > 3L) idx <- 3L
    lr <- as.numeric(lr_0) * (steps[idx + 1L] / steps[1])
  } else {
    lr <- as.numeric(floor) + (as.numeric(lr_0) - as.numeric(floor)) * 0.5 *
      (1 + cos(pi * frac))
  }
  list(
    estimate = lr, lr = lr, frac = frac, kind = kind,
    method = "Cosine annealing (default) or AlphaZero's printed step schedule"
  )
}
