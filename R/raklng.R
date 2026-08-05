# SPDX-License-Identifier: AGPL-3.0-or-later
#' Raking ratio estimation by iterative proportional fitting
#'
#' Each pass rescales within one margin, which breaks the previous
#' margin, which is why this iterates rather than solving once. It
#' converges when the margins are consistent (a common total) and no
#' level with a positive target is empty in the sample; both conditions
#' are checked rather than left to diverge silently.
#'
#' Formula: repeat over margins A, B, ...:
#' \code{w_i <- w_i m^A_h / sum_{j in h} w_j} for every level h, until
#' every margin matches its target.
#'
#' @param y Observed values (used only for the raked estimate).
#' @param weights Starting weights, positive.
#' @param margins List of margins; each is \code{list(labels, targets)}
#'   where \code{targets} is a named numeric vector.
#' @param tol Convergence tolerance on the largest margin discrepancy.
#' @param max_iter Maximum passes.
#' @return List with \code{estimate}, \code{weights}, \code{iterations},
#'   \code{max_margin_error}, \code{N}, \code{n}.
#' @references Deming, W. E. & Stephan, F. F. (1940). On a least squares
#'   adjustment of a sampled frequency table when the expected marginal
#'   totals are known. Annals of Mathematical Statistics 11(4):427-444.
#'   \doi{10.1214/aoms/1177731829}.
#' @export
Raklng <- function(y, weights, margins, tol = 1e-12, max_iter = 200) {
  y <- as.numeric(unlist(y)); w <- as.numeric(unlist(weights))
  if (length(y) == 0L) stop("Raklng: y is empty")
  if (length(w) != length(y)) stop("Raklng: weights must have one entry per observation")
  if (any(w <= 0)) stop("Raklng: weights must be positive")
  if (length(margins) == 0L) stop("Raklng: at least one margin is required")
  marg <- lapply(margins, function(m) {
    labs <- as.character(unlist(m[[1]])); tgt <- m[[2]]
    if (length(labs) != length(y))
      stop("Raklng: margin labels must have one entry per observation")
    tt <- as.numeric(tgt); names(tt) <- names(tgt)
    if (!all(labs %in% names(tt))) stop("Raklng: no target for a level")
    if (any(tt < 0)) stop("Raklng: margin targets must be non-negative")
    list(labs = labs, tt = tt)
  })
  tot0 <- sum(marg[[1]]$tt)
  for (m in marg) if (abs(sum(m$tt) - tot0) > 1e-8 * max(1, abs(tot0)))
    stop("Raklng: margins have inconsistent totals")
  it <- 0L; err <- Inf
  for (it in seq_len(as.integer(max_iter))) {
    for (m in marg) for (k in names(m$tt)) {
      idx <- which(m$labs == k)
      cur <- sum(w[idx])
      if (cur <= 0) {
        if (m$tt[[k]] > 0)
          stop(paste0("Raklng: level ", k, " has no sampled unit but a positive target"))
        next
      }
      w[idx] <- w[idx] * (m$tt[[k]] / cur)
    }
    err <- 0
    for (m in marg) for (k in names(m$tt)) {
      cur <- sum(w[m$labs == k])
      err <- max(err, abs(cur - m$tt[[k]]))
    }
    if (err <= tol) break
  }
  .t1_result(estimate = sum(w * y) / sum(w), weights = w, iterations = it,
             max_margin_error = err, N = sum(w), n = length(y),
             method = "Raking ratio / iterative proportional fitting")
}
