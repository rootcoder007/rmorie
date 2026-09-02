# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kulldorff spatial scan statistic
#'
#' max_z (O_z/E_z)^\{O_z\} ((O_+ - O_z)/(E_+ - E_z))^\{O_+ - O_z\}, reported
#' on the log scale and restricted by default to windows whose internal
#' risk exceeds the overall risk.
#'
#' @param O Observed counts per region.
#' @param E Expected counts per region, strictly positive.
#' @param zones List of candidate windows, each a vector of zero-based
#'   region indices.
#' @param highonly Score only windows with elevated internal risk.
#'
#' @return List with loglr, best (zero-based), maxloglr, bestzone, Oz,
#'   Ez, rrin, rrout, Otot, Etot, nzone.
#' @references Bivand, Pebesma and Gomez-Rubio (2013), Equation (10.3),
#'   p. 354, after Kulldorff and Nagarwalla (1995).  Read from the corpus
#'   PDF.
#' @export
#' @examples
#' Scanstat(O = c(1, 2, 3, 4, 5, 6, 7, 8), E = c(1, 2, 3, 4, 5, 6, 7, 8), zones = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
Scanstat <- function(O, E, zones, highonly = TRUE) {
  O <- .t1_vec(O)
  E <- .t1_vec(E)
  n <- length(O)
  if (length(E) != n) stop("O and E must have the same length")
  if (any(E <= 0)) stop("expected counts must be strictly positive")
  if (any(O < 0)) stop("observed counts must be non-negative")
  Ot <- sum(O)
  Et <- sum(E)
  rr <- Ot / Et
  m <- length(zones)
  if (m == 0L) stop("no candidate zones supplied")
  ll <- numeric(m)
  Ozs <- numeric(m)
  Ezs <- numeric(m)
  rin <- numeric(m)
  rout <- numeric(m)
  for (k in seq_len(m)) {
    idx <- as.integer(zones[[k]]) + 1L
    if (any(idx < 1L | idx > n)) stop("zone index out of range")
    oz <- sum(O[idx])
    ez <- sum(E[idx])
    Ozs[k] <- oz
    Ezs[k] <- ez
    oo <- Ot - oz
    eo <- Et - ez
    rin[k] <- if (ez > 0) oz / ez else NA_real_
    rout[k] <- if (eo > 0) oo / eo else NA_real_
    if (ez <= 0 || eo <= 0) { ll[k] <- -Inf
    next }
    if (isTRUE(highonly) && oz / ez <= rr) { ll[k] <- -Inf
    next }
    v <- 0
    if (oz > 0) v <- v + oz * (log(oz) - log(ez))
    if (oo > 0) v <- v + oo * (log(oo) - log(eo))
    ll[k] <- v
  }
  mx <- max(ll)
  bi <- which.max(ll)
  .t1_result(loglr = ll, best = bi - 1L, maxloglr = mx,
             bestzone = as.integer(zones[[bi]]), Oz = Ozs, Ez = Ezs,
             rrin = rin, rrout = rout, Otot = Ot, Etot = Et, nzone = m,
             method = "Kulldorff spatial scan statistic (Bivand et al. 2013 eq. 10.3)")
}
