# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mutual information between two discrete variables
#'
#' Shannon (1948), A mathematical theory of communication, Bell System
#' Technical Journal 27(3), 379-423: I(X;Y) = sum_x sum_y p(x,y) log(p(x,y)
#' / (p(x) p(y))), with 0 log 0 = 0.  The paper is freely available but was
#' not retrievable here; the definition is quoted in its standard published
#' form.  The plug-in estimator is biased upward by about (|X|-1)(|Y|-1) /
#' (2n) (Miller 1955), so the Miller-Madow corrected value is returned as
#' well -- the raw plug-in alone overstates dependence on small samples.
#' Labels are compared as strings and sorted byte-wise so the Python mirror
#' orders them identically whatever the locale.
#'
#' @param y the first variable (first slot, for signature stability).
#' @param x the second variable, or the first of the pair when y2 is given.
#' @param y2 the second of the pair.
#' @return list: estimate, mi, bits, mm, hx, hy, hxy, n, method.
#' @keywords internal
#' @examples
#' Mutinfo(c(0, 0, 1, 1), c(0, 1, 0, 1))$mi
#' @export
Mutinfo <- function(y, x = NULL, y2 = NULL) {
  if (is.null(y2)) { a <- as.character(y); b <- as.character(x) }
  else { a <- as.character(x); b <- as.character(y2) }
  n <- length(a)
  la <- sort(unique(a), method = "radix")
  lb <- sort(unique(b), method = "radix")
  P <- matrix(0, length(la), length(lb))
  for (i in seq_len(n)) {
    P[match(a[i], la), match(b[i], lb)] <- P[match(a[i], la), match(b[i], lb)] + 1 / n
  }
  px <- numeric(length(la)); py <- numeric(length(lb))
  for (i in seq_along(la)) for (j in seq_along(lb)) {
    px[i] <- px[i] + P[i, j]; py[j] <- py[j] + P[i, j]
  }
  mi <- 0; hxy <- 0
  for (i in seq_along(la)) for (j in seq_along(lb)) {
    if (P[i, j] > 0) {
      mi <- mi + P[i, j] * log(P[i, j] / (px[i] * py[j]))
      hxy <- hxy - P[i, j] * log(P[i, j])
    }
  }
  hx <- 0
  for (v in px) if (v > 0) hx <- hx - v * log(v)
  hy <- 0
  for (v in py) if (v > 0) hy <- hy - v * log(v)
  nz <- 0L
  for (i in seq_along(la)) for (j in seq_along(lb)) if (P[i, j] > 0) nz <- nz + 1L
  mm <- if (n) mi - (nz - length(la) - length(lb) + 1) / (2 * n) else NaN
  list(estimate = mi, mi = mi, bits = mi / log(2), mm = mm, hx = hx, hy = hy,
       hxy = hxy, n = n,
       method = "Plug-in mutual information (Shannon 1948) with the Miller-Madow correction")
}
