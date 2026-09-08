# SPDX-License-Identifier: AGPL-3.0-or-later
#' Biweight M-estimate of location, with redescending weights
#'
#' Huber weights taper but never vanish, so a point far enough out still
#' moves the estimate. The biweight redescends to exactly zero past c.
#' The price is a non-convex objective, so the start matters -- the
#' median and a MAD scale are used.
#'
#' Determinism: fixed number of reweighting sweeps, no tolerance.
#'
#' Formula: \code{w(r) = (1 - (r/c)^2)^2} for \code{|r| <= c}, else 0,
#' with \code{r = (y - mu)/s} and s the consistency-scaled MAD.
#'
#' @param y Sample.
#' @param c Tuning constant; 4.685 gives 95 percent Gaussian efficiency.
#' @param n_iter Reweighting sweeps.
#' @return List with \code{estimate}, \code{scale}, \code{weights}, \code{n}.
#' @references Beaton, A. E. & Tukey, J. W. (1974). Technometrics
#'   16:147-185.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tukeyw(V)
Tukeyw <- function(y, c = 4.685, n_iter = 20) {
  v <- as.numeric(unlist(y))
  n <- length(v)
  mu <- .s4_median(v)
  s <- .s4_median(abs(v - mu)) / 0.6744897501960817
  if (s <= 0) s <- 1
  w <- rep(1, n)
  for (it in seq_len(as.integer(n_iter))) {
    r <- (v - mu) / (c * s)
    w <- ifelse(abs(r) < 1, (1 - r * r)^2, 0)
    sw <- sum(w)
    if (sw > 0) mu <- sum(w * v) / sw
  }
  .t1_result(estimate = mu, scale = s, weights = w, n = n,
             method = "Tukey biweight location, MAD scale")
}
