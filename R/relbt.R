# SPDX-License-Identifier: AGPL-3.0-or-later
#' Reliability: squared predictive ability divided by heritability
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 109-139], Chapter 4, equation (4.2), p. 129, read as a
#' rendered page, defines the predictive ability r as Pearson's correlation
#' between the testing observations and their predictions.  Dekkers (2007),
#' Prediction of response to marker-assisted and genomic selection using
#' selection index theory, Journal of Animal Breeding and Genetics 124(6),
#' 331-341, gives the accuracy of the breeding value as r_g = r / h, so the
#' reliability, the squared accuracy, is rel = r^2 / h^2.  Chapter 4 defines
#' r but never divides it by h; that step is Dekkers's.
#'
#' @param r predictive ability, in [-1, 1].
#' @param h2 narrow-sense heritability, in (0, 1]; recycled against r.
#' @return list: estimate, reliability, accuracy, n, method.
#' @keywords internal
#' @examples
#' Relbt(0.5, 0.25)$estimate
#' @export
Relbt <- function(r, h2) {
  rr <- .s03vec(r)
  hh <- .s03vec(h2)
  if (length(rr) == 0L || length(hh) == 0L) stop("reliability_metric: both r and h2 are required")
  if (length(rr) != length(hh) && length(rr) != 1L && length(hh) != 1L) {
    stop("reliability_metric: r and h2 have incompatible lengths")
  }
  n <- max(length(rr), length(hh))
  rel <- numeric(n)
  acc <- numeric(n)
  for (i in seq_len(n)) {
    a <- rr[((i - 1L) %% length(rr)) + 1L]
    b <- hh[((i - 1L) %% length(hh)) + 1L]
    if (a < -1 || a > 1) stop("reliability_metric: r must lie in [-1, 1]")
    if (b <= 0 || b > 1) stop("reliability_metric: h2 must lie in (0, 1]")
    rel[i] <- a * a / b
    acc[i] <- a / sqrt(b)
  }
  list(estimate = rel[1], reliability = rel, accuracy = acc, n = n,
       method = "rel = r^2 / h^2, with r the Chapter 4 eq. (4.2) predictive ability (Dekkers 2007)")
}
