# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shewhart control chart: the three-sigma decision, and what it costs
#'
#' Shewhart, W. A. (1926), "Quality Control Charts", Bell System Technical
#' Journal 5(4), 593-603, doi:10.1002/j.1538-7305.1926.tb04213.x. The paper was
#' opened directly (Internet Archive scan of BSTJ 5:4) and read as rendered page
#' images: the four-step procedure of specification, estimation, distribution
#' and test is on p. 597; Figure 3, p. 600, is the Inspection Engineering
#' analysis sheet, whose final column computes 3*sigma for each monitored
#' statistic -- 3 sigma_xbar = .0198010, 3 sigma_sigma = .0180106,
#' 3 sigma_k = .0599001, 3 sigma_beta2 = .119800 -- and p. 601 describes the
#' resulting dotted lines in Figure 4 as "the limits within which the different
#' statistics should lie, if the product had been controlled". The multiplier
#' three is therefore taken from the primary source, not from later practice.
#'
#' CITATION LIMIT. The book-length treatment, Economic Control of Quality of
#' Manufactured Product (1931), where the choice of three is argued at length
#' rather than merely applied, is lending-restricted on the Internet Archive and
#' could not be opened; nothing is attributed to it here.
#'
#' The chart signals when \code{|x_t - mu| > k sigma}. With k = 3 and a Gaussian
#' in-control distribution the false-alarm probability per point is
#' \code{2(1 - Phi(3)) = 0.0026998}, so the in-control average run length is
#' about 370 points: monitor a stable process daily for a year and expect
#' roughly one alarm that means nothing. That number, not the 3, is what makes
#' the choice a trade-off, and it is returned alongside the decisions so the
#' alarm rate observed can be read against the alarm rate expected. A chart is a
#' classifier, and its false-positive rate is a property of the limit, not of
#' the data.
#'
#' @param x The monitored statistic, in time order.
#' @param mu In-control centre line.
#' @param sigma In-control standard deviation of the monitored statistic; must
#'   be strictly positive.
#' @param k Limit multiplier. Default 3, as in Shewhart (1926) Figure 3.
#' @return List with \code{estimate} (number of signals), \code{alerts},
#'   \code{z}, \code{lcl}, \code{ucl}, \code{n_alerts}, \code{alarm_rate},
#'   \code{false_alarm_prob}, \code{arl0}, \code{n}, \code{k}, \code{method}.
#' @references Shewhart, W. A. (1926), Bell System Technical Journal
#'   5(4):593-603, doi:10.1002/j.1538-7305.1926.tb04213.x, Figure 3, p. 600.
#' @examples
#' Shewh(c(0, 1, 2, 4), 0, 1)$n_alerts  # 1
#' @export
Shewh <- function(x, mu, sigma, k = 3) {
  xv <- .s03vec(x)
  n <- length(xv)
  if (n == 0L) stop("shewhart: x is empty")
  m <- as.numeric(mu)[1L]
  s <- as.numeric(sigma)[1L]
  kk <- as.numeric(k)[1L]
  if (is.na(s) || !(s > 0) || is.infinite(s)) {
    stop("shewhart: sigma must be a positive finite number")
  }
  if (is.na(m) || is.infinite(m)) stop("shewhart: mu must be finite")
  if (is.na(kk) || !(kk > 0) || is.infinite(kk)) {
    stop("shewhart: k must be a positive finite number")
  }
  lcl <- m - kk * s
  ucl <- m + kk * s
  z <- (xv - m) / s
  alerts <- as.integer(abs(z) > kk)
  n_alerts <- sum(alerts)
  p <- 2 * (1 - .s03pnorm(kk))
  list(estimate = as.integer(n_alerts), alerts = alerts, z = z,
       lcl = lcl, ucl = ucl, n_alerts = as.integer(n_alerts),
       alarm_rate = n_alerts / n,
       false_alarm_prob = p, arl0 = if (p > 0) 1 / p else Inf,
       n = as.integer(n), k = kk,
       method = sprintf("Shewhart (1926) k-sigma control chart, k = %g", kk))
}
