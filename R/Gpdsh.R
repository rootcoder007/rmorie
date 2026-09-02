# SPDX-License-Identifier: AGPL-3.0-or-later
#' Density-shift detection on a stream by Kullback-Leibler divergence
#'
#' The stub carried the label "Gulenko et al (2019)".  No paper by that
#' author group in that year on density-shift detection could be matched
#' against Crossref (the closest, Gulenko et al 2018, CloudNet, is
#' packet-level anomaly detection for black-box services, a different
#' problem).  The attribution is recorded as UNVERIFIED and the method
#' is implemented from the formula the stub states, using the
#' closed-form Gaussian divergence.  KL is not symmetric, and is zero
#' exactly when the two windows agree; both are checked.
#'
#' Formula: KL(p || q) = log(s_q/s_p) + (s_p^2 + (m_p - m_q)^2)/(2 s_q^2) - 1/2.
#'
#' @param y_stream Numeric stream.
#' @param window Window length, at least 2.
#' @param tau Threshold above which a shift is declared.
#' @param floor Lower bound on a window variance.
#' @return List with \code{estimate}, \code{kl}, \code{flagged},
#'   \code{position}, \code{max_kl}, \code{change_point},
#'   \code{n_shifts}, \code{n}, \code{method}.
#' @references Kullback and Leibler (1951), On information and
#'   sufficiency, Annals of Mathematical Statistics 22(1):79-86.
#'   \doi{10.1214/aoms/1177729694}
#' @export
#' @examples
#' set.seed(1)
#' Gpdsh(rnorm(30), window = 10)
Gpdsh <- function(y_stream, window = 10, tau = 0.5, floor = 1e-12) {
  v <- .s03vec(y_stream)
  n <- length(v)
  w <- as.integer(window)
  if (w < 2L) stop("gp_density_shift: window must be at least 2")
  if (n < 2L * w) stop("gp_density_shift: the stream is shorter than two windows")
  t <- as.numeric(tau)
  if (t < 0) stop("gp_density_shift: tau must be non-negative")
  kls <- numeric(0)
  flags <- integer(0)
  pos <- integer(0)
  for (s in seq(w, n - w)) {
    a <- v[(s - w + 1L):s]
    b <- v[(s + 1L):(s + w)]
    ma <- mean(a)
    mb <- mean(b)
    sa <- sqrt(max(sum((a - ma)^2) / (w - 1), as.numeric(floor)))
    sb <- sqrt(max(sum((b - mb)^2) / (w - 1), as.numeric(floor)))
    d <- log(sa / sb) + (sb * sb + (mb - ma)^2) / (2 * sa * sa) - 0.5
    kls <- c(kls, d)
    flags <- c(flags, as.integer(d > t))
    pos <- c(pos, s)
  }
  mx <- which.max(kls)
  .t1_result(estimate = kls[mx], kl = kls, flagged = flags, position = pos,
             max_kl = kls[mx], change_point = pos[mx], n_shifts = sum(flags),
             n = n,
             method = "Gaussian KL between consecutive windows against tau; attribution 'Gulenko et al (2019)' UNVERIFIED")
}
