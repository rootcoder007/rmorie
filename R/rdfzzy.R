# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fuzzy regression discontinuity: the Wald ratio at the cutoff
#'
#' The denominator is the first stage and it is the whole difficulty:
#' when the jump in treatment probability is small the ratio is a weak
#' instrument and its standard error understates the real uncertainty.
#' The first stage is reported alongside, and a denominator
#' indistinguishable from zero is an error rather than an infinity. With
#' a denominator of exactly one the estimator collapses to the sharp one.
#'
#' Formula: \code{tau_LATE = (lim Y+ - lim Y-) / (lim D+ - lim D-)}; the
#' standard error is the delta method applied to that ratio.
#'
#' @param y Outcome.
#' @param x Running variable.
#' @param D Treatment received.
#' @param cutoff Threshold.
#' @param bandwidth Half-window, positive.
#' @return List with \code{estimate}, \code{tau}, \code{se}, \code{z},
#'   \code{reduced_form}, \code{first_stage}, \code{se_reduced_form},
#'   \code{se_first_stage}, \code{n_right}, \code{n_left}, \code{bandwidth}.
#' @references Hahn, J., Todd, P. & Van der Klaauw, W. (2001).
#'   Econometrica 69(1):201-209. \doi{10.1111/1468-0262.00183}; the fuzzy
#'   design is their Theorem 3.
#' @export
#' @examples
#' set.seed(1)
#' r <- Rdfzzy(y = rnorm(10), x = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Rdfzzy <- function(y, x, D, cutoff = 0, bandwidth = 1) {
  y <- as.numeric(unlist(y))
  x <- as.numeric(unlist(x))
  D <- as.numeric(unlist(D))
  if (length(y) == 0L) stop("Rdfzzy: y is empty")
  if (length(x) != length(y) || length(D) != length(y))
    stop("Rdfzzy: x and D must have one entry per observation")
  s <- .rd_sides(x, cutoff, bandwidth, "Rdfzzy")
  yR <- .rd_wls(s$r[s$right], y[s$right], s$w[s$right])
  yL <- .rd_wls(s$r[s$left], y[s$left], s$w[s$left])
  dR <- .rd_wls(s$r[s$right], D[s$right], s$w[s$right])
  dL <- .rd_wls(s$r[s$left], D[s$left], s$w[s$left])
  num <- yR$a - yL$a
  den <- dR$a - dL$a
  if (abs(den) < 1e-10) stop("Rdfzzy: the first stage is indistinguishable from zero")
  tau <- num / den
  vn <- yR$var_a + yL$var_a
  vd <- dR$var_a + dL$var_a
  se <- sqrt(vn / (den * den) + (num * num) * vd / (den^4))
  .t1_result(estimate = tau, tau = tau, se = se,
             z = if (se > 0) tau / se else NA_real_,
             reduced_form = num, first_stage = den,
             se_reduced_form = sqrt(vn), se_first_stage = sqrt(vd),
             n_right = yR$n, n_left = yL$n, bandwidth = s$h,
             method = "Fuzzy RDD, Wald ratio of local linear intercepts")
}
