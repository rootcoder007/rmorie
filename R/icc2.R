# SPDX-License-Identifier: AGPL-3.0-or-later
#' ICC(2,1) two-way random-effects, single rater, absolute agreement
#'
#' Shrout, P. E. and Fleiss, J. L. (1979), "Intraclass correlations: uses in
#' assessing rater reliability", \emph{Psychological Bulletin} 86(2), 420-428,
#' doi:10.1037/0033-2909.86.2.420, is the primary source; it is closed access
#' with no open copy in any repository (Unpaywall reports oa_status "closed"),
#' so the arithmetic was read from Hedderich, J., Sachs, L. and Reynarowych,
#' Z., \emph{Applied Statistics: Methods Using R}, Springer, Section 6.16,
#' pp. 427-428, whose R function labelled "ANOVA according to Shrout-Fleiss"
#' gives SS_b = sum(column totals^2)/n - T^2/(nk), SS_e = SS_t - SS_a - SS_b,
#' JMS = SS_b/(k-1) and EMS = SS_e/((n-1)(k-1)).
#'
#' ERRATUM in that book.  Its R code on p. 428 writes the last term of the
#' denominator as (k * JMS - EMS) / n, not k * (JMS - EMS) / n, and the value
#' it prints for its own example -- pituitary height by MRI, k = 3 examiners
#' on n = 10 patients with intracranial hypotension, p. 427 -- is 0.9759,
#' which is what that expression gives.  The two-way random model
#' y_ij = mu + r_i + c_j + e_ij has E[MSR] = sigma_e^2 + k sigma_r^2,
#' E[MSC] = sigma_e^2 + n sigma_c^2 and E[MSE] = sigma_e^2, so the moment
#' estimates are sigma_r^2 = (MSR - MSE)/k, sigma_c^2 = (MSC - MSE)/n and
#' sigma_e^2 = MSE, and
#'
#' \deqn{ICC(2,1) = \frac{\sigma_r^2}{\sigma_r^2 + \sigma_c^2 + \sigma_e^2} = \frac{MSR - MSE}{MSR + (k-1)MSE + k(MSC - MSE)/n},}{ICC(2,1) = sigma_r^2/(sigma_r^2 + sigma_c^2 + sigma_e^2) = (MSR - MSE)/(MSR + (k-1) MSE + k (MSC - MSE)/n),}
#'
#' which is the form used here.  On the book's own data it returns 0.977209,
#' and the same number comes out of the variance components computed from
#' stats::aov, so 0.9759 is a misprint in the book, not a different
#' convention.  The mean squares themselves agree with stats::aov exactly:
#' MSR 17.700926, MSC 0.272333, MSE 0.121593.
#'
#' @param y Ratings in long format.
#' @param subject Subject of each rating.
#' @param rater Rater of each rating; the design must be complete and balanced.
#' @return list: estimate (ICC(2,1)), bms, wms, jms, ems, n, k, method.
#' @keywords internal
#' @examples
#' Icc2(c(1, 2, 3, 4, 5, 6), c(1, 1, 2, 2, 3, 3), c(1, 2, 1, 2, 1, 2))$estimate
#' @export
Icc2 <- function(y, subject, rater) {
  b <- .icc_balanced(y, subject, "icc_two_way_random")
  n <- b$n; k <- b$k
  rs <- .s03vec(rater)
  if (length(rs) != n * k) {
    stop("icc_two_way_random: rater must have one entry per rating")
  }
  if (length(unique(rs)) != k) {
    stop("icc_two_way_random: the number of raters must match the ratings per subject")
  }
  ms <- .icc_mean_squares(b$rows, n, k)
  den <- ms$bms + (k - 1) * ms$ems + k * (ms$jms - ms$ems) / n
  if (den == 0) stop("icc_two_way_random: the ratings carry no variance")
  list(estimate = (ms$bms - ms$ems) / den, bms = ms$bms, wms = ms$wms,
       jms = ms$jms, ems = ms$ems, n = n, k = k,
       method = "ICC(2,1) two-way random single rater")
}

# The Shrout-Fleiss two-way ANOVA of Hedderich et al., pp. 427-428.
.icc_mean_squares <- function(rows, n, k) {
  tot <- 0; tot2 <- 0; ssa <- 0
  colsum <- numeric(k)
  for (i in seq_len(n)) {
    s <- 0
    for (j in seq_len(k)) {
      e <- rows[i, j]
      s <- s + e; tot <- tot + e; tot2 <- tot2 + e * e
      colsum[j] <- colsum[j] + e
    }
    ssa <- ssa + s * s / k
  }
  corr <- tot * tot / (n * k)
  sst <- tot2 - corr
  ssa <- ssa - corr
  ssb <- sum(colsum * colsum) / n - corr
  sse <- sst - ssa - ssb
  list(bms = ssa / (n - 1), wms = (sst - ssa) / (n * (k - 1)),
       jms = ssb / (k - 1), ems = sse / ((n - 1) * (k - 1)))
}
