# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ChIP-seq peak significance with the MACS dynamic Poisson lambda
# (Chipsq). Bit-identical mirror of src/morie/fn/chipsq.py. Anchored
# against stats::ppois and a hand-computed lambda_local.

#' ChIP-seq candidate-peak significance (MACS dynamic lambda)
#'
#' For a candidate peak of the given width with k ChIP tags, the
#' uniform background rate lambda_BG is replaced by the dynamic
#' \eqn{\lambda_{local} = \max(\lambda_{BG}, [\lambda_{1k},]
#' \lambda_{5k}, \lambda_{10k})}, where \eqn{\lambda_{Xk}} is the
#' control tag count of the X-kb window centred on the peak rescaled
#' to the peak width (count * width / X000). When no control sample
#' exists the 1 kb term is omitted (\code{use_1k = FALSE}). The peak
#' p-value is the Poisson upper tail \eqn{P(X \ge k)} under
#' lambda_local and fold_enrichment is k / lambda_local.
#'
#' @param count ChIP tag count per candidate peak.
#' @param width Peak region width in bp.
#' @param lambda_bg Expected background tag count for the region.
#' @param count_1k,count_5k,count_10k Control tag counts in the
#'   centred 1 kb, 5 kb and 10 kb windows (optional).
#' @param use_1k Set FALSE when no control sample exists.
#' @return List with \code{pvalue}, \code{lambda_local},
#'   \code{fold_enrichment}, \code{count}, \code{width},
#'   \code{n_peaks}, \code{method}.
#' @references Zhang, Y., Liu, T., Meyer, C. A., Eeckhoute, J.,
#'   Johnson, D. S., Bernstein, B. E., Nusbaum, C., Myers, R. M.,
#'   Brown, M., Li, W. and Liu, X. S. (2008), Model-based Analysis
#'   of ChIP-Seq (MACS), Genome Biology 9(9), R137. Dynamic lambda
#'   and Poisson p-value, Methods, Peak detection. Local source:
#'   library/pdf/fetched-wave3/Zhang-2008-MACS-GenomeBiology.pdf.
#' @export
Chipsq <- function(count, width, lambda_bg, count_1k = NULL,
                   count_5k = NULL, count_10k = NULL, use_1k = TRUE) {
  k <- as.numeric(count)
  npk <- length(k)
  vec <- function(x) {
    if (is.null(x)) return(NULL)
    v <- as.numeric(x)
    if (length(v) == 1L && npk > 1L) v <- rep(v, npk)
    if (length(v) != npk) {
      stop("window counts must match count length", call. = FALSE)
    }
    v
  }
  wv <- vec(width)
  c1 <- vec(count_1k); c5 <- vec(count_5k); c10 <- vec(count_10k)
  lam_loc <- numeric(npk); pv <- numeric(npk); fe <- numeric(npk)
  for (i in seq_len(npk)) {
    lam <- lambda_bg
    if (!is.null(c1) && use_1k) lam <- max(lam, c1[i] * wv[i] / 1000)
    if (!is.null(c5)) lam <- max(lam, c5[i] * wv[i] / 5000)
    if (!is.null(c10)) lam <- max(lam, c10[i] * wv[i] / 10000)
    lam_loc[i] <- lam
    ki <- as.integer(k[i])
    pv[i] <- if (ki <= 0) 1 else if (lam <= 0) 0 else
      stats::pgamma(lam, shape = ki)
    fe[i] <- if (lam > 0) k[i] / lam else Inf
  }
  list(pvalue = pv, lambda_local = lam_loc, fold_enrichment = fe,
       count = k, width = wv, n_peaks = npk,
       method = "MACS dynamic-lambda Poisson peak test (Zhang et al. 2008)")
}
