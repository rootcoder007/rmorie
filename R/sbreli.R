# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spearman-Brown prophecy formula
#'
#' Spearman, C. (1910), Correlation calculated from faulty data, British
#' Journal of Psychology 3, 271-295, and Brown, W. (1910), Some
#' experimental results in the correlation of mental abilities, ibid.
#' 296-322, independently give r' = k r / (1 + (k - 1) r) for the
#' reliability of a test lengthened by a factor k.  Neither 1910 paper was
#' available here as a full text; the equation is quoted in its standard
#' published form.
#'
#' @param r reliability of the original test.
#' @param k lengthening factor.
#' @param target optional target reliability; the factor needed to reach it
#'   is returned as k_needed.
#' @return list: estimate, r, k, k_needed, method.
#' @keywords internal
#' @examples
#' Spbrown(0.7, 2)$estimate
#' @export
Spbrown <- function(r, k, target = NULL) {
  r <- as.numeric(r)
  k <- as.numeric(k)
  den <- 1 + (k - 1) * r
  est <- if (den != 0) (k * r) / den else NaN
  if (is.null(target)) {
    kneed <- NaN
  } else {
    t <- as.numeric(target)
    d2 <- r * (1 - t)
    kneed <- if (d2 != 0) (t * (1 - r)) / d2 else NaN
  }
  list(estimate = est, r = r, k = k, k_needed = kneed,
       method = "Spearman-Brown projected reliability")
}
