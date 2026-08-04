# SPDX-License-Identifier: AGPL-3.0-or-later
#' Score (Rao) test of H0: p = p0 for a binomial proportion.
#'
#' The variance is evaluated at the NULL p0, not at the estimate, which
#' is why the score test still works when phat is 0 or 1.
#'
#' Formula: U = (S - n p0) / sqrt(n p0 (1 - p0));
#'   U^2 ~ chi^2_1; p = 2(1 - Phi(|U|))
#'
#' @param successes Number of successes S.
#' @param n Number of trials.
#' @param p0 Null proportion, strictly between 0 and 1.
#' @return List with \code{statistic}, \code{chisq}, \code{p_value},
#'   \code{estimate}, \code{se_null}, \code{n}.
#' @references Rao (1948), Mathematical Proceedings of the Cambridge
#'   Philosophical Society 44(1), 50-57 -- the primary source for the
#'   score test. Wasserman (2004), All of Statistics, does NOT give the
#'   score test, so it is not cited for this formula; the full text of
#'   the book was fetched and searched to establish that.
#' @export
Scoretest <- function(successes, n, p0 = 0.5) {
  n <- as.integer(n); S <- as.numeric(successes); p0 <- as.numeric(p0)
  if (n < 1L) stop("n must be at least 1")
  if (S < 0 || S > n) stop("successes must lie in 0..n")
  if (p0 <= 0 || p0 >= 1) stop("p0 must lie strictly between 0 and 1")
  se <- sqrt(n * p0 * (1 - p0))
  U <- (S - n * p0) / se
  .t1_result(statistic = U, chisq = U^2,
             p_value = 2 * stats::pnorm(abs(U), lower.tail = FALSE),
             estimate = S / n, se_null = se, n = as.numeric(n),
             method = "Rao score test for a binomial proportion")
}
