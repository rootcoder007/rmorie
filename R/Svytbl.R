# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weighted two-way table with a design-corrected test of independence
#'
#' The cell entries are estimated population counts, so Pearson's
#' statistic computed on them is wrong twice: it is on the population
#' scale, and it ignores the variance the weights inflate.  Rao and
#' Scott's first-order correction evaluates Pearson's statistic on the
#' weighted proportions and multiplies by the effective sample size
#' neff = (sum w)^2 / sum w^2 in place of n.  With equal weights
#' neff = n and the statistic is exactly the uncorrected chisq.test
#' statistic.
#'
#' Formula: X2 = neff sum_ij (p_ij - p_i. p_.j)^2 / (p_i. p_.j), referred
#' to chi-square on (r-1)(c-1) degrees of freedom.
#'
#' @param x Row category labels.
#' @param y Column category labels, same length.
#' @param weights Optional design weights; equal weights if NULL.
#' @return List with \code{estimate}, \code{statistic_naive}, \code{df},
#'   \code{p_value}, \code{counts} (row-major), \code{prop}, \code{rows},
#'   \code{cols}, \code{nrow}, \code{ncol}, \code{neff}, \code{deff},
#'   \code{n}, \code{method}.
#' @references Rao, J. N. K. and Scott, A. J. (1984). On chi-squared tests
#'   for multiway contingency tables with cell proportions estimated from
#'   survey data. The Annals of Statistics 12(1):46-60.
#'   \doi{10.1214/aos/1176346391}
#' @examples
#' Svytbl(c(0, 0, 0, 0, 1, 1, 1, 1), c(0, 0, 1, 1, 0, 1, 1, 1))
#' @export
Svytbl <- function(x, y, weights = NULL) {
  xa <- .wfrep_lab(x)
  ya <- .wfrep_lab(y)
  n <- length(xa)
  if (n == 0L) stop("survey_xtab: x is empty")
  if (length(ya) != n) stop("survey_xtab: x and y differ in length")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("survey_xtab: x and weights differ in length")
  rl <- sort(unique(xa))
  cl <- sort(unique(ya))
  r <- length(rl)
  cc <- length(cl)
  if (r < 2L || cc < 2L)
    stop("survey_xtab: need at least two rows and two columns")
  cnt <- numeric(r * cc)
  for (i in seq_len(n)) {
    ii <- match(xa[i], rl)
    jj <- match(ya[i], cl)
    cnt[(ii - 1L) * cc + jj] <- cnt[(ii - 1L) * cc + jj] + w[i]
  }
  tot <- sum(cnt)
  p <- cnt / tot
  M <- matrix(p, nrow = r, ncol = cc, byrow = TRUE)
  pr <- rowSums(M)
  pc <- colSums(M)
  stat <- 0
  for (i in seq_len(r)) for (j in seq_len(cc)) {
    e <- pr[i] * pc[j]
    if (e > 0) stat <- stat + (M[i, j] - e)^2 / e
  }
  sw <- sum(w)
  neff <- sw * sw / sum(w * w)
  df <- (r - 1L) * (cc - 1L)
  X2 <- neff * stat
  list(estimate = as.numeric(X2), statistic_naive = as.numeric(n * stat),
       df = as.integer(df),
       p_value = as.numeric(stats::pchisq(X2, df, lower.tail = FALSE)),
       counts = cnt, prop = p, rows = rl, cols = cl,
       nrow = as.integer(r), ncol = as.integer(cc), neff = as.numeric(neff),
       deff = as.numeric(n / neff), n = as.integer(n),
       method = "Rao-Scott first-order corrected Pearson chi-square [Rao & Scott 1984]")
}

# CANONICAL TEST
# x <- c(0, 0, 0, 0, 1, 1, 1, 1); y <- c(0, 0, 1, 1, 0, 1, 1, 1)
# r <- Svytbl(x, y)
# stopifnot(abs(r$estimate -
#   as.numeric(stats::chisq.test(table(x, y), correct = FALSE)$statistic)) < 1e-12)
