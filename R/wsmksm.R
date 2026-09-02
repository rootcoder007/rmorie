# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-sample Kolmogorov-Smirnov test of H0: F1 = F2
#'
#' The p-value is the asymptotic one; it is unreliable for very small
#' samples and for heavily tied data, and \code{ties} is returned so a
#' caller can see whether that applies.
#'
#' Formula: D = sup_x |F1(x) - F2(x)|; t = sqrt(n1 n2/(n1 + n2)) D;
#'   p = 1 - H(t) = 2 sum_\{j>=1\} (-1)^\{j-1\} exp(-2 j^2 t^2)
#'
#' @param x,y The two samples, each of length at least 1.
#' @param terms Terms of the alternating series used for H(t).
#' @return List with \code{statistic}, \code{scaled}, \code{p_value},
#'   \code{n1}, \code{n2}, \code{ties}.
#' @references Wasserman (2004), All of Statistics, Section 15.4 and
#'   Theorem 15.12, equation (15.14). Fetched as the full text of the
#'   book.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Kstest1(V, V)
Kstest1 <- function(x, y, terms = 200) {
  x <- sort(.t1_vec(x)); y <- sort(.t1_vec(y))
  n1 <- length(x); n2 <- length(y)
  if (n1 < 1L || n2 < 1L) stop("both samples must be non-empty")
  pool <- sort(unique(c(x, y)))
  D <- max(abs(vapply(pool, function(v) sum(x <= v), 0) / n1 -
             vapply(pool, function(v) sum(y <= v), 0) / n2))
  t <- sqrt(n1 * n2 / (n1 + n2)) * D
  p <- if (t > 0) {
    j <- seq_len(as.integer(terms))
    2 * sum(ifelse(j %% 2 == 1, 1, -1) * exp(-2 * j^2 * t^2))
  } else 1
  p <- min(1, max(0, p))
  .t1_result(statistic = D, scaled = t, p_value = p, n1 = n1, n2 = n2,
             ties = n1 + n2 - length(pool),
             method = "Two-sample KS test, Wasserman Theorem 15.12")
}
