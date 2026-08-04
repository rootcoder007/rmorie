# SPDX-License-Identifier: AGPL-3.0-or-later
#' Holm step-down multiple-testing procedure (Holm 1979)
#'
#' Source: Holm, S. (1979), A simple sequentially rejective multiple test
#' procedure, Scandinavian Journal of Statistics 6, 65-70.  The 1979
#' paper is paywalled here; the procedure is quoted in its standard
#' published form, the form given in Hastie, Tibshirani and Friedman,
#' The Elements of Statistical Learning (2nd ed., 2009), section 18.7:
#' order \code{p_(1) <= ... <= p_(m)}, let \code{L} be the smallest
#' \code{j} with \code{p_(j) > alpha/(m-j+1)}, and reject
#' \code{H_(1), ..., H_(L-1)}.  The step-down adjusted p-values are
#' \code{ptilde_(j) = max_{k<=j} min(1, (m-k+1) p_(k))}.
#'
#' @param pvalues Numeric vector of raw p-values.
#' @param alpha Family-wise error rate.  Default 0.05.
#' @return list: reject, p_adjusted, n_reject, alpha, m, method.
#' @examples
#' Holmadj(c(0.01, 0.04, 0.03, 0.005), 0.05)$n_reject
#' @export
Holmadj <- function(pvalues, alpha = 0.05) {
  p <- as.numeric(pvalues)
  m <- length(p)
  if (m == 0L) stop("pvalues is empty")
  alpha <- as.numeric(alpha)
  ord <- order(p)
  ps <- p[ord]
  k <- m
  for (j in seq_len(m)) {
    if (ps[j] > alpha / (m - j + 1)) {
      k <- j - 1L
      break
    }
  }
  adj <- numeric(m)
  run <- 0
  for (j in seq_len(m)) {
    run <- max(run, min(1, (m - j + 1) * ps[j]))
    adj[j] <- run
  }
  reject <- logical(m)
  padj <- numeric(m)
  reject[ord] <- seq_len(m) <= k
  padj[ord] <- adj
  list(
    reject = reject, p_adjusted = padj, n_reject = as.integer(k),
    alpha = alpha, m = m,
    method = "Holm (1979) step-down multiple test"
  )
}
