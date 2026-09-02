# SPDX-License-Identifier: AGPL-3.0-or-later
#' DFBETAS, the scaled per-coefficient deletion influence.
#'
#' Formula: DFBETAS_ij = (b_j - b_(i)j) / (s_(i) sqrt(C_jj)), C = (X'X)^-1
#'
#' @param X Design matrix.
#' @param y Response.
#' @param intercept Prepend a column of ones.

#' @return List with ``dfbetas`` (n by p), ``cutoff``, ``leverage``, ``beta``, ``sigma_i``, ``n``, ``p``.
#' @references Belsley, Kuh and Welsch (1980), Regression Diagnostics: Identifying Influential Data and Sources of Collinearity, Wiley. The book is not held locally; the definitions and the cutoffs used here (2/sqrt(n) for DFBETAS, 2 sqrt(p/n) for DFFITS, scaling by the delete-one root mean square s_(i)) are as documented by the SAS and R reference implementations, which cite BKW for them.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dfbetas(V, V)
Dfbetas <- function(X, y, intercept = TRUE) {
  X <- as.matrix(X); if (isTRUE(intercept)) X <- .t1_cbind1(X)
  y <- .t1_vec(y); n <- nrow(X); p <- ncol(X)
  f <- .t1_lstsq(X, y); h <- .t1_hatdiag(X, f$xtxinv)
  rss <- sum(f$resid^2); df <- n - p
  out <- matrix(NA_real_, n, p); si <- rep(NA_real_, n)
  cj <- sqrt(diag(f$xtxinv))
  for (i in seq_len(n)) {
    d <- 1 - h[i]
    if (d <= 0) next
    s2i <- if (df > 1) (rss - f$resid[i]^2 / d) / (df - 1) else NA_real_
    s <- if (!is.na(s2i) && s2i > 0) sqrt(s2i) else NA_real_
    si[i] <- s
    cx <- as.numeric(f$xtxinv %*% X[i, ])
    out[i, ] <- cx * f$resid[i] / d / (s * cj)
  }
  .t1_result(dfbetas = out, cutoff = 2 / sqrt(n), leverage = h,
             beta = f$beta, sigma_i = si, n = n, p = p,
             method = "DFBETAS (Belsley-Kuh-Welsch)")
}
