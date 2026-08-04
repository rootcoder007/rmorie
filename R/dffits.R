# SPDX-License-Identifier: AGPL-3.0-or-later
#' DFFITS, the scaled deletion influence on the fitted value.
#'
#' Formula: DFFITS_i = t*_i sqrt(h_ii/(1-h_ii)), t*_i = e_i / (s_(i) sqrt(1-h_ii))
#'
#' @param X Design matrix.
#' @param y Response.
#' @param intercept Prepend a column of ones.

#' @return List with ``dffits``, ``cutoff``, ``leverage``, ``student``, ``n``, ``p``.
#' @references Belsley, Kuh and Welsch (1980), Regression Diagnostics: Identifying Influential Data and Sources of Collinearity, Wiley. The book is not held locally; the definitions and the cutoffs used here (2/sqrt(n) for DFBETAS, 2 sqrt(p/n) for DFFITS, scaling by the delete-one root mean square s_(i)) are as documented by the SAS and R reference implementations, which cite BKW for them.
#' @export
Dffitsols <- function(X, y, intercept = TRUE) {
  X <- as.matrix(X); if (isTRUE(intercept)) X <- .t1_cbind1(X)
  y <- .t1_vec(y); n <- nrow(X); p <- ncol(X)
  f <- .t1_lstsq(X, y); h <- .t1_hatdiag(X, f$xtxinv)
  rss <- sum(f$resid^2); df <- n - p
  dff <- rep(NA_real_, n); stu <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    d <- 1 - h[i]
    if (d <= 0 || df <= 1) next
    s2i <- (rss - f$resid[i]^2 / d) / (df - 1)
    s <- if (s2i > 0) sqrt(s2i) else NA_real_
    t <- f$resid[i] / (s * sqrt(d))
    stu[i] <- t; dff[i] <- t * sqrt(h[i] / d)
  }
  .t1_result(dffits = dff, cutoff = 2 * sqrt(p / n), leverage = h,
             student = stu, n = n, p = p,
             method = "DFFITS (Belsley-Kuh-Welsch)")
}
