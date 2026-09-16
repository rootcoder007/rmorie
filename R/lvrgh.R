# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hat-matrix diagonal (leverages)
#'
#' h_ii = x_i' (X'X)^-1 x_i, computed as the squared row norms of the thin QR
#' factor Q.  sum(h) = rank and the Belsley-Kuh-Welsch rule flags h > 2p/n.
#' Source consulted: Belsley, Kuh and Welsch (1980), Regression Diagnostics,
#' chapter 2.  Matches stats::hatvalues.
#'
#' @param X design matrix.
#' @param intercept prepend a column of ones.
#' @return list: estimate, leverage, rank, threshold, high, trace, n, method.
#' @keywords internal
#' @examples
#' lvrgh(cbind(1:10, c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)))$trace
#' @export
lvrgh <- function(X, intercept = TRUE) {
  m <- as.matrix(X)
  if (nrow(m) == 1L && ncol(m) > 1L) m <- t(m)
  n <- nrow(m)
  d <- if (intercept) cbind(1, m) else m
  dimnames(d) <- NULL
  qrd <- qr(d)
  q <- qr.Q(qrd)
  h <- rowSums(q * q)
  p <- ncol(d)
  thr <- 2 * p / n
  list(estimate = max(h), leverage = h, rank = as.integer(p), threshold = thr,
       high = h > thr, trace = sum(h), n = n,
       method = "Hat-matrix diagonal / leverage (Belsley, Kuh & Welsch 1980, ch. 2)")
}

# CANONICAL TEST
# r <- lvrgh(cbind(1:10, c(2,1,4,3,6,5,8,7,10,9)))
# stopifnot(abs(r$leverage[1] - 0.4) < 1e-12, abs(r$trace - 3) < 1e-12)

#' @rdname lvrgh
#' @keywords internal
#' @export
morie_lvrgh <- lvrgh
