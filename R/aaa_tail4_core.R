# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shared numeric helpers for the tail4 batch
#'
#' Internal only.  These mirror \code{morie.fn._t4core} on the Python
#' side so the two arms can be compared value-for-value.  Base R already
#' has ranks, ties, least squares and the distribution functions, so
#' most of this is a naming shim; the parts that are not -- the biased
#' autocorrelation used by \code{stats::Box.test}, the Bartlett long-run
#' variance used by \code{tseries::pp.test}, and the tie-corrected
#' Kendall variance -- are the parts that decide whether cross-language
#' parity holds at all.
#'
#' @name tail4_core
#' @keywords internal
NULL

#' .t4_vec
#'
#' A step of the tail4_core implementation. Called by \code{Beggtest}, \code{Boxpierce}, \code{Dprime} and 11 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{unlist}.
#' @return A vector, from \code{as.numeric}.
#' @export
.t4_vec <- function(x) as.numeric(unlist(x))

#' .t4_mat
#'
#' A step of the tail4_core implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @return A matrix, from \code{matrix}.
#' @export
.t4_mat <- function(X) {
  if (is.matrix(X)) return(matrix(as.numeric(X), nrow = nrow(X)))
  if (is.data.frame(X)) return(as.matrix(X))
  matrix(as.numeric(X), ncol = 1L)
}

#' .t4_ranks
#'
#' A step of the tail4_core implementation. Called by \code{Pettitt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{rank}.
#' @return The value of \code{rank}.
#' @export
.t4_ranks <- function(x) rank(x, ties.method = "average")

#' .t4_tiecounts
#'
#' A step of the tail4_core implementation. Called by \code{.t4_kendalltaub}, \code{Mktest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{table}.
#' @return A vector, from \code{as.numeric}.
#' @export
.t4_tiecounts <- function(x) as.numeric(table(x))

# Sample autocorrelations with the biased (n in both numerator and
# denominator) normalisation, i.e. stats::acf.
#' Sample autocorrelations with the biased (n in both numerator and
#'
#' denominator) normalisation, i.e. stats::acf.
#'
#' @param x A vector; its length is taken.
#' @param lag A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
.t4_acfbiased <- function(x, lag) {
  n <- length(x)
  d <- x - mean(x)
  c0 <- sum(d * d)
  vapply(seq_len(lag), function(k) sum(d[(k + 1):n] * d[1:(n - k)]) / c0, numeric(1))
}

# Newey-West long-run variance with Bartlett weights -- tseries' pp_sum.
#' Newey-West long-run variance with Bartlett weights -- tseries\'
#' pp_sum
#'
#' A step of the tail4_core implementation. Called by \code{Pptest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u A vector; its length is taken and its elements indexed.
#' @param lag A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.t4_lrvnw <- function(u, lag) {
  n <- length(u)
  s <- sum(u * u) / n
  tot <- 0
  for (i in seq_len(lag)) {
    acc <- sum(u[(i + 1):n] * u[1:(n - i)])
    tot <- tot + acc * (1 - i / (lag + 1))
  }
  s + 2 * tot / n
}

#' .t4_olsfit
#'
#' A step of the tail4_core implementation. Called by \code{.t4_poly4}, \code{.t4_wls_int}, \code{Lctest} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param y A matrix; passed to \code{crossprod}.
#' @return A list with \code{beta}, \code{fitted}, \code{resid}, \code{xtxinv}.
#' @export
.t4_olsfit <- function(X, y) {
  X <- as.matrix(X); y <- as.numeric(y)
  xtx <- crossprod(X)
  xtxinv <- solve(xtx)
  beta <- as.numeric(xtxinv %*% crossprod(X, y))
  fitted <- as.numeric(X %*% beta)
  list(beta = beta, fitted = fitted, resid = y - fitted, xtxinv = xtxinv)
}

#' .t4_kendallS
#'
#' A step of the tail4_core implementation. Called by \code{.t4_kendalltaub}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @return The value of \code{S}, as built in the body.
#' @export
.t4_kendallS <- function(x, y) {
  n <- length(x)
  S <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n)
    S <- S + sign(x[j] - x[i]) * sign(y[j] - y[i])
  S
}

# Kendall tau-b and the normal-approximation z, matching
# stats::cor.test(method = "kendall", exact = FALSE).
#' Kendall tau-b and the normal-approximation z, matching
#'
#' stats::cor.test(method = "kendall", exact = FALSE).
#'
#' @param x A vector; its length is taken.
#' @param y Passed to \code{.t4_kendallS}.
#' @return A list with \code{tau}, \code{z}.
#' @export
.t4_kendalltaub <- function(x, y) {
  n <- length(x)
  S <- .t4_kendallS(x, y)
  tx <- .t4_tiecounts(x); ty <- .t4_tiecounts(y)
  n0 <- n * (n - 1) / 2
  n1 <- sum(tx * (tx - 1)) / 2
  n2 <- sum(ty * (ty - 1)) / 2
  den <- sqrt((n0 - n1) * (n0 - n2))
  tau <- if (den > 0) S / den else NaN
  v0 <- n * (n - 1) * (2 * n + 5)
  vt <- sum(tx * (tx - 1) * (2 * tx + 5))
  vu <- sum(ty * (ty - 1) * (2 * ty + 5))
  v1 <- sum(tx * (tx - 1)) * sum(ty * (ty - 1))
  v2 <- sum(tx * (tx - 1) * (tx - 2)) * sum(ty * (ty - 1) * (ty - 2))
  v <- (v0 - vt - vu) / 18 + v1 / (2 * n * (n - 1)) +
    v2 / (9 * n * (n - 1) * (n - 2))
  list(tau = tau, z = if (v > 0) S / sqrt(v) else NaN)
}

#' .t4_result
#'
#' A step of the tail4_core implementation. Called by \code{Avgpathlen}, \code{Beggtest}, \code{Boxpierce} and 13 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ... Passed through.
#' @return The value of \code{out}, as built in the body.
#' @export
.t4_result <- function(...) {
  out <- list(...)
  class(out) <- c("morie_rich_result", "list")
  out
}
