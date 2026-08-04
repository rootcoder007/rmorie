# SPDX-License-Identifier: AGPL-3.0-or-later
#' Test a Dirichlet-process partition cluster count against its prior
#'
#' Source READ FROM THE CORPUS PDF: Ghosal, S. and van der Vaart, A.
#' (2017), Fundamentals of Nonparametric Bayesian Inference, section
#' 4.1.5, Proposition 4.8, crediting Antoniak (1974), Annals of
#' Statistics 2, 1152-1174.  (The Antoniak paper was located only as a
#' scanned image PDF with no text layer and could not be read; the
#' Ghosal and van der Vaart statement is a primary textbook source.)
#'
#' Proposition 4.8: for an atomless base measure of total mass M the
#' indicators D_i of "observation i is a new value" are INDEPENDENT
#' Bernoulli with \code{P(D_i = 1) = M/(M + i - 1)}, so
#' \code{K_n = sum_i D_i} has exact moments
#' \code{E(K_n) = sum_i M/(M + i - 1)} and
#' \code{var(K_n) = sum_i M(i-1)/(M + i - 1)^2}, is asymptotically
#' normal, and is close in total variation to Poisson(E K_n).
#'
#' Since the D_i are independent Bernoulli with known unequal
#' probabilities, the exact null law of K_n is Poisson-binomial, and it
#' is convolved directly here rather than approximated -- a fixed
#' n-step recursion with no sampling and no early exit, so the two-sided
#' p-value is exact and identical in both language arms.  The
#' normal-approximation deviate is reported alongside.
#'
#' @param partition Vector of cluster labels, one per observation; only
#'   the number of distinct labels matters.  May instead be a single
#'   integer cluster count, in which case \code{n} is required.
#' @param alpha Dirichlet-process concentration M.  Default 1.
#' @param n Sample size; inferred from \code{partition} when it is a
#'   vector.
#' @return list: K, E_K, var_K, z, p_value, p_normal, alpha, n, method.
#' @examples
#' Dpsing(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), alpha = 1)$p_value
#' @export
Dpsing <- function(partition, alpha = 1, n = NULL) {
  if (length(partition) == 1L && is.numeric(partition) &&
    partition == round(partition) && !is.null(n)) {
    K <- as.integer(partition)
    nn <- as.integer(n)
  } else {
    K <- length(unique(partition))
    nn <- if (is.null(n)) length(partition) else as.integer(n)
  }
  M <- as.numeric(alpha)
  if (M <= 0) stop("alpha must be positive")
  if (nn < 1) stop("n must be at least 1")
  if (K < 1 || K > nn) stop("cluster count must lie in 1..n")

  i <- seq_len(nn)
  p <- M / (M + i - 1)
  e_k <- sum(p)
  var_k <- sum(M * (i - 1) / (M + i - 1)^2)

  pmf <- 1
  for (pi in p) {
    nxt <- numeric(length(pmf) + 1L)
    nxt[seq_along(pmf)] <- nxt[seq_along(pmf)] + pmf * (1 - pi)
    nxt[seq_along(pmf) + 1L] <- nxt[seq_along(pmf) + 1L] + pmf * pi
    pmf <- nxt
  }
  pk <- pmf[K + 1L]
  tol <- 1e-12 * max(1, pk)
  p_exact <- min(1, max(0, sum(pmf[pmf <= pk + tol])))

  if (var_k > 0) {
    z <- (K - e_k) / sqrt(var_k)
    p_norm <- min(1, 2 * stats::pnorm(abs(z), lower.tail = FALSE))
  } else {
    z <- NaN
    p_norm <- NaN
  }
  list(
    K = K, E_K = e_k, var_K = var_k, z = z,
    p_value = p_exact, p_normal = p_norm, alpha = M, n = nn,
    method = paste(
      "DP cluster-count test, exact Poisson-binomial",
      "(Ghosal and van der Vaart 2017 Prop. 4.8; Antoniak 1974)"
    )
  )
}
