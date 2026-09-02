# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero Reanalyze targets and prioritised-replay weights
#'
#' z_t = sum_\{j<n\} gamma^j u_\{t+j\} + gamma^n nu_\{t+n\};
#' pi_t(a) = N_t(a)/sum_b N_t(b); p_i = |nu_i - z_i|;
#' P(i) = p_i^alpha / sum_k p_k^alpha; w_i = ((1/N)(1/P(i)))^beta.
#'
#' @param rewards Environment rewards u_1..u_T.
#' @param freshvalues Root values from the fresh search, length T.
#' @param visits Fresh root visit counts, T x A.
#' @param n Bootstrap horizon.
#' @param gamma Discount factor.
#' @param alpha,beta Prioritised-replay exponents.
#' @param oldvalues Values for the priority gap; NULL uses freshvalues.
#'
#' @return List with target, policy, priority, prob, weight, T, A, n,
#'   gamma.
#' @references Schrittwieser et al. (2020), arXiv:1911.08265, Appendix H
#'   and Methods (training).  Read from the ar5iv rendering.
#' @export
#' @examples
#' Mzreanal(rewards = c(1, 2, 3, 4, 5, 6, 7, 8), freshvalues = c(1, 2, 3, 4, 5, 6, 7, 8), visits = c(1, 2, 3, 4, 5, 6, 7, 8))
Mzreanal <- function(rewards, freshvalues, visits, n = 5, gamma = 0.997,
                     alpha = 1, beta = 1, oldvalues = NULL) {
  u <- .t1_vec(rewards)
  nu <- .t1_vec(freshvalues)
  T <- length(u)
  if (length(nu) != T) stop("rewards and freshvalues must have the same length")
  Vs <- .t1_mat(visits)
  if (nrow(Vs) != T) stop("visits must have one row per time step")
  A <- ncol(Vs)
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  g <- as.numeric(gamma)
  z <- numeric(T)
  for (t in seq_len(T)) {
    s <- 0
    for (j in 0:(n - 1L)) if (t + j <= T) s <- s + g^j * u[t + j]
    if (t + n <= T) s <- s + g^n * nu[t + n]
    z[t] <- s
  }
  tot <- rowSums(Vs)
  pol <- Vs / ifelse(tot == 0, 1, tot)
  pol[tot == 0, ] <- 0
  ov <- if (is.null(oldvalues)) nu else .t1_vec(oldvalues)
  if (length(ov) != T) stop("oldvalues must have length T")
  pr <- abs(ov - z)
  pa <- pr^as.numeric(alpha)
  sp <- sum(pa)
  prob <- if (sp == 0) rep(1 / T, T) else pa / sp
  w <- (1 / (T * prob))^as.numeric(beta)
  .t1_result(
    target = z, policy = pol, priority = pr, prob = prob,
    weight = w, T = T, A = A, n = n, gamma = g,
    method = "MuZero Reanalyze targets (Schrittwieser et al. 2020 App. H)"
  )
}
