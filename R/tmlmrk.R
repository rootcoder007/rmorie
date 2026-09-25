# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the long-run value of a policy in a finite Markov decision process
#'
#' The value is the stationary average reward
#' \code{V^pi = sum_s d^pi(s) r*(s, pi(s))}, with \code{d^pi} the
#' stationary distribution of the transition matrix induced by
#' \code{pi}.  The stationary average avoids inventing a discount factor
#' the caller never supplied and is what a single long trajectory
#' identifies.
#'
#' The reward model is the cell mean \code{r(s, a)}; the behaviour policy
#' is the empirical action frequency per state.  The clever covariate is
#' the importance ratio \code{H = I(a = pi(s))/b(pi(s)|s)} and
#' \code{r*(s, pi(s)) = r(s, pi(s)) + eps/b(pi(s)|s)} with
#' \code{eps = sum H (R - r(s, a))/sum H^2}.  Transitions are counted
#' from consecutive entries, so the input must be one trajectory in time
#' order.
#'
#' @param state State label at each step, in time order.
#' @param action Action taken at each step.
#' @param reward Reward received at each step.
#' @param policy Action the evaluated policy takes in each distinct
#'   state, in sorted order of the state labels.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_states}, \code{n}.
#' @references Liao, P., Klasnja, P. & Murphy, S. A. (2021). JASA
#'   116(533):382-391, \doi{10.1080/01621459.2020.1807993} (the influence
#'   curve \code{w(S) H (R + h(S') - h(S) - V)});
#'   Murphy, S. A. (2003). JRSS B 65(2):331-355; van der Laan,
#'   M. J. & Rubin, D. (2006). IJB 2(1):11.
#' @export
#' @examples
#' Tmlmrk(state = c(1, 2, 3, 4, 5, 6, 7, 8), action = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   reward = c(1, 2, 3, 4, 5, 6, 7, 8), policy = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlmrk <- function(state, action, reward, policy) {
  sv <- as.numeric(state)
  av <- as.numeric(action)
  rv <- as.numeric(reward)
  pv <- as.numeric(policy)
  n <- length(sv)
  if (n < 2L || length(av) != n || length(rv) != n)
    stop("Tmlmrk: state, action and reward must share one length >= 2")
  states <- sort(unique(sv))
  ns <- length(states)
  if (length(pv) != ns)
    stop("Tmlmrk: policy must give one action per distinct state")
  si <- match(sv, states)
  pol <- pv
  b <- numeric(n)
  for (i in seq_len(n)) {
    s <- sv[i]
    tot <- sum(sv == s)
    hit <- sum(sv == s & abs(av - pol[si[i]]) < 1e-9)
    b[i] <- .s4_clip(hit / tot, 0.01, 1)
  }
  num <- numeric(ns)
  cntr <- numeric(ns)
  for (i in seq_len(n)) if (abs(av[i] - pol[si[i]]) < 1e-9) {
    num[si[i]] <- num[si[i]] + rv[i]
    cntr[si[i]] <- cntr[si[i]] + 1
  }
  rhat <- ifelse(cntr > 0, num / cntr, 0)
  H <- ifelse(abs(av - pol[si]) < 1e-9, 1, 0) / b
  Qobs <- rhat[si]
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (rv - Qobs)) / den else 0
  bst <- numeric(ns)
  for (k in seq_len(ns)) {
    rows <- which(si == k)
    bst[k] <- if (length(rows) > 0L) b[rows[1L]] else 1
  }
  rstar <- rhat + eps / bst
  P <- matrix(0, ns, ns)
  cnt <- numeric(ns)
  for (i in seq_len(n - 1L)) {
    k <- si[i]
    if (abs(av[i] - pol[k]) < 1e-9) {
      P[k, si[i + 1L]] <- P[k, si[i + 1L]] + 1
      cnt[k] <- cnt[k] + 1
    }
  }
  for (k in seq_len(ns)) if (cnt[k] > 0) P[k, ] <- P[k, ] / cnt[k] else P[k, k] <- 1
  A <- matrix(0, ns, ns)
  for (k in seq_len(ns)) for (j in seq_len(ns))
    A[k, j] <- (if (j == k) 1 else 0) - P[j, k]
  A[ns, ] <- 1
  rhs <- numeric(ns)
  rhs[ns] <- 1
  d <- as.numeric(solve(A, rhs))
  V <- sum(d * rstar)
  ## differential value h: (I - P) h = r* - V, pinned by d'h = 0
  Ah <- diag(ns) - P
  bh <- rstar - V
  Ah[ns, ] <- d
  bh[ns] <- 0
  h <- as.numeric(solve(Ah, bh))
  ## influence curve per transition (Liao, Klasnja & Murphy 2021):
  ## w(S) H (R + h(S') - h(S) - V); martingale differences, so the
  ## variance of their mean is sum(D^2) / (n - 1)^2
  m1 <- n - 1L
  s0 <- si[seq_len(m1)]
  emp <- as.numeric(table(factor(s0, levels = seq_len(ns)))) / m1
  w <- ifelse(emp[s0] > 0, d[s0] / emp[s0], 0)
  ic <- w * H[seq_len(m1)] * (rv[seq_len(m1)] + h[si[2:n]] - h[s0] - V)
  se <- sqrt(sum(ic^2)) / m1
  .t1_result(estimate = V, se = se, eps = eps, n_states = ns, n = n,
             method = "TMLE for the long-run average reward of a policy in an MDP")
}
