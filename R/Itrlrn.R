# SPDX-License-Identifier: AGPL-3.0-or-later
#' Iterative Q-learning (backward induction) for a dynamic regime
#'
#' Q-learning solves the regime backwards.  At the final stage the
#' Q-function is fitted by least squares on the stage covariates, the
#' action and their interaction; at earlier stages the same regression
#' is run on the pseudo-outcome R_t + gamma max_a' Q_\{t+1\}(s', a').  The
#' optimal rule at stage t is 1\{Q_t(s,1) > Q_t(s,0)\} and the value of
#' the regime is the average of max_a Q_1(S_1, a).  Every step is a
#' closed-form least-squares solve; nothing is simulated.
#'
#' Formula: Q_t(s,a) <- R_t + gamma max_a' Q_\{t+1\}(s', a').
#'
#' @param state Stage covariates, one row per (subject, stage) record.
#' @param action Binary action in that record.
#' @param reward Reward received in that record.
#' @param time Stage index; every stage must hold the same number of
#'   records in the same subject order.
#' @param gamma Discount factor.
#' @return List with \code{estimate}, \code{value}, \code{stage_value},
#'   \code{coef}, \code{share_treated}, \code{n_stages},
#'   \code{n_subjects}, \code{gamma}, \code{n}, \code{method}.
#' @references Murphy (2003), Optimal dynamic treatment regimes, Journal
#'   of the Royal Statistical Society Series B 65(2):331-355,
#'   \doi{10.1111/1467-9868.00389}; Petersen et al (2014), Journal of
#'   Causal Inference 2(2):147-185. \doi{10.1515/jci-2013-0007}
#' @export
#' @examples
#' state <- rep(c(0, 1), 8)
#' action <- rep(c(1, 0), 8)
#' reward <- rep(c(1, 0), 8)
#' time <- rep(1:2, each = 8)
#' Itrlrn(state, action, reward, time)
Itrlrn <- function(state, action, reward, time, gamma = 1) {
  a <- .s03vec(action)
  r <- .s03vec(reward)
  tm <- .s03vec(time)
  n <- length(r)
  if (n == 0L) stop("iterative_q_learning: reward is empty")
  if (length(a) != n || length(tm) != n) stop("iterative_q_learning: action, reward and time have different lengths")
  if (any(a != 0 & a != 1)) stop("iterative_q_learning: action must be 0 or 1")
  S <- if (!is.null(state)) .s03mat(state) else matrix(0, n, 0)
  if (nrow(S) != n) stop("iterative_q_learning: state and reward have different lengths")
  k <- ncol(S)
  stages <- sort(unique(tm))
  T <- length(stages)
  if (T == 0L) stop("iterative_q_learning: no stages")
  idx <- lapply(stages, function(s) which(tm == s))
  m <- length(idx[[1]])
  if (any(vapply(idx, length, 0L) != m)) stop("iterative_q_learning: stages have different numbers of records")
  p <- 2L + 2L * k
  if (m <= p) stop("iterative_q_learning: too few records per stage for the Q-model")
  rowf <- function(sv, av) c(1, sv, av, av * sv)
  Vnext <- numeric(m)
  betas <- vector("list", T)
  shares <- numeric(T)
  values <- numeric(T)
  for (t in seq(T, 1L)) {
    g <- idx[[t]]
    Z <- t(vapply(seq_len(m), function(u) rowf(S[g[u], ], a[g[u]]), numeric(p)))
    tgt <- r[g] + gamma * Vnext
    b <- .s03lstsq(Z, tgt)
    q0 <- vapply(seq_len(m), function(u) sum(rowf(S[g[u], ], 0) * b), 0)
    q1 <- vapply(seq_len(m), function(u) sum(rowf(S[g[u], ], 1) * b), 0)
    Vnext <- ifelse(q1 > q0, q1, q0)
    betas[[t]] <- b
    shares[t] <- sum(q1 > q0) / m
    values[t] <- sum(Vnext) / m
  }
  .t1_result(estimate = values[1], value = values[1], stage_value = values,
             coef = unlist(betas), share_treated = shares, n_stages = T,
             n_subjects = m, gamma = gamma, n = n,
             method = "Q_t(s,a) <- R_t + gamma max_a' Q_{t+1}(s',a') by least squares, Murphy (2003)")
}
