# SPDX-License-Identifier: AGPL-3.0-or-later
#' Backward-targeted Q-learning for a multi-stage treatment regime
#'
#' Plain Q-learning is a sequence of regressions and inherits a bias at
#' every stage, because the maximisation is applied to a fitted value and
#' a fitted value is biased in the direction the maximiser looks.
#' Targeting each stage before carrying it back removes the first-order
#' part: at stage \code{t} the clever covariate
#' \code{H = I(a = a*(s))/b(a*(s)|s)} fluctuates \code{Q_t} along the
#' regime being evaluated, and only the targeted
#' \code{V_t(s) = Q*_t(s, a*(s))} is passed back as part of the
#' pseudo-outcome.  The behaviour policy is logistic in the state, so
#' actions are assumed binary.
#'
#' Rows must be grouped by stage, with the subjects in the same order
#' within each stage and equal counts per stage.
#'
#' @param state State at each subject-stage.
#' @param action Binary action at each subject-stage.
#' @param reward Reward collected at each subject-stage.
#' @param time Stage index of each row.
#' @return List with \code{estimate}, \code{se}, \code{n_stages},
#'   \code{n_subj}, \code{n}.
#' @references Murphy, S. A. (2003). JRSS B 65(2):331-355; Petersen, M.
#'   et al. (2014). Journal of Causal Inference 2(2):147-185.
#' @export
#' @examples
#' Tmlqlc(state = c(1, 2, 3, 4, 5, 6, 7, 8), action = c(1, 2, 3, 4, 5, 6, 7, 8), reward = c(1, 2, 3, 4, 5, 6, 7, 8), time = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlqlc <- function(state, action, reward, time) {
  sv <- as.numeric(state)
  av <- as.numeric(action)
  rv <- as.numeric(reward)
  tv <- as.numeric(time)
  n <- length(sv)
  if (n == 0L || length(av) != n || length(rv) != n || length(tv) != n)
    stop("Tmlqlc: state, action, reward and time must share one length")
  stages <- sort(unique(tv))
  T <- length(stages)
  rows <- lapply(stages, function(s) which(tv == s))
  m <- length(rows[[1L]])
  for (r in rows) if (length(r) != m)
    stop("Tmlqlc: every stage must have the same number of rows")
  V <- numeric(m)
  ic <- numeric(m)
  for (t in seq(T, 1L)) {
    idx <- rows[[t]]
    des <- cbind(1, sv[idx], av[idx], sv[idx] * av[idx])
    pseudo <- rv[idx] + (if (t < T) V else 0)
    qb <- .s4_ols(des, pseudo)$beta
    bb <- .s4_glmbin(cbind(1, sv[idx]), av[idx])
    b1 <- .s4_clip(.s4_expit(as.numeric(cbind(1, sv[idx]) %*% bb)), 0.025, 0.975)
    q <- function(a) as.numeric(cbind(1, sv[idx], a, sv[idx] * a) %*% qb)
    q1 <- q(1)
    q0 <- q(0)
    astar <- ifelse(q1 >= q0, 1, 0)
    ba <- ifelse(astar > 0.5, b1, 1 - b1)
    H <- ifelse(abs(av[idx] - astar) < 0.5, 1, 0) / ba
    Qobs <- ifelse(av[idx] > 0.5, q1, q0)
    den <- sum(H * H)
    eps <- if (den != 0) sum(H * (pseudo - Qobs)) / den else 0
    Vnew <- ifelse(astar > 0.5, q1, q0) + eps / ba
    ic <- H * (pseudo - Qobs - eps * H) + ic
    V <- Vnew
  }
  psi <- sum(V) / m
  ic <- ic + V - psi
  se <- if (m > 1L) sqrt(sum((ic - mean(ic))^2) / (m - 1) / m) else NaN
  .t1_result(estimate = psi, se = se, n_stages = T, n_subj = m, n = n,
             method = "Backward-targeted Q-learning for a multi-stage regime")
}
