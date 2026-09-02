# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero recurrent inference step
#'
#' r^k, s^k = g(s^\{k-1\}, a^k) followed by p^k, v^k = f(s^k).
#'
#' @param state Hidden state s^\{k-1\}.
#' @param action Action a^k.
#' @param dynamics g, returning list(reward, state).
#' @param prediction f, returning list(policy, value); NULL skips it.
#'
#' @return List with state, reward, policy, value.
#' @references Schrittwieser et al. (2020), arXiv:1911.08265, Methods
#'   (Search) and Equation (1).  Read from the ar5iv rendering.
#' @export
#' @examples
#' dynamics <- function(s, a) list(0.5, s + a)
#' Mzrecur(state = 0, action = 1, dynamics = dynamics)
Mzrecur <- function(state, action, dynamics, prediction = NULL) {
  out <- dynamics(state, action)
  r <- out[[1]]
  s <- out[[2]]
  p <- NULL
  v <- NULL
  if (!is.null(prediction)) {
    pv <- prediction(s)
    p <- pv[[1]]
    v <- as.numeric(pv[[2]])
  }
  .t1_result(
    state = s, reward = as.numeric(r), policy = p, value = v,
    method = "MuZero recurrent inference (Schrittwieser et al. 2020)"
  )
}
