# Monte Carlo tree search with UCT.
# Source: Browne et al. (2012), IEEE Trans. Comput. Intell. AI Games
# 4(1), 1-43, Sec. 3.3 (UCT), Algorithm 2 (UCT search), Algorithm 3
# (negamax backup); Coulom (2006), Computers and Games, 72-83
# (fetched-wave3/MCTS_survey_Browne_2012.pdf).  Mirrors Python
# morie.fn.mctsr exactly: same expansion order, same random-rollout
# RNG stream, same tie-breaking.

#' Monte Carlo tree search with UCT and random rollouts
#'
#' Each iteration runs Browne et al. Algorithm 2: TREEPOLICY descends
#' by BESTCHILD while fully expanded, EXPAND adds one untried action,
#' DEFAULTPOLICY plays uniformly at random to a terminal state, and
#' BACKUP propagates the reward. Selection uses
#' UCT_j = Xbar_j + 2*Cp*sqrt(2*log(n)/n_j), unvisited children
#' valued Inf. Cp = 1/sqrt(2) is the Kocsis-Szepesvari value for
#' rewards in [0, 1].  Both survey backups are provided: "sum"
#' (Algorithm 2) and "negamax" (Algorithm 3, two-player zero-sum).
#' Both final-move criteria: "robust" (most-visited root child,
#' default) and "max" (highest mean reward).
#'
#' @param root_state Starting state.
#' @param actions Function(state) returning legal actions.
#' @param step Function(state, action) returning the next state.
#' @param reward Function(terminal_state) returning a numeric reward.
#' @param is_terminal Function(state) returning TRUE/FALSE.
#' @param n_iter Iterations (computational budget).
#' @param c Exploration constant Cp.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param backup "sum" (Algorithm 2) or "negamax" (Algorithm 3).
#' @param final "robust" (most visited) or "max" (highest mean).
#' @return A list with elements \code{action}, \code{root_visits},
#'   \code{child_visits}, \code{child_values}, \code{n_iter},
#'   \code{c}, \code{backup}, \code{final}, \code{seed},
#'   \code{method}.
#' @references Browne, C. B. et al. (2012). A survey of Monte Carlo
#'   tree search methods. IEEE Transactions on Computational
#'   Intelligence and AI in Games, 4(1), 1-43.
#' @export
morie_mctsr <- function(root_state, actions, step, reward, is_terminal,
                        n_iter = 200, c = 1 / sqrt(2), seed = 0,
                        backup = "sum", final = "robust") {
  if (!backup %in% c("sum", "negamax"))
    stop("backup must be 'sum' or 'negamax'")
  if (!final %in% c("robust", "max"))
    stop("final must be 'robust' or 'max'")
  e <- .ghc_rng(seed)
  # node store in parallel vectors; index 1 = root
  st <- list(root_state)
  par <- c(NA_integer_)
  act <- list(NULL)
  kids <- list(integer(0))
  untried <- list(as.list(actions(root_state)))
  Nv <- 0
  Qv <- 0
  if (!length(untried[[1]]) && !is_terminal(root_state))
    stop("root has no legal actions")

  best_child <- function(v) {
    ch <- kids[[v]]
    vals <- vapply(ch, function(k) {
      if (Nv[k] == 0) Inf else
        Qv[k] / Nv[k] + 2 * c * sqrt(2 * log(Nv[v]) / Nv[k])
    }, numeric(1))
    best <- max(vals)
    ties <- ch[vals == best]
    if (length(ties) == 1L) return(ties)
    ties[floor(.ghc_unif(e, 1) * length(ties)) + 1L]
  }

  for (it in seq_len(as.integer(n_iter))) {
    v <- 1L
    repeat {
      if (is_terminal(st[[v]])) break
      if (length(untried[[v]])) {
        a <- untried[[v]][[1]]
        untried[[v]] <- untried[[v]][-1]
        s2 <- step(st[[v]], a)
        st[[length(st) + 1L]] <- s2
        nid <- length(st)
        par[nid] <- v
        act[[nid]] <- a
        kids[[nid]] <- integer(0)
        untried[[nid]] <- as.list(actions(s2))
        Nv[nid] <- 0; Qv[nid] <- 0
        kids[[v]] <- c(kids[[v]], nid)
        v <- nid
        break
      }
      if (!length(kids[[v]])) break
      v <- best_child(v)
    }
    s <- st[[v]]
    while (!is_terminal(s)) {
      acts <- actions(s)
      if (!length(acts)) break
      s <- step(s, acts[[floor(.ghc_unif(e, 1) * length(acts)) + 1L]])
    }
    delta <- as.numeric(reward(s))
    node <- v
    while (!is.na(node)) {
      Nv[node] <- Nv[node] + 1
      Qv[node] <- Qv[node] + delta
      if (backup == "negamax") delta <- -delta
      node <- par[node]
    }
  }

  ch <- kids[[1]]
  if (!length(ch)) stop("no children expanded; increase n_iter")
  pick <- if (final == "robust") ch[which.max(Nv[ch])] else
    ch[which.max(ifelse(Nv[ch] > 0, Qv[ch] / Nv[ch], -Inf))]
  labs <- vapply(ch, function(k) as.character(act[[k]]), character(1))
  list(action = act[[pick]], root_visits = Nv[1],
       child_visits = setNames(as.integer(Nv[ch]), labs),
       child_values = setNames(
         ifelse(Nv[ch] > 0, Qv[ch] / Nv[ch], 0), labs),
       n_iter = as.integer(n_iter), c = as.numeric(c),
       backup = backup, final = final, seed = seed,
       method = sprintf("UCT MCTS (Browne et al. 2012, Algorithm %s)",
                        if (backup == "sum") "2" else "3"))
}
