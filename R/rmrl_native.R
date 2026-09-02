# morie.fn -- function file (rootcoder007/morie)
# Reward machines and Q-Learning for Reward Machines (QRM).
#
# Toro Icarte, R., Klassen, T. Q., Valenzano, R., & McIlraith, S. A.
# (2018) "Using Reward Machines for High-Level Task Specification and
# Decomposition in Reinforcement Learning", ICML, PMLR 80.
#
# A reward machine (Definition 3.1) is a tuple <U, u0, delta_u,
# delta_r> over a set of propositional symbols P: a finite set of
# machine states U, an initial state u0, a state-transition function
# delta_u : U x 2^P -> U, and a reward-transition function delta_r.
# At each step the machine reads the truth assignment sigma_t = L(s_t)
# produced by a labelling function L : S -> 2^P, moves to
# u_{t+1} = delta_u(u_t, sigma_t), and emits delta_r(u_t, u_{t+1}).
#
# A machine is simple (Definition 3.2) when every delta_r(u, u') is a
# constant; an edge is written <phi, c>, meaning "take this edge when
# the truth assignment satisfies phi, and pay c".
#
# The reward may be non-Markovian in the environment state. Folding
# the history into U makes the joint process Markovian again
# (Definition 3.3, MDPRM) and exposes the task structure.
#
# QRM (Algorithm 1) keeps one q-function per machine state and after
# every real environment step (s, a, s') updates ALL of them
# counterfactually: for each machine state u_j compute
# u_k = delta_u(u_j, L(s')) and the reward it would have paid, then
#   q_j(s, a) <-alpha- r + gamma * max_a' q_k(s', a')
# (or just r at a dead end). One transition trains every sub-policy
# at once; the update is off-policy and no sub-policy is pruned, so
# QRM converges to an optimal policy in the tabular case.
#
# qlearn_flat is plain tabular q-learning on the product state (s, u):
# the honest baseline that sees the same information but gets only one
# update per step.

#' .rmrl_key
#'
#' A step of the rmrl_native implementation. Called by \code{morie_rmrl}, \code{morie_rmrl_qlearn_flat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ... Passed through.
#' @return A character value.
#' @export
.rmrl_key <- function(...) {
  paste(vapply(list(...), function(x) as.character(x), character(1)),
        collapse="\r")
}

#' .rmrl_compile
#'
#' A step of the rmrl_native implementation. Called by \code{morie_rmrl_reward_machine}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param phi Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return The value of \code{function}.
#' @export
.rmrl_compile <- function(phi) {
  if (is.null(phi) ||
      (is.character(phi) && length(phi) == 1L && tolower(phi) == "true")) {
    return(function(sigma) TRUE)
  }
  if (is.character(phi) && length(phi) == 1L) {
    pos <- phi
    neg <- character(0)
  } else if (is.list(phi) && length(phi) == 2L) {
    pos <- as.character(unlist(phi[[1L]]))
    neg <- as.character(unlist(phi[[2L]]))
  } else {
    stop(sprintf(paste0("reward_machine: formula must be 'true', a ",
                        "proposition name, or list(positive, negative), ",
                        "got %s"), paste(deparse(phi), collapse=" ")))
  }
  function(sigma) {
    sg <- as.character(sigma)
    all(pos %in% sg) && !any(neg %in% sg)
  }
}

#' A simple reward machine <U, u0, delta_u, delta_r> (Defs 3.1-3.2)
#'
#' Edges are list(u, formula, u_next, reward). formula is either the
#' string "true" or list(positive, negative) of proposition names. Edges
#' are tested in the order given and the first match wins; if none
#' matches, the machine stays in u and pays 0.
#'
#' @param edges See Usage.
#' @param u0 Coerced to character by the body, with \code{as.character}. Defaults to \code{0}.
#' @param terminal Coerced to character by the body, with \code{as.character}. Defaults to \code{c()}.
#' @return The value of \code{m}, as built in the body.
#' @export
morie_rmrl_reward_machine <- function(edges, u0=0, terminal=c()) {
  # A simple reward machine <U, u0, delta_u, delta_r> (Defs 3.1-3.2).
  # Edges are list(u, formula, u_next, reward). formula is either the
  # string "true" or list(positive, negative) of proposition names.
  # Edges are tested in the order given and the first match wins; if
  # none matches, the machine stays in u and pays 0.
  m <- list(u0=u0, terminal=as.character(terminal), edges=list(),
            states=unique(c(as.character(u0), as.character(terminal))))
  for (e in edges) {
    if (length(e) != 4L) {
      stop("reward_machine: each edge must be (u, formula, u_next, reward)")
    }
    u <- e[[1L]]
    phi <- e[[2L]]
    u2 <- e[[3L]]
    cc <- e[[4L]]
    ku <- as.character(u)
    m$edges[[ku]] <- c(m$edges[[ku]],
                       list(list(test=.rmrl_compile(phi), u2=u2,
                                 c=as.numeric(cc))))
    m$states <- unique(c(m$states, ku, as.character(u2)))
  }
  class(m) <- "morie_reward_machine"
  m
}

#' (delta_u(u, sigma), delta_r(u, delta_u(u, sigma)))
#'
#' A step of the rmrl_native implementation. Called by \code{morie_rmrl}, \code{morie_rmrl_qlearn_flat}, \code{morie_rmrl_reward_machine_run}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param machine A list; the body reads \code{$edges}, \code{$terminal} from it.
#' @param u Coerced to character by the body, with \code{as.character}.
#' @param sigma See Usage.
#' @return A list with \code{u}, \code{reward}.
#' @export
morie_rmrl_machine_step <- function(machine, u, sigma) {
  # (delta_u(u, sigma), delta_r(u, delta_u(u, sigma))).
  if (as.character(u) %in% machine$terminal) {
    return(list(u=u, reward=0.0))
  }
  for (edge in machine$edges[[as.character(u)]]) {
    if (edge$test(sigma)) {
      return(list(u=edge$u2, reward=edge$c))
    }
  }
  list(u=u, reward=0.0)
}

#' Drive a machine over a sequence of truth assignments. labels is
#'
#' sigma_0, sigma_1, ..., i.e. L(s) for each visited state. Returns the
#' machine-state trajectory and the rewards emitted.
#'
#' @param machine A list; the body reads \code{$terminal}, \code{$u0} from it.
#' @param labels See Usage.
#' @return A list with \code{estimate}, \code{states}, \code{rewards}, \code{total_reward}, \code{final_state}, \code{accepted}, \code{method}.
#' @export
morie_rmrl_reward_machine_run <- function(machine, labels) {
  # Drive a machine over a sequence of truth assignments. labels is
  # sigma_0, sigma_1, ..., i.e. L(s) for each visited state. Returns
  # the machine-state trajectory and the rewards emitted.
  u <- machine$u0
  us <- list(u)
  rs <- numeric(0)
  for (sigma in labels) {
    st <- morie_rmrl_machine_step(machine, u, sigma)
    u <- st$u
    us <- c(us, list(u))
    rs <- c(rs, st$reward)
  }
  list(
    estimate=us,
    states=us,
    rewards=rs,
    total_reward=sum(rs),
    final_state=u,
    accepted=as.character(u) %in% machine$terminal,
    method="reward machine run (Icarte et al. 2018 Def. 3.1)"
  )
}

#' Epsilon-greedy with ties broken UNIFORMLY AT RANDOM. The table
#'
#' starts all-zero, so every action ties; deterministic tie-breaking
#' would turn the initial behaviour into a systematic drift. rng is a
#' .ghc_rng state: the Python arm draws rng.random() from
#' np.random.default_rng(seed), bit-identical to .ghc_unif here.
#'
#' @param row A vector; indexed elementwise.
#' @param A A vector; its length is taken and its elements indexed.
#' @param epsilon Passed to \code{<}.
#' @param rng Passed to \code{.ghc_unif}.
#' @return The value of \code{[[}.
#' @export
.rmrl_eps_greedy <- function(row, A, epsilon, rng) {
  # epsilon-greedy with ties broken UNIFORMLY AT RANDOM. The table
  # starts all-zero, so every action ties; deterministic tie-breaking
  # would turn the initial behaviour into a systematic drift.
  # rng is a .ghc_rng state: the Python arm draws rng.random() from
  # np.random.default_rng(seed), bit-identical to .ghc_unif here.
  if (.ghc_unif(rng, 1L) < epsilon) {
    return(A[[as.integer(.ghc_unif(rng, 1L) * length(A)) + 1L]])
  }
  bv <- NULL
  best <- list()
  for (a in A) {
    v <- row[[as.character(a)]]
    if (is.null(bv) || v > bv) {
      bv <- v
      best <- list(a)
    } else if (v == bv) {
      best <- c(best, list(a))
    }
  }
  if (length(best) == 1L) {
    return(best[[1L]])
  }
  best[[as.integer(.ghc_unif(rng, 1L) * length(best)) + 1L]]
}

#' morie_rmrl
#'
#' A step of the rmrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param machines A vector; its length is taken and its elements indexed.
#' @param states Coerced to list by the body, with \code{as.list}.
#' @param actions Coerced to list by the body, with \code{as.list}.
#' @param step A function; the body checks with \code{is.function}.
#' @param label A function; the body checks with \code{is.function}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param epsilon Passed to \code{.rmrl_eps_greedy}. Defaults to \code{0.1}.
#' @param episodes A count; the body uses it as \code{seq_len(...)}. Defaults to \code{500}.
#' @param horizon A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.
#' @param start Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.
#' @param dead_end A function; the body checks with \code{is.function}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param task_order Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return A list with \code{estimate}, \code{q}, \code{policy}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{n_qfunctions}, \code{episodes}, \code{method}.
#' @export
morie_rmrl <- function(machines, states, actions, step, label, gamma=0.9,
                       alpha=0.5, epsilon=0.1, episodes=500, horizon=100,
                       start=NULL, dead_end=NULL, seed=0,
                       task_order=NULL) {
  # Q-Learning for Reward Machines (Algorithm 1), tabular.
  # machines: one machine (from morie_rmrl_reward_machine) or a list
  # of them (the paper's multi-task setting). step(s, a) returns
  # s_next or list(s_next, done). label(s) returns the propositions
  # true in s. Returns q as a named list keyed "task\ru\rs" of named
  # numeric vectors over actions; policy keyed "task\ru\rs"; returns
  # per episode; mean_return_last over the final tenth.
  if (inherits(machines, "morie_reward_machine")) {
    machines <- list(machines)
  }
  if (length(machines) == 0L) {
    stop("rmrl: need at least one reward machine")
  }
  S <- as.list(states)
  A <- as.list(actions)
  if (length(S) == 0L || length(A) == 0L) {
    stop("rmrl: states and actions must be non-empty")
  }
  if (!is.function(step) || !is.function(label)) {
    stop("rmrl: step and label must be callable")
  }
  episodes <- as.integer(episodes)
  horizon <- as.integer(horizon)
  if (episodes < 1L || horizon < 1L) {
    stop("rmrl: episodes and horizon must be >= 1")
  }
  de <- if (is.function(dead_end)) dead_end else function(s) FALSE
  s0 <- if (is.function(start)) {
    start
  } else if (is.null(start)) {
    function() S[[1L]]
  } else {
    function() start
  }
  if (is.null(task_order)) {
    task_order <- (seq_len(episodes) - 1L) %% length(machines)
  } else {
    task_order <- as.integer(task_order)
    if (length(task_order) < episodes) {
      stop("rmrl: task_order shorter than episodes")
    }
  }
  rng <- .ghc_rng(seed)
  akeys <- vapply(A, as.character, character(1))
  # q[["i\ru"]][["s"]] is a named numeric vector over actions --
  # one q-function per machine state (line 2).
  q <- list()
  for (i in seq_along(machines)) {
    mm <- machines[[i]]
    for (u in mm$states) {
      tab <- list()
      for (s in S) {
        tab[[as.character(s)]] <- stats::setNames(rep(0.0, length(A)),
                                                  akeys)
      }
      q[[.rmrl_key(i - 1L, u)]] <- tab
    }
  }
  returns <- numeric(0)
  for (l in seq_len(episodes)) {
    i <- task_order[l] + 1L
    mm <- machines[[i]]
    u <- mm$u0
    s <- s0()
    total <- 0.0
    for (t_ in seq_len(horizon)) {
      if (isTRUE(de(s)) || as.character(u) %in% mm$terminal) {
        break
      }
      a <- .rmrl_eps_greedy(q[[.rmrl_key(i - 1L, u)]][[as.character(s)]],
                            A, epsilon, rng)
      out <- step(s, a)
      done <- FALSE
      if (is.list(out) && length(out) == 2L) {
        s1 <- out[[1L]]
        done <- isTRUE(out[[2L]])
      } else {
        s1 <- out
      }
      sigma <- as.character(unlist(label(s1)))
      dead <- isTRUE(de(s1))
      ks <- as.character(s)
      ks1 <- as.character(s1)
      ka <- as.character(a)
      # Lines 12-20: update EVERY q-function of EVERY machine.
      for (o in seq_along(machines)) {
        mo <- machines[[o]]
        for (uj in mo$states) {
          st <- morie_rmrl_machine_step(mo, uj, sigma)
          uk <- st$u
          r <- st$reward
          if (dead || as.character(uk) %in% mo$terminal) {
            target <- r
          } else {
            target <- r + gamma * max(q[[.rmrl_key(o - 1L, uk)]][[ks1]])
          }
          kj <- .rmrl_key(o - 1L, uj)
          cur <- q[[kj]][[ks]][[ka]]
          q[[kj]][[ks]][[ka]] <- cur + alpha * (target - cur)
        }
      }
      stp <- morie_rmrl_machine_step(mm, u, sigma)
      u <- stp$u
      total <- total + stp$reward
      s <- s1
      if (done || dead) {
        break
      }
    }
    returns <- c(returns, total)
  }
  policy <- list()
  for (key in names(q)) {
    tab <- q[[key]]
    for (s in S) {
      row <- tab[[as.character(s)]]
      policy[[paste(key, as.character(s), sep="\r")]] <-
        A[[which.max(row)]]
    }
  }
  tenth <- max(1L, episodes %/% 10L)
  list(
    estimate=q,
    q=q,
    policy=policy,
    returns=returns,
    mean_return_last=sum(utils::tail(returns, tenth)) / tenth,
    mean_return_first=sum(returns[seq_len(tenth)]) / tenth,
    n_qfunctions=length(q),
    episodes=episodes,
    method="QRM (Icarte et al. 2018, Algorithm 1)"
  )
}

#' morie_rmrl_qlearn_flat
#'
#' A step of the rmrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param machine A list; the body reads \code{$states}, \code{$terminal}, \code{$u0} from it.
#' @param states Coerced to list by the body, with \code{as.list}.
#' @param actions Coerced to list by the body, with \code{as.list}.
#' @param step Accepted by the signature and not used anywhere in the body.
#' @param label Accepted by the signature and not used anywhere in the body.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param epsilon Passed to \code{.rmrl_eps_greedy}. Defaults to \code{0.1}.
#' @param episodes A count; the body uses it as \code{seq_len(...)}. Defaults to \code{500}.
#' @param horizon A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.
#' @param start Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.
#' @param dead_end A function; the body checks with \code{is.function}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{q}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{method}.
#' @export
morie_rmrl_qlearn_flat <- function(machine, states, actions, step, label,
                                   gamma=0.9, alpha=0.5, epsilon=0.1,
                                   episodes=500, horizon=100, start=NULL,
                                   dead_end=NULL, seed=0) {
  # Tabular q-learning on the product state (s, u): the same
  # information as QRM but only the experienced (s, u) pair is
  # updated per step, with no counterfactual sweep. This is the
  # baseline the decomposition claim is measured against.
  S <- as.list(states)
  A <- as.list(actions)
  de <- if (is.function(dead_end)) dead_end else function(s) FALSE
  s0 <- if (is.function(start)) {
    start
  } else if (is.null(start)) {
    function() S[[1L]]
  } else {
    function() start
  }
  rng <- .ghc_rng(seed)
  akeys <- vapply(A, as.character, character(1))
  q <- list()
  for (u in machine$states) {
    for (s in S) {
      q[[.rmrl_key(u, s)]] <- stats::setNames(rep(0.0, length(A)), akeys)
    }
  }
  episodes <- as.integer(episodes)
  horizon <- as.integer(horizon)
  returns <- numeric(0)
  for (l_ in seq_len(episodes)) {
    u <- machine$u0
    s <- s0()
    total <- 0.0
    for (t_ in seq_len(horizon)) {
      if (isTRUE(de(s)) || as.character(u) %in% machine$terminal) {
        break
      }
      a <- .rmrl_eps_greedy(q[[.rmrl_key(u, s)]], A, epsilon, rng)
      out <- step(s, a)
      done <- FALSE
      if (is.list(out) && length(out) == 2L) {
        s1 <- out[[1L]]
        done <- isTRUE(out[[2L]])
      } else {
        s1 <- out
      }
      st <- morie_rmrl_machine_step(machine, u,
                                    as.character(unlist(label(s1))))
      u1 <- st$u
      r <- st$reward
      if (isTRUE(de(s1)) || as.character(u1) %in% machine$terminal) {
        target <- r
      } else {
        target <- r + gamma * max(q[[.rmrl_key(u1, s1)]])
      }
      ka <- as.character(a)
      cur <- q[[.rmrl_key(u, s)]][[ka]]
      q[[.rmrl_key(u, s)]][[ka]] <- cur + alpha * (target - cur)
      total <- total + r
      s <- s1
      u <- u1
      if (done || isTRUE(de(s1))) {
        break
      }
    }
    returns <- c(returns, total)
  }
  tenth <- max(1L, episodes %/% 10L)
  list(
    estimate=q,
    q=q,
    returns=returns,
    mean_return_last=sum(utils::tail(returns, tenth)) / tenth,
    mean_return_first=sum(returns[seq_len(tenth)]) / tenth,
    method="tabular q-learning on (s, u)"
  )
}

#' morie_rmrl_cheatsheet
#'
#' A step of the rmrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_rmrl_cheatsheet <- function() {
  paste0(
    "rmrl: reward machine <U, u0, delta_u, delta_r> (Icarte ",
    "2018 Def. 3.1) + QRM (Alg. 1): one q-function per machine ",
    "state, every step updates ALL of them counterfactually via ",
    "u_k = delta_u(u_j, L(s')). qlearn_flat is the (s,u) ",
    "baseline. Handles rewards non-Markovian in s."
  )
}

# compact aliases per ledger/NAMING.md
morie_rmrl_qrm <- morie_rmrl
morie_rmrl_rewardmachine <- morie_rmrl_reward_machine
