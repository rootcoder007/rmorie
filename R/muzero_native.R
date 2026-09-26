# MuZero: MCTS over a learned latent model.
# Source: Schrittwieser, J. et al. (2020), "Mastering Atari, Go,
# Chess and Shogi by Planning with a Learned Model", Nature 588,
# arXiv:1911.08265.
#
# Native implementation mirroring Python morie.fn.muzero exactly: the
# same pUCT selection (eq. 2) with c1=1.25 and c2=19652, the same
# bootstrapped return (eqs. 3-4), the same tree-wide min-max Q
# normalisation (eq. 5), and the same one-call-per-simulation bound
# on the dynamics and prediction networks.

#' @keywords internal
#' @noRd
.ghc_muzero_gamma_rv <- function(alpha, e) {
  if (alpha < 1) {
    u <- .ghc_unif(e, 1L)
    return(.ghc_muzero_gamma_rv(alpha + 1, e) * (u ^ (1 / alpha)))
  }
  d <- alpha - 1 / 3
  cc <- 1 / sqrt(9 * d)
  repeat {
    x <- .ghc_norm(e, 1L)
    v <- (1 + cc * x)^3
    if (v <= 0) next
    u <- .ghc_unif(e, 1L)
    if (log(u) < 0.5 * x * x + d - d * v + d * log(v)) return(d * v)
  }
}

#' @keywords internal
#' @noRd
.ghc_muzero_add_noise <- function(prior, alpha, frac, seed) {
  if (alpha <= 0) stop("muzero: dirichlet_alpha must be > 0")
  if (frac < 0 || frac > 1)
    stop("muzero: exploration_fraction must lie in [0, 1]")
  e <- .ghc_rng(seed)
  g <- vapply(prior, function(p) .ghc_muzero_gamma_rv(alpha, e),
              numeric(1))
  noise <- g / sum(g)
  (1 - frac) * prior + frac * noise
}

#' @keywords internal
#' @noRd
.ghc_muzero_minmax_update <- function(mm, v) {
  if (is.null(mm$lo) || v < mm$lo) mm$lo <- v
  if (is.null(mm$hi) || v > mm$hi) mm$hi <- v
  mm
}

#' @keywords internal
#' @noRd
.ghc_muzero_minmax_norm <- function(mm, v) {
  if (is.null(mm$lo) || is.null(mm$hi)) return(v)
  if (mm$hi > mm$lo) return((v - mm$lo) / (mm$hi - mm$lo))
  v
}

#' @keywords internal
#' @noRd
.ghc_muzero_node_new <- function(prior = 0) {
  nd <- new.env(parent = emptyenv())
  nd$visits <- 0L
  nd$value_sum <- 0
  nd$prior <- prior
  nd$children <- list()
  nd$state <- NULL
  nd$reward <- 0
  nd$expanded <- FALSE
  nd
}

#' @keywords internal
#' @noRd
.ghc_muzero_node_value <- function(node) {
  if (node$visits > 0L) node$value_sum / node$visits else 0
}

#' @keywords internal
#' @noRd
.ghc_muzero_node_expand <- function(node, state, prior, A) {
  node$state <- state
  node$expanded <- TRUE
  for (i in seq_along(A)) {
    node$children[[A[[i]]]] <- .ghc_muzero_node_new(prior[i])
  }
  node
}

#' @keywords internal
#' @noRd
# Q(s, a) = r(s, a) + gamma V(s'): the pseudocode's
# child.reward + discount * child.value()
.ghc_muzero_edge_q <- function(child, gamma) {
  if (child$visits > 0L) child$reward + gamma * .ghc_muzero_node_value(child) else 0
}

#' @keywords internal
#' @noRd
.ghc_muzero_select <- function(node, A, mm, c1, c2, gamma) {
  total <- sum(vapply(A, function(a) node$children[[a]]$visits,
                       integer(1)))
  sqrt_total <- if (total > 0L) sqrt(total) else 0
  best <- -Inf
  best_a <- A[1]
  for (a in A) {
    ch <- node$children[[a]]
    expl <- ch$prior * sqrt_total / (1 + ch$visits) *
      (c1 + log((total + c2 + 1) / c2))
    q <- if (ch$visits > 0L) .ghc_muzero_minmax_norm(mm,
                                                     .ghc_muzero_edge_q(ch, gamma))
         else 0
    sc <- q + expl
    if (sc > best) { best <- sc
    best_a <- a }
  }
  best_a
}

#' @keywords internal
#' @noRd
.ghc_muzero_backup <- function(path, value, gamma, mm) {
  g <- value
  for (k in rev(seq_along(path))) {
    node <- path[[k]]
    node$value_sum <- node$value_sum + g
    node$visits <- node$visits + 1L
    if (k > 1L)   # the edge into this node
      mm <- .ghc_muzero_minmax_update(mm, node$reward + gamma * .ghc_muzero_node_value(node))
    g <- node$reward + gamma * g
  }
  list(mm = mm, path = path)
}

#' Run MuZero's MCTS
#'
#' @param observation Passed to representation.
#' @param actions Action space.
#' @param representation,dynamics,prediction h, g, f callables.
#' @param simulations Number of simulations.
#' @param gamma Discount.
#' @param c1,c2 pUCT constants.
#' @param dirichlet_alpha Optional root Dirichlet noise.
#' @param exploration_fraction Noise weight at the root.
#' @param temperature Temperature on visit-count distribution.
#' @param seed Seed for root noise.
#' @return A list with policy, action, value, visits, Q, prior, and
#'   bookkeeping.
#' @export
#' @keywords internal
morie_muzero <- function(observation, actions, representation,
                          dynamics, prediction, simulations = 50,
                          gamma = 0.997, c1 = 1.25, c2 = 19652,
                          dirichlet_alpha = NULL,
                          exploration_fraction = 0.25,
                          temperature = 1, seed = 0) {
  A <- as.list(actions)
  if (length(A) == 0L) stop("muzero: actions must be non-empty")
  for (fnname in c("representation", "dynamics", "prediction"))
    if (!is.function(get(fnname, inherits = TRUE)))
      stop(paste0("muzero: ", fnname, " must be callable"))
  simulations <- as.integer(simulations)
  if (simulations < 1L) stop("muzero: simulations must be >= 1")
  if (c2 <= 0) stop("muzero: c2 must be > 0")
  calls <- c(0L, 0L)
  predict <- function(s) {
    calls[2] <<- calls[2] + 1L
    out <- prediction(s)
    p <- as.numeric(out[[1]])
    if (length(p) != length(A))
      stop(paste0("muzero: prediction returned ", length(p),
                  " priors for ", length(A), " actions"))
    tot <- sum(p)
    if (tot <= 0) stop("muzero: prior must have positive mass")
    list(p = p / tot, v = as.numeric(out[[2]]))
  }
  s0 <- representation(observation)
  pr <- predict(s0)
  prior <- pr$p
  if (!is.null(dirichlet_alpha))
    prior <- .ghc_muzero_add_noise(prior, as.numeric(dirichlet_alpha),
                                    as.numeric(exploration_fraction), seed)
  root <- .ghc_muzero_node_expand(.ghc_muzero_node_new(0), s0, prior, A)
  mm <- list(lo = NULL, hi = NULL)
  for (s in seq_len(simulations)) {
    node <- root
    path <- list(node)
    acts <- list()
    while (isTRUE(node$expanded)) {
      a <- .ghc_muzero_select(node, A, mm, c1, c2, as.numeric(gamma))
      acts[[length(acts) + 1L]] <- a
      node <- node$children[[a]]
      path[[length(path) + 1L]] <- node
    }
    parent <- path[[length(path) - 1L]]
    calls[1] <- calls[1] + 1L
    rd <- dynamics(parent$state, acts[[length(acts)]])
    node$reward <- as.numeric(rd[[1]])
    prp <- predict(rd[[2]])
    node <- .ghc_muzero_node_expand(node, rd[[2]], prp$p, A)
    path[[length(path)]] <- node
    bu <- .ghc_muzero_backup(path, prp$v, as.numeric(gamma), mm)
    mm <- bu$mm
  }
  visits <- vapply(A, function(a) root$children[[a]]$visits, integer(1))
  total <- sum(visits)
  if (total <= 0L)
    stop("muzero: no simulations reached the root's children")
  if (temperature == 0) {
    best <- which.max(visits) - 1L
    policy <- vapply(seq_along(A), function(i) if (i - 1L == best) 1 else 0,
                     numeric(1))
  } else {
    w <- visits ^ (1 / as.numeric(temperature))
    policy <- w / sum(w)
  }
  Qa <- lapply(A, function(a) .ghc_muzero_edge_q(root$children[[a]], as.numeric(gamma)))
  root_value <- sum(visits * unlist(Qa)) / total
  list(estimate = policy, policy = policy,
       action = A[[which.max(policy)]],
       value = as.numeric(root_value),
       visits = setNames(as.list(visits), as.character(A)),
       Q = setNames(Qa, as.character(A)),
       prior = setNames(lapply(A, function(a)
         root$children[[a]]$prior), as.character(A)),
       n_dynamics_calls = calls[1], n_prediction_calls = calls[2],
       simulations = simulations,
       method = "MuZero MCTS (Schrittwieser et al. 2020, eqs. 2-5)")
}

morie_muzero_mcts_search <- morie_muzero

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_add_noise
#'
#' A step of the muzero_native implementation. Called by \code{muzero_search}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param prior Numeric; combined arithmetically in the body.
#' @param alpha Passed to \code{<=}.
#' @param frac Numeric; combined arithmetically in the body.
#' @param seed Passed to \code{set.seed}.
#' @return A numeric value.
#' @export
muzero_add_noise <- function(prior, alpha, frac, seed) {
  if (alpha <= 0) stop("muzero: dirichlet_alpha must be > 0")
  if (!(frac >= 0 && frac <= 1)) stop("muzero: exploration_fraction must lie in [0, 1]")
  .rmorie_local_seed(seed)
  g <- sapply(prior, function(p) muzero_gamma_rv(alpha))
  s <- sum(g)
  noise <- g / s
  (1 - frac) * prior + frac * noise
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_backup
#'
#' A step of the muzero_native implementation. Called by \code{muzero_search}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param path A vector; its length is taken and its elements indexed.
#' @param value See Usage.
#' @param gamma Numeric; combined arithmetically in the body.
#' @param mm A list; the body reads \code{$update} from it.
#' @return The value of \code{for}.
#' @export
muzero_backup <- function(path, value, gamma, mm) {
  g <- value
  for (i in rev(seq_along(path))) {
    node <- path[[i]]
    node$value_sum <- node$value_sum + g
    node$visits <- node$visits + 1
    mm$update(node$value())
    g <- node$reward + gamma * g
  }
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_cheatsheet
#'
#' A step of the muzero_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
muzero_cheatsheet <- function() {
  paste(paste0(
    "muzero: MCTS over a LEARNED latent model -- h (represent), g",
    " (dynamics -> reward, next latent), f (predict -> prior, val",
    "ue); no observation is ever reconstructed. pUCT eq. 2 with c",
    "1=1.25, c2=19652; backup eqs. 3-4 form the l-k step bootstra",
    "pped return G^k and fold it into a running mean; Q is min-ma",
    "x normalised over the whole tree (eq. 5) because values are ",
    "unbounded. Search policy = visit counts. One g and one f cal",
    "l per simulation."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_gamma_rv
#'
#' A step of the muzero_native implementation. Called by \code{muzero_add_noise}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Numeric; combined arithmetically in the body.
#' @return The value of \code{repeat}.
#' @export
muzero_gamma_rv <- function(alpha) {
  if (alpha < 1) {
    u <- runif(1)
    return(muzero_gamma_rv(alpha + 1) * (u ^ (1 / alpha)))
  }
  d <- alpha - 1 / 3
  cc <- 1 / sqrt(9 * d)
  repeat {
    x <- rnorm(1)
    v <- (1 + cc * x)^3
    if (v <= 0) next
    u <- runif(1)
    if (log(u) < 0.5 * x * x + d - d * v + d * log(v)) return(d * v)
  }
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_MinMax
#'
#' A step of the muzero_native implementation. Called by \code{muzero_search}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return The value of \code{env}, as built in the body.
#' @export
muzero_MinMax <- function() {
  env <- new.env(parent = emptyenv())
  env$lo <- NULL
  env$hi <- NULL
  env$update <- function(v) {
    env$lo <- if (is.null(env$lo)) v else min(env$lo, v)
    env$hi <- if (is.null(env$hi)) v else max(env$hi, v)
  }
  env$normalize <- function(v) {
    if (is.null(env$lo) || is.null(env$hi)) return(v)
    if (env$hi > env$lo) (v - env$lo) / (env$hi - env$lo) else v
  }
  env
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_Node
#'
#' A step of the muzero_native implementation. Called by \code{muzero_search}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param prior A vector; indexed elementwise. Defaults to \code{0}.
#' @return The value of \code{env}, as built in the body.
#' @export
muzero_Node <- function(prior = 0) {
  env <- new.env(parent = emptyenv())
  env$visits <- 0
  env$value_sum <- 0
  env$prior <- prior
  env$children <- list()
  env$state <- NULL
  env$reward <- 0
  env$expanded <- FALSE
  env$value <- function() {
    if (env$visits > 0) env$value_sum / env$visits else 0
  }
  env$expand <- function(state, prior, actions) {
    env$state <- state
    env$expanded <- TRUE
    for (i in seq_along(actions)) {
      env$children[[as.character(actions[[i]])]] <- muzero_Node(prior[[i]])
    }
  }
  env
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_search
#'
#' A step of the muzero_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param observation Passed to \code{representation}.
#' @param actions Coerced to list by the body, with \code{as.list}.
#' @param representation Passed to \code{c}.
#' @param dynamics Passed to \code{c}.
#' @param prediction Passed to \code{c}.
#' @param simulations A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50}.
#' @param gamma Passed to \code{muzero_backup}. Defaults to \code{0.997}.
#' @param c1 Passed to \code{muzero_select}. Defaults to \code{1.25}.
#' @param c2 The body requires: muzero: c2 must be > 0. Defaults to \code{19652}.
#' @param dirichlet_alpha Optional; may be \code{NULL}. Coerced to numeric by the body,
#' with \code{as.numeric}.
#' @param exploration_fraction Coerced to numeric by the body, with \code{as.numeric}.
#' Defaults to \code{0.25}.
#' @param temperature Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param seed Passed to \code{muzero_add_noise}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{policy}, \code{action}, \code{value},
#' \code{visits}, \code{Q}, \code{prior}, \code{n_dynamics_calls},
#' \code{n_prediction_calls}, \code{simulations}, \code{method}.
#' @export
muzero_search <- function(observation, actions, representation, dynamics,
                          prediction, simulations = 50, gamma = 0.997,
                          c1 = 1.25, c2 = 19652, dirichlet_alpha = NULL,
                          exploration_fraction = 0.25, temperature = 1,
                          seed = 0) {
  A <- as.list(actions)
  if (length(A) == 0L) stop("muzero: actions must be non-empty")
  for (pair in list(c(representation, "representation"),
                    c(dynamics, "dynamics"),
                    c(prediction, "prediction"))) {
    if (!is.function(pair[[1]])) stop(sprintf("muzero: %s must be callable", pair[[2]]))
  }
  simulations <- as.integer(simulations)
  if (simulations < 1L) stop("muzero: simulations must be >= 1")
  if (c2 <= 0) stop("muzero: c2 must be > 0")
  A_keys <- sapply(A, as.character)
  calls <- c(0L, 0L)
  predict_fn <- function(s) {
    calls[[2]] <<- calls[[2]] + 1L
    out <- prediction(s)
    p <- as.numeric(out[[1]])
    v <- as.numeric(out[[2]])
    if (length(p) != length(A)) {
      stop(sprintf("muzero: prediction returned %d priors for %d actions",
                   length(p), length(A)))
    }
    tot <- sum(p)
    if (tot <= 0) stop("muzero: prior must have positive mass")
    list(p = p / tot, v = v)
  }
  root <- muzero_Node()
  s0 <- representation(observation)
  pr <- predict_fn(s0)
  prior <- pr$p
  if (!is.null(dirichlet_alpha)) {
    prior <- muzero_add_noise(prior, as.numeric(dirichlet_alpha),
                              as.numeric(exploration_fraction), seed)
  }
  root$expand(s0, prior, A)
  mm <- muzero_MinMax()
  for (sim in seq_len(simulations)) {
    node <- root
    path <- list(node)
    acts <- c()
    repeat {
      if (!node$expanded) break
      k <- muzero_select(node, A_keys, mm, c1, c2)
      a <- A[[which(A_keys == k)]]
      acts <- c(acts, a)
      node <- node$children[[k]]
      path[[length(path) + 1L]] <- node
    }
    parent <- path[[length(path) - 1L]]
    calls[[1]] <- calls[[1]] + 1L
    out <- dynamics(parent$state, acts[[length(acts)]])
    r <- as.numeric(out[[1]])
    s <- out[[2]]
    node$reward <- r
    pr2 <- predict_fn(s)
    node$expand(s, pr2$p, A)
    muzero_backup(path, pr2$v, gamma, mm)
  }
  visits <- sapply(A_keys, function(k) root$children[[k]]$visits)
  total <- sum(visits)
  if (total <= 0) stop("muzero: no simulations reached the root's children")
  if (temperature == 0) {
    best <- which.max(visits)
    policy <- ifelse(seq_along(A) == best, 1, 0)
  } else {
    w <- visits ^ (1 / as.numeric(temperature))
    policy <- w / sum(w)
  }
  root_value <- 0
  for (i in seq_along(A)) {
    k <- A_keys[[i]]
    ch <- root$children[[k]]
    root_value <- root_value + ch$visits * ch$value()
  }
  root_value <- root_value / total
  list(estimate = policy, policy = policy,
       action = A[[which.max(policy)]],
       value = as.numeric(root_value),
       visits = setNames(as.list(visits), A_keys),
       Q = setNames(lapply(A_keys, function(k) root$children[[k]]$value()), A_keys),
       prior = setNames(lapply(A_keys, function(k) root$children[[k]]$prior), A_keys),
       n_dynamics_calls = calls[[1]], n_prediction_calls = calls[[2]],
       simulations = simulations,
       method = "MuZero MCTS (Schrittwieser et al. 2020, eqs. 2-5)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' muzero_select
#'
#' A step of the muzero_native implementation. Called by \code{muzero_search}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$children} from it.
#' @param A_keys A vector; indexed elementwise.
#' @param mm A list; the body reads \code{$normalize} from it.
#' @param c1 Numeric; combined arithmetically in the body.
#' @param c2 Numeric; combined arithmetically in the body.
#' @return The value of \code{best_a}, as built in the body.
#' @export
muzero_select <- function(node, A_keys, mm, c1, c2) {
  total <- 0
  for (k in A_keys) total <- total + node$children[[k]]$visits
  sqrt_total <- if (total > 0) sqrt(total) else 0
  best <- -Inf
  best_a <- A_keys[[1]]
  for (k in A_keys) {
    ch <- node$children[[k]]
    explore <- ch$prior * sqrt_total / (1 + ch$visits) *
      (c1 + log((total + c2 + 1) / c2))
    q <- if (ch$visits > 0) mm$normalize(ch$value()) else 0
    score <- q + explore
    if (score > best) { best <- score
    best_a <- k }
  }
  best_a
}
