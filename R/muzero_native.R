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
  list(visits = 0L, value_sum = 0, prior = prior, children = list(),
       state = NULL, reward = 0, expanded = FALSE)
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
  for (i in seq_along(A)) node$children[[A[i]]] <- .ghc_muzero_node_new(prior[i])
  node
}

#' @keywords internal
#' @noRd
.ghc_muzero_select <- function(node, A, mm, c1, c2) {
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
                                                     .ghc_muzero_node_value(ch))
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
    mm <- .ghc_muzero_minmax_update(mm, .ghc_muzero_node_value(node))
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
      a <- .ghc_muzero_select(node, A, mm, c1, c2)
      acts[[length(acts) + 1L]] <- a
      node <- node$children[[a]]
      path[[length(path) + 1L]] <- node
    }
    parent <- path[[length(path) - 1L]]
    calls[1] <<- calls[1] + 1L
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
  root_value <- sum(vapply(A, function(a)
    root$children[[a]]$visits * .ghc_muzero_node_value(root$children[[a]]),
    numeric(1))) / total
  list(estimate = policy, policy = policy,
       action = A[[which.max(policy)]],
       value = as.numeric(root_value),
       visits = setNames(as.list(visits), as.character(A)),
       Q = setNames(lapply(A, function(a)
         .ghc_muzero_node_value(root$children[[a]])), as.character(A)),
       prior = setNames(lapply(A, function(a)
         root$children[[a]]$prior), as.character(A)),
       n_dynamics_calls = calls[1], n_prediction_calls = calls[2],
       simulations = simulations,
       method = "MuZero MCTS (Schrittwieser et al. 2020, eqs. 2-5)")
}

morie_muzero_mcts_search <- morie_muzero
