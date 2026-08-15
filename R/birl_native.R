# birl -- Bayesian inverse reinforcement learning.
# Ramachandran & Amir (2007) "Bayesian Inverse Reinforcement Learning",
# IJCAI-07.
# Base R only.

PRIORS <- c("uniform", "gaussian", "laplacian", "ising")

.mdp <- function(T, gamma) {
  if (length(T) == 0) stop("birl: the transition model is empty")
  nS <- length(T)
  nA <- length(T[[1]])
  if (nA == 0) stop("birl: there are no actions")
  for (s in 1:nS) {
    if (length(T[[s]]) != nA)
      stop("birl: every state needs the same actions")
    for (a in 1:nA) {
      row <- T[[s]][[a]]
      if (length(row) != nS)
        stop("birl: a transition row has the wrong length")
      tot <- sum(row)
      if (abs(tot - 1) > 1e-8 || min(row) < 0)
        stop("birl: transition rows must be probability distributions")
    }
  }
  if (gamma < 0 || gamma >= 1) stop("birl: gamma must be in [0, 1)")
  list(nS = nS, nA = nA)
}

.solve <- function(A, b) {
  n <- nrow(A)
  M <- cbind(A, b)
  for (c in 1:n) {
    piv <- c
    best <- abs(M[c, c])
    for (r in (c + 1):n) {
      v <- abs(M[r, c])
      if (v > best) { best <- v; piv <- r }
    }
    if (best < 1e-14) stop("birl: the value system is singular")
    if (piv != c) { tmp <- M[c, ]; M[c, ] <- M[piv, ]; M[piv, ] <- tmp }
    for (r in 1:n) {
      if (r == c) next
      f <- M[r, c] / M[c, c]
      if (f == 0) next
      M[r, c:(n + 1)] <- M[r, c:(n + 1)] - f * M[c, c:(n + 1)]
    }
  }
  M[, n + 1] / diag(M)
}

policy_values <- function(T, R, gamma, policy) {
  m <- .mdp(T, gamma)
  nS <- m$nS
  if (length(policy) != nS || length(R) != nS)
    stop("birl: policy and reward need one entry per state")
  A <- matrix(0, nS, nS)
  for (i in 1:nS) for (j in 1:nS)
    A[i, j] <- (if (i == j) 1 else 0) - gamma * T[[i]][[policy[i]]][j]
  .solve(A, as.numeric(R))
}

q_values <- function(T, R, gamma, V) {
  m <- .mdp(T, gamma); nS <- m$nS; nA <- m$nA
  Q <- matrix(0, nS, nA)
  for (s in 1:nS) for (a in 1:nA) {
    Q[s, a] <- R[s] + gamma * sum(T[[s]][[a]] * V)
  }
  Q
}

policy_iteration <- function(T, R, gamma, policy = NULL, max_iter = 200) {
  m <- .mdp(T, gamma); nS <- m$nS; nA <- m$nA
  if (length(R) != nS) stop("birl: one reward per state is required")
  pi <- if (is.null(policy)) rep(1L, nS) else as.integer(policy)
  if (length(pi) != nS) stop("birl: the starting policy has the wrong length")
  sweeps <- 0
  for (it in seq_len(max_iter)) {
    V <- policy_values(T, R, gamma, pi)
    Q <- q_values(T, R, gamma, V)
    new <- apply(Q, 1, which.max)
    sweeps <- sweeps + 1
    if (identical(new, pi)) {
      return(list(policy = pi, V = V, Q = Q, sweeps = sweeps))
    }
    pi <- new
  }
  V <- policy_values(T, R, gamma, pi)
  list(policy = pi, V = V, Q = q_values(T, R, gamma, V), sweeps = sweeps)
}

log_likelihood <- function(Q, observations, alpha = 1) {
  if (alpha <= 0) stop("birl: alpha must be positive")
  if (length(observations) == 0) stop("birl: no observations")
  total <- 0
  for (sa in observations) {
    s <- as.integer(sa[1]); a <- as.integer(sa[2])
    if (s < 1 || s > nrow(Q) || a < 1 || a > ncol(Q))
      stop("birl: an observation is out of range")
    row <- alpha * Q[s, ]
    m <- max(row)
    total <- total + (row[a] - (m + log(sum(exp(row - m)))))
  }
  total
}

log_prior <- function(R, prior = "uniform", scale = 1, r_max = NULL,
                      J = 0.1, H = 0, neighbours = NULL) {
  if (!(prior %in% PRIORS))
    stop(sprintf("birl: prior must be one of %s", paste(PRIORS, collapse = ", ")))
  if (scale <= 0) stop("birl: scale must be positive")
  if (prior == "uniform") {
    if (!is.null(r_max) && any(abs(R) > r_max + 1e-12)) return(-Inf)
    return(0)
  }
  if (prior == "gaussian") return(-sum(R^2) / (2 * scale^2))
  if (prior == "laplacian") return(-sum(abs(R)) / scale)
  pairs <- if (is.null(neighbours))
             do.call(rbind, lapply(1:(length(R) - 1), function(i) c(i, i + 1)))
           else neighbours
  -(J * sum(R[pairs[, 1]] * R[pairs[, 2]]) + H * sum(R))
}

.rng <- function(seed) {
  st <- as.integer(seed); if (st <= 0) st <- 1L
  f <- function() {
    st <<- .ghc_lcg31(st)
    st / 2147483648
  }
  f
}

policy_walk <- function(T, observations, gamma, n_iter = 1000, delta = 0.25,
                        alpha = 1, prior = "uniform", scale = 1, r_max = 1,
                        J = 0.1, H = 0, burn = NULL, seed = 0, R0 = NULL) {
  m <- .mdp(T, gamma); nS <- m$nS; nA <- m$nA
  if (delta <= 0) stop("birl: delta must be positive")
  if (n_iter < 1) stop("birl: n_iter must be positive")
  burn <- if (is.null(burn)) n_iter %/% 2 else as.integer(burn)
  if (burn < 0 || burn >= n_iter) stop("birl: burn must be less than n_iter")
  rnd <- .rng(seed + 3L)
  grid <- function(v) round(v / delta) * delta
  R <- if (is.null(R0)) vapply(1:nS, function(i)
    grid((2 * rnd() - 1) * r_max), numeric(1)) else vapply(R0, grid, numeric(1))
  got <- policy_iteration(T, R, gamma)
  pi <- got$policy; Q <- got$Q
  score <- function(Qm, Rv) {
    lp <- log_prior(Rv, prior, scale, r_max, J, H, NULL)
    if (is.infinite(lp) && lp < 0) return(lp)
    log_likelihood(Qm, observations, alpha) + lp
  }
  cur <- score(Q, R)
  samples <- matrix(0, n_iter, nS)
  accepted <- 0L; repolicy <- 0L
  for (it in seq_len(n_iter)) {
    s <- as.integer(rnd() * nS) + 1L
    if (s > nS) s <- nS
    step <- if (rnd() < 0.5) delta else -delta
    cand <- R
    cand[s] <- grid(cand[s] + step)
    if (!is.null(r_max) && abs(cand[s]) > r_max + 1e-12) {
      samples[it, ] <- R
      next
    }
    Vp <- policy_values(T, cand, gamma, pi)
    Qp <- q_values(T, cand, gamma, Vp)
    changed <- FALSE
    for (st_ in 1:nS) {
      if (Qp[st_, pi[st_]] < max(Qp[st_, ]) - 1e-12) {
        changed <- TRUE; break
      }
    }
    if (changed) {
      repolicy <- repolicy + 1L
      got2 <- policy_iteration(T, cand, gamma, pi)
      newpi <- got2$policy; newQ <- got2$Q
    } else {
      newpi <- pi; newQ <- Qp
    }
    prop <- score(newQ, cand)
    if (prop > cur || log(max(rnd(), 1e-300)) < prop - cur) {
      R <- cand; pi <- newpi; Q <- newQ; cur <- prop
      accepted <- accepted + 1L
    }
    samples[it, ] <- R
  }
  if (burn > 0) samples <- samples[(burn + 1):n_iter, , drop = FALSE]
  if (nrow(samples) == 0) samples <- matrix(R, 1, nS)
  list(samples = samples, acceptance = accepted / n_iter,
       policy_iterations = repolicy, n_proposals = n_iter,
       final_policy = pi)
}

birl <- function(T, observations, gamma = 0.9, n_iter = 1000, delta = 0.25,
                 alpha = 1, prior = "uniform", scale = 1, r_max = 1,
                 J = 0.1, H = 0, burn = NULL, seed = 0, R0 = NULL) {
  .mdp(T, gamma)
  obs <- lapply(observations, function(sa) c(as.integer(sa[1]),
                                              as.integer(sa[2])))
  walk <- policy_walk(T, obs, gamma, n_iter, delta, alpha, prior, scale,
                      r_max, J, H, burn, seed, R0)
  S <- walk$samples
  n <- nrow(S)
  mean_r <- colMeans(S)
  var_r <- apply(S, 2, function(c) sum((c - mean(c))^2) / max(n - 1, 1))
  got <- policy_iteration(T, mean_r, gamma)
  list(estimate = mean_r, reward_mean = mean_r,
       reward_sd = sqrt(var_r), policy = got$policy, V = got$V, Q = got$Q,
       samples = S, acceptance = walk$acceptance,
       policy_iterations = walk$policy_iterations,
       n_proposals = walk$n_proposals, n_samples = n, prior = prior,
       alpha = as.numeric(alpha), delta = as.numeric(delta),
       method = paste("Bayesian IRL (Ramachandran & Amir 2007):",
                      "Boltzmann expert likelihood, PolicyWalk over the",
                      "reward grid, posterior mean reward per Theorem 3"),
       note = paste("Theorem 3 says the reported policy is the optimal",
                    "one for the posterior MEAN reward, not the mode and",
                    "not any single sample; policy_iterations counts how",
                    "often step 3(c) actually had to recompute the policy"))
}

bayesian_irl <- birl
bayesianirl <- birl

# house entry point: the package exports one morie_<module>
morie_birl <- birl
