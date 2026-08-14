# smcsam_native.R -- mirror of smcsam_python_reference.py

# Sequential Monte Carlo samplers.
# Sources: Del Moral, P., Doucet, A. & Jasra, A. (2006) "Sequential
# Monte Carlo samplers", *Journal of the Royal Statistical Society:
# Series B* 68(3), 411-436, doi:10.1111/j.1467-9868.2006.00553.x.
# Equations 10-12, 30-31, the ESS criterion and section 3.1.1.
# Metropolis, N., Rosenbluth, A. W., Rosenbluth, M. N., Teller, A. H.
# & Teller, E. (1953) "Equation of state calculations by fast
# computing machines", *Journal of Chemical Physics* 21(6),
# 1087-1092, doi:10.1063/1.1699114 -- the accept-reject rule.
#
# Native implementation mirroring Python morie.fn.smcsam exactly:
# the same incremental weights (equation 31 with the MCMC rule,
# equation 12 with the general rule), the same log-normalising-
# constant running product, the same ESS = 1/sum W^2, the same
# multinomial / stratified / systematic / residual resampling
# schemes, the same temperature ladder builders, and the same
# random-walk kernel.

ess <- function(weights) {
  tot <- sum(weights)
  if (tot <= 0)
    stop("smcsam: weights must have positive total mass")
  w <- weights / tot
  1 / sum(w * w)
}

# Binary search over a precomputed cumulative distribution.  R's
# findInterval has the same effect but is documented to return the
# leftmost interval, so we keep our own to mirror the Python
# arm's hand-written loop.
.smcsam_pick <- function(cum, u) {
  lo <- 1L; hi <- length(cum)
  while (lo < hi) {
    mid <- (lo + hi) %/% 2L
    if (cum[mid] < u) lo <- mid + 1L else hi <- mid
  }
  lo
}

resample <- function(weights, rng, scheme = "systematic") {
  tot <- sum(weights)
  if (tot <= 0)
    stop("smcsam: weights must have positive total mass")
  w <- weights / tot
  n <- length(w)
  run <- 0
  cum <- numeric(n)
  for (i in seq_len(n)) {
    run <- run + w[i]
    cum[i] <- run
  }
  if (scheme == "multinomial") {
    u <- .ghc_unif(rng, n)
    return(vapply(u, function(ui) .smcsam_pick(cum, ui),
                  integer(1L)))
  }
  if (scheme == "stratified") {
    u0 <- .ghc_unif(rng, n)
    return(vapply(seq_len(n),
                  function(k) .smcsam_pick(cum, (k - 1L + u0[k]) / n),
                  integer(1L)))
  }
  if (scheme == "systematic") {
    u0 <- .ghc_unif(rng, 1L)
    return(vapply(seq_len(n),
                  function(k) .smcsam_pick(cum, (k - 1L + u0) / n),
                  integer(1L)))
  }
  if (scheme == "residual") {
    counts <- as.integer(n * w)
    out <- rep(seq_len(n), counts)
    rem <- n - length(out)
    if (rem > 0L) {
      resid <- n * w - counts
      rs <- resid / sum(resid)
      rrun <- 0
      rcum <- numeric(n)
      for (i in seq_len(n)) {
        rrun <- rrun + rs[i]
        rcum[i] <- rrun
      }
      u <- .ghc_unif(rng, rem)
      add <- vapply(u, function(ui) .smcsam_pick(rcum, ui),
                    integer(1L))
      out <- c(out, add)
    }
    return(out)
  }
  stop("smcsam: scheme must be multinomial, stratified, systematic ",
       "or residual, got ", deparse(scheme))
}

temperature_ladder <- function(n_steps, kind = "geometric", power = 1.0) {
  n <- as.integer(n_steps)
  if (n < 2L)
    stop("smcsam: need at least two steps, got ", n)
  if (kind == "geometric")
    return((seq_len(n) - 1L) / (n - 1L))
  if (kind == "power") {
    if (as.numeric(power) <= 0)
      stop("smcsam: power must be positive")
    return(((seq_len(n) - 1L) / (n - 1L)) ^ as.numeric(power))
  }
  if (kind == "prior")
    return(c(0, rep(1, n - 1L)))
  stop("smcsam: kind must be geometric, power or prior, got ",
       deparse(kind))
}

# A Metropolis random walk that, just like the Python arm's
# random_walk_kernel(scale, n_moves), performs n_moves of
# x' = x + Normal(0, scale^2) and returns the average accept
# rate over the inner moves.
.smcsam_rwk <- function(scale, n_moves) {
  force(scale); force(n_moves)
  function(x, log_target, rng) {
    cur <- as.numeric(x)
    lp  <- log_target(cur)
    acc <- 0L
    for (m in seq_len(as.integer(n_moves))) {
      prop <- cur + as.numeric(scale) * .ghc_norm(rng, length(cur))
      lq <- log_target(prop)
      if (is.finite(lq) && log(runif(1, 0, 1)) < (lq - lp)) {
        cur <- prop; lp <- lq; acc <- acc + 1L
      }
    }
    list(x = cur, accept = acc / as.numeric(n_moves))
  }
}

random_walk_kernel <- function(scale = 1.0, n_moves = 1L) {
  .smcsam_rwk(scale, n_moves)
}

smcsam <- function(log_gamma, initial, n_particles = 500L, ladder = NULL,
                   n_steps = 20L, kernel = NULL, ess_threshold = 0.5,
                   scheme = "systematic", seed = 0L, weight_rule = "mcmc",
                   log_forward = NULL, log_backward = NULL) {
  if (!(weight_rule %in% c("mcmc", "general")))
    stop("smcsam: weight_rule must be 'mcmc' or 'general'")
  if (weight_rule == "general" &&
      (is.null(log_forward) || is.null(log_backward)))
    stop("smcsam: weight_rule='general' needs log_forward and ",
         "log_backward densities (equation 12)")
  phis <- if (!is.null(ladder)) as.numeric(ladder)
          else temperature_ladder(n_steps)
  if (length(phis) < 2L)
    stop("smcsam: the ladder needs at least two steps")
  N <- as.integer(n_particles)
  if (N < 2L)
    stop("smcsam: need at least two particles, got ", N)
  th <- as.numeric(ess_threshold)
  if (!is.finite(th) || !(th > 0) || !(th <= 1))
    stop("smcsam: ess_threshold must lie in (0, 1], got ", format(th))
  rng <- .ghc_rng(as.numeric(seed))
  move <- if (!is.null(kernel)) kernel else random_walk_kernel()

  # Initial particle draw; each particle is a numeric vector.
  X <- vector("list", N)
  for (i in seq_len(N)) X[[i]] <- as.numeric(initial(rng))
  logW <- rep(0, N)
  log_norm <- 0
  ess_trace <- numeric(0)
  resampled <- integer(0)
  accept_trace <- numeric(0)

  for (n in seq(2L, length(phis))) {
    prev <- phis[n - 1L]; cur <- phis[n]
    # incremental weights BEFORE the move (equation 31, MCMC rule)
    if (weight_rule == "mcmc") {
      inc <- vapply(seq_len(N),
                    function(i) log_gamma(X[[i]], cur) -
                                 log_gamma(X[[i]], prev),
                    numeric(1L))
      mx <- max(inc)
      # running product of incremental weights = Z_n / Z_{n-1}
      # using the log-sum-exp trick in the same form as the
      # Python arm's mx + log(sum exp(...))
      wprev <- exp(logW - max(logW))
      tot_prev <- sum(wprev)
      log_norm <- log_norm + mx +
        log(sum(wprev * exp(inc - mx)) / tot_prev)
      logW <- logW + inc
    }

    target <- function(x) log_gamma(x, cur)

    moved <- vector("list", N)
    acc <- 0
    for (i in seq_len(N)) {
      out <- move(X[[i]], target, rng)
      xn <- if (is.list(out)) out$x else out[[1]]
      a  <- if (is.list(out)) out$accept else out[[2]]
      if (weight_rule == "general") {
        num <- log_gamma(xn, cur)   + log_backward(xn, X[[i]], cur)
        den <- log_gamma(X[[i]], prev) + log_forward(X[[i]], xn, cur)
        logW[i] <- logW[i] + num - den
      }
      moved[[i]] <- as.numeric(xn)
      acc <- acc + as.numeric(a)
    }
    X <- moved
    accept_trace <- c(accept_trace, acc / N)

    mx <- max(logW)
    w <- exp(logW - mx)
    e <- ess(w)
    ess_trace <- c(ess_trace, e)
    if (e < th * N) {
      idx <- resample(w, rng, scheme)
      X <- X[idx]
      logW <- rep(0, N)
      resampled <- c(resampled, n)
    }
  }

  mx <- max(logW)
  w <- exp(logW - mx)
  tot <- sum(w)
  W <- w / tot
  dim <- length(X[[1L]])
  mean <- numeric(dim)
  for (k in seq_len(dim))
    mean[k] <- sum(W * vapply(X, function(x) x[k], numeric(1L)))
  var <- numeric(dim)
  for (k in seq_len(dim))
    var[k] <- sum(W * vapply(X, function(x) (x[k] - mean[k]) ^ 2,
                             numeric(1L)))
  list(estimate = mean, mean = mean, variance = var,
       particles = X, weights = W,
       log_norm_const = log_norm,
       ess = ess(w), ess_trace = ess_trace,
       resampled = resampled, accept_trace = accept_trace,
       ladder = phis, n_particles = N,
       weight_rule = weight_rule,
       method = "SMC sampler (Del Moral, Doucet & Jasra 2006)")
}

.smcsam_cheatsheet <- function() {
  paste("smcsam: SMC samplers (Del Moral, Doucet & Jasra 2006). A ",
        "sequence pi_n on a FIXED space is made sequential by an ",
        "artificial joint target built from BACKWARD kernels L_k, so ",
        "the weights update recursively (eqs.11-12). With an MCMC ",
        "kernel of invariant distribution pi_n, the natural L is its ",
        "reversal (eq.30) and the incremental weight collapses to ",
        "gamma_n(x_{n-1})/gamma_{n-1}(x_{n-1}) (eq.31) -- evaluated ",
        "BEFORE the move, and free of the kernel. Degeneracy watched ",
        "by ESS = 1/sum W^2, resample below N/2. The running product ",
        "of incremental weights estimates Z_n/Z_1.", sep = "")
}

smc_sampler <- smcsam
sequential_mc_sampler <- smcsam

# Native entry point.
morie_smcsam <- smcsam
