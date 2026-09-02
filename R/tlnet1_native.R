# morie.fn -- function file (rootcoder007/morie)
#' Single time point interventions in network-dependent data
#' 
#' \eqn{N} units connected by a social network. For each we record
#' baseline covariates \eqn{W_i}, exposure \eqn{A_i} and outcome
#' \eqn{Y_i}, and we observe \eqn{F_i} -- the units that could
#' influence \eqn{i}, "i's friends". The number of friends varies with
#' \eqn{i} and is assumed to vanish relative to \eqn{N}.
#' 
#' **Two dependencies are allowed, and naming them is the modelling
#' step.** A unit's *exposure* may depend on its own baseline covariates
#' and on those of its friends; a unit's *outcome* may depend on its own
#' baseline and exposure and on those of its friends. Everything else is
#' excluded by assumption: **all** dependence between units is fully
#' described by the known network. That assumption is what makes the
#' problem tractable, and it is also the one most likely to be wrong --
#' an unobserved edge is indistinguishable from unmeasured confounding
#' between the two units it should have joined.
#' 
#' **Interference means the estimand must be a policy, not a value.**
#' Under interference \eqn{Y_i} depends on the treatments of others, so
#' "the effect of treatment" is not defined until the whole assignment is
#' specified. The estimand is the mean outcome under a stochastic policy
#' applied network-wide, and useful contrasts fall out of it: the
#' **direct** effect fixes the neighbourhood exposure and varies the
#' unit's own; the **spillover** effect fixes the unit's own and varies
#' the neighbourhood's. ``decompose_effects`` computes both, since
#' reporting only the total hides which mechanism produced it.
#' 
#' **Inference is in \eqn{N} with dependence.** The influence curve
#' terms are correlated exactly along network edges, so the variance adds
#' those covariances; with \eqn{\max_i|F_i|/N \to 0} the sum is still
#' \eqn{O(N)} and a central limit theorem applies.
#' 
#' References
#' van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
#' Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 21 (Sofrygin,
#' Ogburn & van der Laan): N units connected by a social network with
#' baseline covariates, exposure and outcome recorded for each, and the
#' observed set F_i of units connected to and able to influence i; the
#' number of friends varying in i and assumed to vanish when scaled by
#' 1/N; the two permitted between-unit dependencies -- exposure depending
#' on own and friends' baseline covariates, outcome depending on own and
#' friends' baseline and exposure -- and the modelling assumption that
#' ANY dependence among units is fully described by the known network,
#' with i's exposure and outcome depending on others only through i's
#' friends.
#' 
#' Sofrygin, O. & van der Laan, M. J. (2017) "Semi-Parametric Estimation
#' and Inference for the Mean Outcome of the Single Time-Point
#' Intervention in a Causally Connected Population", *Journal of Causal
#' Inference* 5(1), 20160003, doi:10.1515/jci-2016-0003.
#' 
#' Hudgens, M. G. & Halloran, M. E. (2008) "Toward Causal Inference With
#' Interference", *Journal of the American Statistical Association*
#' 103(482), 832-842, doi:10.1198/016214508000000292. Direct and
#' spillover effects under interference.
#' @noRd

.tlnet1_EPS <- 1e-12

# Private helper: coerce to a flat numeric vector (equivalent of k.vec)
#' Private helper: coerce to a flat numeric vector (equivalent of k.vec)
#'
#' A step of the tlnet1_native implementation. Called by \code{friend_summary}, \code{network_influence_variance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param values A matrix; the body checks with \code{is.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
.tlnet1_vec <- function(values) {
  if (is.matrix(values)) {
    as.numeric(values)
  } else if (is.list(values)) {
    unlist(lapply(values, as.numeric))
  } else {
    as.numeric(values)
  }
}

# Private helper: coerce to a list of numeric row-vectors (equivalent of k.mat)
#' Private helper: coerce to a list of numeric row-vectors (equivalent
#' of k.mat)
#'
#' A step of the tlnet1_native implementation. Called by \code{decompose_effects}, \code{policy_mean}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A matrix; indexed by row and column.
#' @return One of two values, depending on the branch taken.
#' @export
.tlnet1_mat <- function(W) {
  if (is.matrix(W) || is.data.frame(W)) {
    n <- nrow(W)
    lapply(seq_len(n), function(i) as.numeric(W[i, , drop = FALSE]))
  } else if (is.list(W)) {
    lapply(W, function(r) as.numeric(r))
  } else {
    list(as.numeric(W))
  }
}

# Private helper: friend set for unit i (1-based, excluding self)
#' Private helper: friend set for unit i (1-based, excluding self)
#'
#' A step of the tlnet1_native implementation. Called by \code{.tlnet1_count_edges}, \code{check_network_assumption}, \code{friend_summary} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param friends A vector; indexed elementwise.
#' @param i Passed to \code{setdiff}.
#' @return The value of \code{setdiff}.
#' @export
.tlnet1_fset <- function(friends, i) {
  setdiff(friends[[i]], i)
}

# Private helper: count total directed edges (i -> j for j in F_i)
#' Private helper: count total directed edges (i -> j for j in F_i)
#'
#' A step of the tlnet1_native implementation. Called by \code{network_influence_variance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param friends A vector; its length is taken.
#' @return The value of \code{total}, as built in the body.
#' @export
.tlnet1_count_edges <- function(friends) {
  N <- length(friends)
  total <- 0L
  for (i in seq_len(N)) {
    total <- total + length(.tlnet1_fset(friends, i))
  }
  total
}

#' Summarise a quantity over \eqn{F_i}
#' @param values See Usage.
#' @param friends See Usage.
#' @param kind See Usage.
friend_summary <- function(values, friends, kind = "fraction") {
  v <- .tlnet1_vec(values)
  N <- length(v)
  if (length(friends) != N) {
    stop(sprintf("tlnet1: %d values but %d friend sets",
                 N, length(friends)))
  }
  out <- vector("list", N)
  for (i in seq_len(N)) {
    f <- sort(.tlnet1_fset(friends, i))
    if (length(f) == 0L) {
      out[[i]] <- 0.0
    } else if (kind == "fraction") {
      out[[i]] <- sum(v[f]) / length(f)
    } else if (kind == "count") {
      out[[i]] <- as.numeric(sum(v[f]))
    } else {
      stop(sprintf("tlnet1: kind must be fraction or count, got %s",
                   kind))
    }
  }
  out
}

#' The conditions the identification rests on
#' 
#' Degrees must vanish relative to \eqn{N}, and the network must be
#' symmetric -- an asymmetric "friend" relation means influence
#' flows somewhere the model does not represent.
#' @param friends See Usage.
#' @param N See Usage.
check_network_assumption <- function(friends, N = NULL) {
  n <- if (is.null(N)) length(friends) else as.integer(N)
  deg <- sapply(seq_along(friends),
                function(i) length(.tlnet1_fset(friends, i)))
  asym <- list()
  for (i in seq_along(friends)) {
    fi <- .tlnet1_fset(friends, i)
    for (j in fi) {
      if (!(i %in% friends[[j]])) {
        asym[[length(asym) + 1L]] <- c(i, j)
      }
    }
  }
  max_deg <- if (length(deg) > 0L) max(deg) else 0L
  max_share <- if (length(deg) > 0L) as.numeric(max_deg) / as.numeric(n) else 0.0
  sparse <- if (length(deg) > 0L) max_share < 0.25 else TRUE
  list(
    max_degree = max_deg,
    max_share = max_share,
    sparse = sparse,
    asymmetric_edges = asym,
    symmetric = length(asym) == 0L,
    note = "all dependence is assumed described by the KNOWN network; an unobserved edge is indistinguishable from unmeasured confounding"
  )
}

#' Mean outcome under a stochastic network-wide policy
#' 
#' Treatments are drawn independently with probability
#' ``own_prob``, then the neighbourhood summary follows -- so the
#' estimand is a property of the POLICY, which is the only thing
#' well defined under interference.
#' @param Q_fn See Usage.
#' @param W See Usage.
#' @param friends See Usage.
#' @param own_prob See Usage.
#' @param seed See Usage.
#' @param draws See Usage.
policy_mean <- function(Q_fn, W, friends, own_prob, seed = 0, draws = 200) {
  rows <- .tlnet1_mat(W)
  N <- length(rows)
  p <- as.numeric(own_prob)
  if (p < 0.0 || p > 1.0) {
    stop(sprintf("tlnet1: the policy probability must lie in [0,1], got %s",
                 own_prob))
  }
  e <- .ghc_rng(seed)
  nd <- as.integer(draws)
  tot <- 0.0
  for (d in seq_len(nd)) {
    u <- .ghc_unif(e, N)
    a <- ifelse(u < p, 1.0, 0.0)
    fs <- friend_summary(a, friends)
    s <- 0.0
    for (i in seq_len(N)) {
      s <- s + as.numeric(Q_fn(a[i], fs[[i]], rows[[i]]))
    }
    tot <- tot + s / N
  }
  list(psi = tot / nd, policy_prob = p, draws = nd, N = N)
}

#' Direct and spillover effects, separately
#' 
#' Direct: own exposure varies with the neighbourhood held at
#' ``p_low``. Spillover: own held at ``p_low`` while the
#' neighbourhood varies. Reporting only the total hides which
#' mechanism produced it.
#' @param Q_fn See Usage.
#' @param W See Usage.
#' @param friends See Usage.
#' @param p_high See Usage.
#' @param p_low See Usage.
#' @param seed See Usage.
#' @param draws See Usage.
decompose_effects <- function(Q_fn, W, friends, p_high = 1.0, p_low = 0.0,
                              seed = 0, draws = 200) {
  rows <- .tlnet1_mat(W)
  N <- length(rows)
  e <- .ghc_rng(seed)
  nd <- as.integer(draws)

  mean_with <- function(own, neigh_p) {
    tot <- 0.0
    for (d in seq_len(nd)) {
      u <- .ghc_unif(e, N)
      a <- ifelse(u < neigh_p, 1.0, 0.0)
      fs <- friend_summary(a, friends)
      s <- 0.0
      for (i in seq_len(N)) {
        s <- s + as.numeric(Q_fn(own, fs[[i]], rows[[i]]))
      }
      tot <- tot + s / N
    }
    tot / nd
  }

  d <- mean_with(1.0, p_low) - mean_with(0.0, p_low)
  s <- mean_with(0.0, p_high) - mean_with(0.0, p_low)
  tot <- mean_with(1.0, p_high) - mean_with(0.0, p_low)

  list(
    estimate = list(direct = d, spillover = s, total = tot),
    direct = d, spillover = s, total = tot,
    method = "direct and spillover decomposition under network interference; van der Laan & Rose (2018) Chap. 21",
    note = "under interference the estimand is a POLICY; 'the effect of treatment' is undefined until the whole assignment is specified"
  )
}

#' Variance with covariance along edges only
#' @param ic See Usage.
#' @param friends See Usage.
network_influence_variance <- function(ic, friends) {
  v <- .tlnet1_vec(ic)
  N <- length(v)
  if (length(friends) != N) {
    stop(sprintf("tlnet1: %d influence values but %d friend sets",
                 N, length(friends)))
  }
  m <- sum(v) / N
  var <- sum((v - m)^2) / N
  cov <- 0.0
  for (i in seq_len(N)) {
    fi <- .tlnet1_fset(friends, i)
    if (length(fi) > 0L) {
      cov <- cov + sum((v[i] - m) * (v[fi] - m))
    }
  }
  tot <- max((var + cov / N) / N, 0.0)
  list(
    se = sqrt(tot),
    se_independent = sqrt(var / N),
    edges_counted = .tlnet1_count_edges(friends),
    note = "correlation exists exactly along edges"
  )
}

#' One-line description of the module
#' @noRd
.tlnet1_cheatsheet <- function() {
  paste0("tlnet1: N units on a known social network, F_i = i's ",
         "friends, |F_i|/N -> 0. Two dependencies allowed: exposure ",
         "on own and friends' covariates, outcome on own and ",
         "friends' covariates and exposures -- and ALL dependence ",
         "is assumed described by the KNOWN network, which is the ",
         "assumption most likely to fail, since an unobserved edge ",
         "looks exactly like unmeasured confounding. Under ",
         "interference the estimand must be a POLICY; direct ",
         "(own exposure varies) and SPILLOVER (neighbours' varies) ",
         "effects are reported separately.")
}

# compact alias per ledger/NAMING.md
networksingletimepoint <- policy_mean

# module entry point
morie_tlnet1 <- policy_mean
