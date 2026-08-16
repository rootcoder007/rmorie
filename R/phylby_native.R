# phylby.R -- MrBayes 3: Bayesian phylogenetic inference under mixed models
# Reference: Ronquist, F., & Huelsenbeck, J. P. (2003) "MrBayes 3: Bayesian
# phylogenetic inference under mixed models", Bioinformatics 19(12), 1572-1574.
# doi:10.1093/bioinformatics/btg180

# Private helpers (prefixed with .phylby_)

#' .phylby_tips
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_splits_of}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A vector; its length is taken and its elements indexed.
#' @param out Optional; may be \code{NULL}. Passed to \code{.phylby_tips}.
#' @return The value of \code{out}, as built in the body.
#' @export
.phylby_tips <- function(node, out = NULL) {
  if (is.null(out)) out <- character(0)
  if (!is.list(node)) {
    out <- c(out, node)
    return(out)
  }
  nc <- length(node)
  for (c in seq(1, nc, 2)) {
    out <- .phylby_tips(node[[c]], out)
  }
  return(out)
}

#' .phylby_splits_of
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_clade_credibility}, \code{.phylby_topology_key}, \code{morie_phylby_splits_of}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_tips}.
#' @return The value of \code{[}.
#' @export
.phylby_splits_of <- function(tree) {
  all_tips <- sort(.phylby_tips(tree))
  n <- length(all_tips)
  acc <- new.env()
  acc$out <- list()
  walk <- function(node) {
    if (!is.list(node)) {
      return(node)
    }
    below <- character(0)
    nc <- length(node)
    for (c in seq(1, nc, 2)) {
      got <- walk(node[[c]])
      below <- union(below, got)
    }
    if (length(below) > 1 && length(below) < n - 1) {
      side <- below
      other <- setdiff(all_tips, below)
      side_sorted <- sort(side)
      other_sorted <- sort(other)
      if (side_sorted[1] < other_sorted[1]) {
        acc$out[[length(acc$out) + 1]] <- side
      } else {
        acc$out[[length(acc$out) + 1]] <- other
      }
    }
    return(below)
  }
  walk(tree)
  # Python accumulates splits in a set; a split reached twice is stored once.
  out <- acc$out
  if (length(out) < 2L) return(out)
  keys <- vapply(out, function(s) paste(sort(s), collapse = ","), character(1))
  out[!duplicated(keys)]
}

#' .phylby_topology_key
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_nni_neighbours}, \code{morie_phylby_topology_key}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_splits_of}.
#' @return The value of \code{[}.
#' @export
.phylby_topology_key <- function(tree) {
  splits <- .phylby_splits_of(tree)
  if (length(splits) == 0) return(list())
  sorted_splits <- lapply(splits, sort)
  keys <- sapply(sorted_splits, function(s) paste(s, collapse = ","))
  sorted_splits[order(keys)]
}

#' .phylby_replace_branch
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node See Usage.
#' @param path A vector; its length is taken and its elements indexed.
#' @param value Passed to \code{.phylby_replace_branch}.
#' @return The value of \code{parts}, as built in the body.
#' @export
.phylby_replace_branch <- function(node, path, value) {
  if (length(path) == 0) return(node)
  idx <- path[1] + 1
  parts <- node
  if (length(path) == 1) {
    parts[[idx]] <- value
  } else {
    parts[[idx]] <- .phylby_replace_branch(parts[[idx]], path[-1], value)
  }
  return(parts)
}

#' .phylby_branch_paths
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_log_posterior}, \code{.phylby_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A vector; its length is taken and its elements indexed.
#' @param path Defaults to \code{integer(0)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.phylby_branch_paths <- function(node, path = integer(0)) {
  if (!is.list(node)) return(list())
  if (length(node) %% 2 != 0) {
    stop("phylby: a node must be (child, length) pairs, got an odd number of entries")
  }
  out <- list()
  nc <- length(node)
  for (c in seq(1, nc, 2)) {
    out[[length(out) + 1]] <- c(path, c)
    child_paths <- .phylby_branch_paths(node[[c]], c(path, c - 1L))
    out <- c(out, child_paths)
  }
  return(out)
}

#' .phylby_subtrees
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_nni_neighbours}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A vector; its length is taken and its elements indexed.
#' @param path Defaults to \code{integer(0)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.phylby_subtrees <- function(node, path = integer(0)) {
  if (!is.list(node)) return(list())
  out <- list()
  nc <- length(node)
  for (c in seq(1, nc, 2)) {
    out[[length(out) + 1]] <- list(path = c(path, c - 1L), subtree = node[[c]])
    child_subtrees <- .phylby_subtrees(node[[c]], c(path, c - 1L))
    out <- c(out, child_subtrees)
  }
  return(out)
}

#' .phylby_set_at
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_nni_neighbours}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node See Usage.
#' @param path A vector; its length is taken and its elements indexed.
#' @param value Passed to \code{.phylby_set_at}.
#' @return The value of \code{parts}, as built in the body.
#' @export
.phylby_set_at <- function(node, path, value) {
  if (length(path) == 0) return(value)
  parts <- node
  idx <- path[1] + 1
  parts[[idx]] <- .phylby_set_at(parts[[idx]], path[-1], value)
  return(parts)
}

#' .phylby_get_at
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_log_posterior}, \code{.phylby_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A vector; indexed elementwise.
#' @param path See Usage.
#' @return The value of \code{node}, as built in the body.
#' @export
.phylby_get_at <- function(node, path) {
  for (i in path) {
    node <- node[[i + 1]]
  }
  return(node)
}

#' .phylby_nni_neighbours
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_step}, \code{morie_phylby_nni_neighbours}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_subtrees}.
#' @return The value of \code{uniq}, as built in the body.
#' @export
.phylby_nni_neighbours <- function(tree) {
  out <- list()
  subs <- .phylby_subtrees(tree)
  n <- length(subs)
  is_prefix <- function(p, q) {
    if (length(q) > length(p)) return(FALSE)
    all(p[seq_along(q)] == q)
  }
  for (i in seq_len(n)) {
    pa <- subs[[i]]$path
    a <- subs[[i]]$subtree
    for (j in seq_len(n)) {
      if (i == j) next
      pb <- subs[[j]]$path
      b <- subs[[j]]$subtree
      if (is_prefix(pa, pb) || is_prefix(pb, pa)) next
      cand <- .phylby_set_at(.phylby_set_at(tree, pa, b), pb, a)
      k1 <- .phylby_topology_key(cand)
      k2 <- .phylby_topology_key(tree)
      if (!identical(k1, k2)) {
        out[[length(out) + 1]] <- cand
      }
    }
  }
  seen <- new.env()
  seen$keys <- list()
  uniq <- list()
  for (t in out) {
    k <- .phylby_topology_key(t)
    key_str <- paste(sapply(k, function(s) paste(s, collapse = ",")), collapse = "|")
    if (is.null(seen$keys[[key_str]])) {
      seen$keys[[key_str]] <- TRUE
      uniq[[length(uniq) + 1]] <- t
    }
  }
  return(uniq)
}

#' .phylby_log_posterior
#'
#' A step of the phylby_native implementation. Called by \code{.phylby_step}, \code{morie_phylby_log_posterior}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_branch_paths}.
#' @param seqs A vector; indexed elementwise.
#' @param pi Passed to \code{morie_phylml}.
#' @param rate Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param branch_prior_mean Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param partitions Defaults to \code{NULL}.
#' @param rates Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param temperature Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A list with \code{loglik}, \code{logprior}, \code{logpost}.
#' @export
.phylby_log_posterior <- function(tree, seqs, pi = NULL, rate = 1.0,
                                    branch_prior_mean = 0.1,
                                    partitions = NULL, rates = NULL,
                                    temperature = 1.0) {
  if (branch_prior_mean <= 0) {
    stop("phylby: the branch prior mean must be positive")
  }
  if (rate <= 0) {
    stop("phylby: the substitution rate must be positive")
  }
  paths <- .phylby_branch_paths(tree)
  lengths <- sapply(paths, function(p) .phylby_get_at(tree, p))
  if (any(lengths < 0)) {
    stop("phylby: branch lengths must be non-negative")
  }
  lam <- 1.0 / branch_prior_mean
  log_prior <- sum(log(lam) - lam * lengths)
  ll <- 0.0
  if (is.null(partitions)) {
    ll <- morie_phylml(tree, seqs, pi, rate)$log_likelihood
  } else {
    names_p <- sort(unique(partitions))
    if (is.null(rates)) {
      rates <- as.list(setNames(rep(1.0, length(names_p)), names_p))
    }
    rate_vals <- sapply(names_p, function(k) {
      r <- rates[[k]]
      if (is.null(r)) 1.0 else r
    })
    if (any(rate_vals <= 0)) {
      stop("phylby: partition rates must be positive")
    }
    for (k in names_p) {
      keep <- which(partitions == k)
      sub <- list()
      for (t in names(seqs)) {
        s <- seqs[[t]]
        sub[[t]] <- paste(substring(s, keep, keep), collapse = "")
      }
      ll <- ll + morie_phylml(tree, sub, pi, rate * rates[[k]])$log_likelihood
    }
    log_prior <- log_prior + sum(-rate_vals)
  }
  return(list(loglik = ll, logprior = log_prior,
              logpost = temperature * (ll + log_prior)))
}

#' .phylby_rng
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seed Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{function}.
#' @export
.phylby_rng <- function(seed) {
  st <- as.numeric(seed) %% 2147483648
  if (st == 0) st <- 1L
  env <- new.env()
  env$st <- st
  function() {
    env$st <- .ghc_lcg31(env$st)
    env$st / 2^31
  }
}

#' .phylby_clade_credibility
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby_clade_credibility}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param samples A vector; its length is taken.
#' @return The value of \code{result}, as built in the body.
#' @export
.phylby_clade_credibility <- function(samples) {
  if (length(samples) == 0) {
    stop("phylby: no samples to summarise")
  }
  counts <- list()
  for (t in samples) {
    splits <- .phylby_splits_of(t)
    for (s in splits) {
      key <- paste(sort(s), collapse = ",")
      counts[[key]] <- if (is.null(counts[[key]])) 1 else counts[[key]] + 1
    }
  }
  n <- as.numeric(length(samples))
  result <- list()
  for (k in names(counts)) {
    result[[k]] <- counts[[k]] / n
  }
  return(result)
}

#' .phylby_step
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state A list; the body reads \code{$rate}, \code{$rates}, \code{$tree} from it.
#' @param seqs Passed to \code{.phylby_log_posterior}.
#' @param pi Passed to \code{.phylby_log_posterior}.
#' @param prior_mean Passed to \code{.phylby_log_posterior}.
#' @param partitions Passed to \code{.phylby_log_posterior}.
#' @param rnd See Usage.
#' @param beta Numeric; combined arithmetically in the body.
#' @param tune Numeric; combined arithmetically in the body.
#' @return A list with \code{state}, \code{accepted}.
#' @export
.phylby_step <- function(state, seqs, pi, prior_mean, partitions, rnd, beta, tune) {
  tree <- state$tree
  rate <- state$rate
  rates <- state$rates
  u <- rnd()
  hastings <- 0.0
  new_tree <- tree
  new_rate <- rate
  new_rates <- rates
  if (u < 0.4) {
    cand <- .phylby_nni_neighbours(tree)
    if (length(cand) == 0) {
      return(list(state = state, accepted = FALSE))
    }
    idx <- floor(rnd() * length(cand)) + 1
    new_tree <- cand[[idx]]
  } else if (u < 0.8) {
    paths <- .phylby_branch_paths(tree)
    p <- paths[[floor(rnd() * length(paths)) + 1]]
    m <- exp(tune * (rnd() - 0.5))
    new_tree <- .phylby_replace_branch(tree, p, .phylby_get_at(tree, p) * m)
    hastings <- log(m)
  } else {
    m <- exp(tune * (rnd() - 0.5))
    hastings <- log(m)
    new_rate <- rate * m
  }
  cur <- .phylby_log_posterior(tree, seqs, pi, rate, prior_mean, partitions, rates)$logpost
  prop <- .phylby_log_posterior(new_tree, seqs, pi, new_rate, prior_mean, partitions, new_rates)$logpost
  logalpha <- beta * (prop - cur) + hastings
  if (log(max(rnd(), 1e-300)) < logalpha) {
    return(list(state = list(tree = new_tree, rate = new_rate, rates = new_rates),
                accepted = TRUE))
  }
  return(list(state = state, accepted = FALSE))
}

# Public API

#' morie_phylby_chain_temperature
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param j Coerced to numeric by the body, with \code{as.numeric}.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{0.2}.
#' @return A numeric value.
#' @export
morie_phylby_chain_temperature <- function(j, lam = 0.2) {
  if (lam < 0) stop("phylby: the heating parameter must be >= 0")
  if (j < 0) stop("phylby: the chain index must be >= 0")
  1.0 / (1.0 + lam * as.numeric(j))
}

#' morie_phylby_swap_acceptance
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta_j Numeric; combined arithmetically in the body.
#' @param beta_k Numeric; combined arithmetically in the body.
#' @param logp_j Numeric; combined arithmetically in the body.
#' @param logp_k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
morie_phylby_swap_acceptance <- function(beta_j, beta_k, logp_j, logp_k) {
  min(1.0, exp(min((beta_j - beta_k) * (logp_k - logp_j), 700.0)))
}

#' morie_phylby_splits_of
#'
#' A step of the phylby_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_splits_of}.
#' @return The value of \code{.phylby_splits_of}.
#' @export
morie_phylby_splits_of <- function(tree) {
  .phylby_splits_of(tree)
}

#' morie_phylby_topology_key
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_topology_key}.
#' @return The value of \code{.phylby_topology_key}.
#' @export
morie_phylby_topology_key <- function(tree) {
  .phylby_topology_key(tree)
}

#' morie_phylby_nni_neighbours
#'
#' A step of the phylby_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_nni_neighbours}.
#' @return The value of \code{.phylby_nni_neighbours}.
#' @export
morie_phylby_nni_neighbours <- function(tree) {
  .phylby_nni_neighbours(tree)
}

#' morie_phylby_log_posterior
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylby_log_posterior}.
#' @param seqs Passed to \code{.phylby_log_posterior}.
#' @param pi Passed to \code{.phylby_log_posterior}.
#' @param rate Passed to \code{.phylby_log_posterior}. Defaults to \code{1}.
#' @param branch_prior_mean Passed to \code{.phylby_log_posterior}. Defaults to \code{0.1}.
#' @param partitions Passed to \code{.phylby_log_posterior}.
#' @param rates Passed to \code{.phylby_log_posterior}.
#' @param temperature Passed to \code{.phylby_log_posterior}. Defaults to \code{1}.
#' @return The value of \code{.phylby_log_posterior}.
#' @export
morie_phylby_log_posterior <- function(tree, seqs, pi = NULL, rate = 1.0,
                                       branch_prior_mean = 0.1,
                                       partitions = NULL, rates = NULL,
                                       temperature = 1.0) {
  .phylby_log_posterior(tree, seqs, pi, rate, branch_prior_mean, partitions, rates, temperature)
}

#' morie_phylby_clade_credibility
#'
#' A step of the phylby_native implementation. Called by \code{morie_phylby}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param samples Passed to \code{.phylby_clade_credibility}.
#' @return The value of \code{.phylby_clade_credibility}.
#' @export
morie_phylby_clade_credibility <- function(samples) {
  .phylby_clade_credibility(samples)
}

#' morie_phylby
#'
#' A step of the phylby_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alignment A vector; indexed elementwise.
#' @param n_iter Numeric; combined arithmetically in the body. Defaults to \code{2000}.
#' @param burnin Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param n_chains Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @param lam Passed to \code{morie_phylby_chain_temperature}. Defaults to \code{0.2}.
#' @param swap_every Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param sample_every Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param pi Passed to \code{.phylby_step}.
#' @param rate Defaults to \code{1}.
#' @param branch_prior_mean Passed to \code{.phylby_step}. Defaults to \code{0.1}.
#' @param partitions Optional; may be \code{NULL}. A vector; its length is taken.
#' @param tree Defaults to \code{NULL}.
#' @param n_runs Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @param tune Passed to \code{.phylby_step}. Defaults to \code{1}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_phylby <- function(alignment, n_iter = 2000, burnin = NULL, n_chains = 4,
                          lam = 0.2, swap_every = 10, sample_every = 10,
                          pi = NULL, rate = 1.0, branch_prior_mean = 0.1,
                          partitions = NULL, tree = NULL, n_runs = 2,
                          tune = 1.0, seed = 0) {
  seqs <- list()
  for (k in names(alignment)) {
    seqs[[k]] <- toupper(as.character(alignment[[k]]))
  }
  if (length(seqs) < 4) {
    stop("phylby: at least four taxa are needed for an unrooted topology to vary")
  }
  Ls <- unique(sapply(seqs, nchar))
  if (length(Ls) != 1 || Ls[1] == 0) {
    stop("phylby: sequences must be aligned and non-empty")
  }
  if (n_iter < 1 || n_chains < 1 || n_runs < 1) {
    stop("phylby: n_iter, n_chains and n_runs must be positive")
  }
  if (swap_every < 1 || sample_every < 1) {
    stop("phylby: swap_every and sample_every must be positive")
  }
  if (!is.null(partitions) && length(partitions) != nchar(seqs[[1]])) {
    stop("phylby: one partition label per site is required")
  }
  burn <- if (is.null(burnin)) as.integer(n_iter) %/% 2L else as.integer(burnin)
  if (burn < 0 || burn >= n_iter) {
    stop("phylby: burnin must be less than n_iter")
  }
  names_vec <- sort(names(seqs))
  if (is.null(tree)) {
    tree <- list(names_vec[1], 0.1, names_vec[2], 0.1)
    if (length(names_vec) > 2) {
      for (i in 3:length(names_vec)) {
        t <- names_vec[i]
        tree <- list(tree, 0.1, t, 0.1)
      }
    }
  }
  parts <- if (is.null(partitions)) character(0) else sort(unique(partitions))
  runs <- list()
  accepted <- 0
  swaps <- 0
  proposed_swaps <- 0
  for (r in seq_len(as.integer(n_runs))) {
    rnd <- .phylby_rng(seed + 1000 * (r - 1) + 1)
    chains <- list()
    for (j in seq_len(as.integer(n_chains))) {
      chain_rates <- as.list(setNames(rep(1.0, length(parts)), parts))
      chains[[j]] <- list(tree = tree, rate = rate, rates = chain_rates)
    }
    betas <- sapply(seq(0, as.integer(n_chains) - 1),
                    function(j) morie_phylby_chain_temperature(j, lam))
    samples <- list()
    for (it in seq_len(as.integer(n_iter))) {
      it0 <- it - 1
      for (j in seq_len(as.integer(n_chains))) {
        result <- .phylby_step(chains[[j]], seqs, pi, branch_prior_mean,
                                partitions, rnd, betas[j], tune)
        chains[[j]] <- result$state
        if (j == 1 && result$accepted) {
          accepted <- accepted + 1
        }
      }
      if (as.integer(n_chains) > 1 && (it0 + 1) %% as.integer(swap_every) == 0) {
        a <- floor(rnd() * n_chains) + 1
        b <- floor(rnd() * n_chains) + 1
        if (a != b) {
          proposed_swaps <- proposed_swaps + 1
          la <- morie_phylby_log_posterior(chains[[a]]$tree, seqs, pi,
                                           chains[[a]]$rate, branch_prior_mean,
                                           partitions, chains[[a]]$rates)$logpost
          lb <- morie_phylby_log_posterior(chains[[b]]$tree, seqs, pi,
                                           chains[[b]]$rate, branch_prior_mean,
                                           partitions, chains[[b]]$rates)$logpost
          if (rnd() < morie_phylby_swap_acceptance(betas[a], betas[b], la, lb)) {
            temp <- chains[[a]]
            chains[[a]] <- chains[[b]]
            chains[[b]] <- temp
            swaps <- swaps + 1
          }
        }
      }
      if (it0 >= burn && (it0 - burn) %% as.integer(sample_every) == 0) {
        samples[[length(samples) + 1]] <- chains[[1]]$tree
      }
    }
    if (length(samples) == 0) {
      samples[[1]] <- chains[[1]]$tree
    }
    runs[[r]] <- samples
  }
  cred <- lapply(runs, morie_phylby_clade_credibility)
  keys <- unique(unlist(lapply(cred, names)))
  asdsf <- 0.0
  if (length(runs) > 1 && length(keys) > 0) {
    tot <- 0.0
    for (k in keys) {
      fs <- sapply(cred, function(c) if (is.null(c[[k]])) 0.0 else c[[k]])
      m <- mean(fs)
      tot <- tot + sqrt(mean((fs - m)^2))
    }
    asdsf <- tot / length(keys)
  }
  pooled <- list()
  for (s in runs) {
    for (t in s) {
      pooled[[length(pooled) + 1]] <- t
    }
  }
  topo <- list()
  for (t in pooled) {
    k <- morie_phylby_topology_key(t)
    key_str <- paste(sapply(k, function(s) paste(s, collapse = ",")), collapse = "|")
    topo[[key_str]] <- if (is.null(topo[[key_str]])) 1 else topo[[key_str]] + 1
  }
  best_key <- names(which.max(sapply(topo, function(x) x)))
  result <- list(
    estimate = morie_phylby_clade_credibility(pooled),
    clade_credibility = morie_phylby_clade_credibility(pooled),
    samples = pooled,
    runs = runs,
    map_topology = best_key,
    map_probability = topo[[best_key]] / length(pooled),
    topology_counts = topo,
    asdsf = asdsf,
    acceptance = accepted / (n_iter * n_runs),
    swap_rate = if (proposed_swaps > 0) swaps / proposed_swaps else 0.0,
    temperatures = sapply(seq(0, as.integer(n_chains) - 1),
                          function(j) morie_phylby_chain_temperature(j, lam)),
    n_chains = as.integer(n_chains),
    n_runs = as.integer(n_runs),
    n_samples = length(pooled),
    method = "MrBayes 3 (Ronquist & Huelsenbeck 2003): Metropolis-coupled MCMC over topology, branch lengths and model, with a uniform topology prior and exponential branch lengths",
    note = "the likelihood is Felsenstein pruning from morie.fn.phylml; asdsf is the average standard deviation of split frequencies between independent runs, the diagnostic MrBayes prints, and should approach zero"
  )
  return(result)
}

morie_bayesian_phylogeny <- morie_phylby

#' morie_phylby_cheatsheet
#'
#' A step of the phylby_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_phylby_cheatsheet <- function() {
  "phylby: MrBayes 3 (Ronquist & Huelsenbeck 2003). MCMC over (topology, branch lengths, rate) with a uniform topology prior and exponential branch lengths, the likelihood coming from Felsenstein pruning. Metropolis coupling runs n chains at beta_j = 1/(1 + lambda j) and swaps them with min(1, exp[(beta_j - beta_k)(l_k - l_j)]); only the cold chain is sampled. Partitions give each subset of sites its own rate. Convergence is judged by the average standard deviation of split frequencies between independent runs."
}
