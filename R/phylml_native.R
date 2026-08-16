# Maximum-likelihood phylogeny: Felsenstein's pruning algorithm and the
# F81 substitution model.
#
# Felsenstein, J. (1981) "Evolutionary Trees from DNA Sequences: A
# Maximum Likelihood Approach", *Journal of Molecular Evolution* 17,
# 368-376.
#
# Sites are assumed independent, so the likelihood is computed per site
# and multiplied across sites. Written out naively, the likelihood of a
# tree sums over the unknown states at every interior node -- "this
# expression will have 256 terms, and in general the expression for
# :math:`n` species will have :math:`2^{2n-2}` terms, which can easily be
# a very large number".
#
# The economy comes from moving the summation signs rightwards. Define
# :math:`L_s(k)`, the likelihood of the data at or above node :math:`k`
# given that :math:`k` has state :math:`s`. At a tip, :math:`L_s(k)` is
# 0 for every state except the one observed, where it is 1. At an
# interior node with children :math:`i` and :math:`j` joined by branches
# :math:`v_i` and :math:`v_j`,
#
# .. math:: L_{s}(k) = [\sum_{s_i} P_{s s_i}(v_i) L_{s_i}(i)]
#                      [\sum_{s_j} P_{s s_j}(v_j) L_{s_j}(j)],
#
# evaluated by a postorder traversal, and the site likelihood is
# (eq. 5)
#
# .. math:: L = \sum_{s_0} \pi_{s_0} L_{s_0}(0).
#
# Felsenstein calls this **pruning**, "since it in effect removes two
# tips from the tree at each step". It turns an exponential sum into a
# linear one.
#
# The substitution model (his eqs. 6-7) assumes that in time
# :math:`dt` a base is replaced with probability :math:`u dt`, its
# replacement being :math:`j` with probability :math:`\pi_j` -- so a base
# may be "replaced" by itself and not every substitution is observable.
# That gives
#
# .. math:: P_{ij}(t) = e^{-ut} \delta_{ij} + (1 - e^{-ut}) \pi_j,
#
# which follows once you notice that :math:`e^{-ut}` is the probability
# of no change at all and that any change lands in :math:`j` with
# probability :math:`\pi_j`. This is the model now called **F81**; at
# :math:`\pi = (1/4,1/4,1/4,1/4)` it reduces to Jukes-Cantor.
#
# Two properties of the model are load-bearing and both are anchored:
#
# *Reversibility* (his eq. 8), :math:`\pi_i P_{ij}(t) = \pi_j
# P_{ji}(t)`, means the process looks the same run forwards or
# backwards, so the tree may be rooted anywhere without changing the
# likelihood.
#
# The *pulley principle*: the likelihood depends on the two branches
# either side of the root only through their **sum**, so length may be
# slid from one to the other freely. That is why an unrooted tree is
# what is actually identifiable, and it is checked here directly.
#
# ``optimise_branch`` maximises the likelihood over a single branch by
# golden-section search, which is the one-dimensional step the paper's
# iterative scheme is built from.

.phylml_BASES <- c("A", "C", "G", "T")

#' .phylml_pi
#'
#' A step of the phylml_native implementation. Called by \code{morie_phylml}, \code{site_likelihood}, \code{substitution_matrix}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pi See Usage.
#' @return The value of \code{p}, as built in the body.
#' @export
.phylml_pi <- function(pi) {
  if (is.null(pi)) {
    return(rep(0.25, 4))
  }
  p <- as.numeric(pi)
  if (length(p) != 4) {
    stop("phylml: pi must have four entries (A, C, G, T)")
  }
  if (any(p < 0)) {
    stop("phylml: pi must be non-negative")
  }
  s <- sum(p)
  if (abs(s - 1.0) > 1e-9) {
    stop(sprintf("phylml: pi must sum to 1, got %g", s))
  }
  return(p)
}

#' substitution_matrix
#'
#' A step of the phylml_native implementation. Called by \code{.phylml_prune}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Numeric; combined arithmetically in the body.
#' @param pi Passed to \code{.phylml_pi}.
#' @param u Defaults to \code{1}.
#' @return The value of \code{M}, as built in the body.
#' @export
substitution_matrix <- function(t, pi = NULL, u = 1.0) {
  p <- .phylml_pi(pi)
  t <- as.numeric(t)
  if (t < 0.0) {
    stop("phylml: branch length must be >= 0")
  }
  e <- exp(-as.numeric(u) * t)
  M <- matrix(0.0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    for (j in 1:4) {
      M[i, j] <- e * (if (i == j) 1.0 else 0.0) + (1.0 - e) * p[j]
    }
  }
  return(M)
}

#' .phylml_tip_vector
#'
#' A step of the phylml_native implementation. Called by \code{.phylml_prune}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param base See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
.phylml_tip_vector <- function(base) {
  b <- toupper(as.character(base))
  if (b %in% c("-", "N", "?")) {
    return(rep(1.0, 4))
  }
  if (!(b %in% .phylml_BASES)) {
    stop(sprintf("phylml: unknown base %s; expected one of ACGT or a gap", base))
  }
  v <- rep(0.0, 4)
  v[which(.phylml_BASES == b)] <- 1.0
  return(v)
}

#' .phylml_prune
#'
#' A step of the phylml_native implementation. Called by \code{site_likelihood}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A vector; its length is taken and its elements indexed.
#' @param site Character; passed to \code{substr}.
#' @param pi Passed to \code{.phylml_prune}.
#' @param u Passed to \code{.phylml_prune}.
#' @param seqs A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.phylml_prune <- function(node, site, pi, u, seqs) {
  if (!is.list(node)) {
    return(.phylml_tip_vector(substr(seqs[[node]], site, site)))
  }
  if (length(node) %% 2 != 0) {
    stop(sprintf("phylml: a node must be (child, length, ...) pairs, got %d entries", length(node)))
  }
  out <- rep(1.0, 4)
  idx <- seq(1, length(node), by = 2)
  for (k in idx) {
    child <- node[[k]]
    v <- node[[k + 1]]
    below <- .phylml_prune(child, site, pi, u, seqs)
    P <- substitution_matrix(v, pi, u)
    for (s in 1:4) {
      out[s] <- out[s] * sum(P[s, ] * below)
    }
  }
  return(out)
}

#' site_likelihood
#'
#' A step of the phylml_native implementation. Called by \code{morie_phylml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree Passed to \code{.phylml_prune}.
#' @param seqs Passed to \code{.phylml_prune}.
#' @param site Passed to \code{.phylml_prune}.
#' @param pi Passed to \code{.phylml_pi}.
#' @param u Passed to \code{.phylml_prune}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
site_likelihood <- function(tree, seqs, site, pi = NULL, u = 1.0) {
  p <- .phylml_pi(pi)
  L <- .phylml_prune(tree, site, p, u, seqs)
  return(sum(p * L))
}

#' morie_phylml
#'
#' A step of the phylml_native implementation. Called by \code{.phylby_log_posterior}, \code{optimise_branch}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree See Usage.
#' @param seqs A vector; its length is taken.
#' @param pi Passed to \code{.phylml_pi}.
#' @param u Defaults to \code{1}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_phylml <- function(tree, seqs, pi = NULL, u = 1.0) {
  p <- .phylml_pi(pi)
  if (!(is.list(seqs) || is.character(seqs)) || length(seqs) == 0) {
    stop("phylml: seqs must be a non-empty dict of name -> sequence")
  }
  if (is.list(seqs)) {
    lens <- sapply(seqs, function(v) nchar(as.character(v)))
  } else {
    lens <- nchar(seqs)
  }
  if (length(unique(lens)) != 1) {
    stop(sprintf("phylml: sequences must be aligned to a common length, got %s",
                 paste(sort(unique(lens)), collapse = ", ")))
  }
  n_sites <- unique(lens)[1]
  if (n_sites == 0) {
    stop("phylml: sequences are empty")
  }

  site_L <- numeric(n_sites)
  for (i in 1:n_sites) {
    Li <- site_likelihood(tree, seqs, i, p, u)
    if (Li <= 0.0) {
      stop(sprintf("phylml: site %d has zero likelihood; check the tree and the alignment", i))
    }
    site_L[i] <- Li
  }
  logs <- log(site_L)
  total <- sum(logs)
  result <- list(
    estimate = as.numeric(total),
    log_likelihood = as.numeric(total),
    site_likelihoods = as.numeric(site_L),
    site_log_likelihoods = as.numeric(logs),
    n_sites = as.integer(n_sites),
    n_taxa = as.integer(length(seqs)),
    pi = p,
    u = as.numeric(u),
    method = "ML phylogeny by pruning (Felsenstein 1981)"
  )
  class(result) <- c("morie_richresult", "list")
  return(result)
}

#' optimise_branch
#'
#' A step of the phylml_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param make_tree See Usage.
#' @param seqs Passed to \code{morie_phylml}.
#' @param pi Passed to \code{morie_phylml}.
#' @param u Passed to \code{morie_phylml}. Defaults to \code{1}.
#' @param lo Defaults to \code{1e-06}.
#' @param hi Defaults to \code{10}.
#' @param tol Defaults to \code{1e-10}.
#' @param max_iter Defaults to \code{200}.
#' @return The value of \code{result}, as built in the body.
#' @export
optimise_branch <- function(make_tree, seqs, pi = NULL, u = 1.0, lo = 1e-6, hi = 10.0,
                            tol = 1e-10, max_iter = 200) {
  if (!is.function(make_tree)) {
    stop("phylml: make_tree must be callable")
  }
  if (!(lo < hi)) {
    stop("phylml: need lo < hi")
  }
  g <- (sqrt(5.0) - 1.0) / 2.0
  a <- as.numeric(lo)
  b <- as.numeric(hi)
  c <- b - g * (b - a)
  d <- a + g * (b - a)
  fc <- morie_phylml(make_tree(c), seqs, pi, u)$log_likelihood
  fd <- morie_phylml(make_tree(d), seqs, pi, u)$log_likelihood
  for (iter in 1:max_iter) {
    if (fc > fd) {
      new_b <- d
      new_d <- c
      new_fd <- fc
      b <- new_b
      d <- new_d
      fd <- new_fd
      c <- b - g * (b - a)
      fc <- morie_phylml(make_tree(c), seqs, pi, u)$log_likelihood
    } else {
      new_a <- c
      new_c <- d
      new_fc <- fd
      a <- new_a
      c <- new_c
      fc <- new_fc
      d <- a + g * (b - a)
      fd <- morie_phylml(make_tree(d), seqs, pi, u)$log_likelihood
    }
    if (abs(b - a) < tol) {
      break
    }
  }
  v <- 0.5 * (a + b)
  edge <- 1e-6 * (as.numeric(hi) - as.numeric(lo))
  at_bound <- (v <= as.numeric(lo) + edge || v >= as.numeric(hi) - edge)
  ll <- morie_phylml(make_tree(v), seqs, pi, u)$log_likelihood
  result <- list(
    estimate = as.numeric(v),
    length = as.numeric(v),
    log_likelihood = as.numeric(ll),
    at_bound = as.logical(at_bound),
    bounds = c(as.numeric(lo), as.numeric(hi)),
    method = "branch optimisation (Felsenstein 1981)"
  )
  class(result) <- c("morie_richresult", "list")
  return(result)
}

#' .phylml_cheatsheet
#'
#' A step of the phylml_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.phylml_cheatsheet <- function() {
  return("phylml: Felsenstein (1981) pruning. L_s(k) = prod over children of sum_x P_sx(v) L_x(child); tips are 0/1 indicators; L = sum_s pi_s L_s(root) (eq. 5). Turns a 2^(2n-2)-term sum into a linear traversal. F81 model P_ij(t) = e^-ut delta_ij + (1-e^-ut) pi_j (eq. 7), reversible, and the PULLEY PRINCIPLE means the two root branches matter only through their sum.")
}

# compact alias per ledger/NAMING.md
maximum_likelihood_phylogeny <- morie_phylml

# public names resolved by fn/_lazy_map.json
phylogenetic_ml <- morie_phylml
phylogeneticml <- morie_phylml
