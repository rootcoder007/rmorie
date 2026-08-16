# Felsenstein pruning likelihood for trees.
# Source: Felsenstein (1981), J. Molecular Evolution 17(6), 368-376,
# Eqs. 4-7 and the pruning algorithm
# (fetched-wave3/Evolutionary trees from DNA sequences- A maximum
# likelihood approach.pdf).  Mirrors Python morie.fn.felsen exactly.

#' .felsen_pij
#'
#' Part of the felsen_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param pi See Usage.
#' @return The value of \code{P}, as built in the body.
#' @export
.felsen_pij <- function(t, pi) {
  e <- exp(-t)
  P <- matrix(rep(pi, each = 4) * (1 - e), 4, 4)
  diag(P) <- diag(P) + e
  P
}

#' .felsen_prune
#'
#' Part of the felsen_native implementation; see the file header for the
#' source it follows.
#'
#' @param node See Usage.
#' @param site See Usage.
#' @param pi See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.felsen_prune <- function(node, site, pi) {
  if (is.character(node)) {
    s <- match(site[[node]], c("A", "C", "G", "T"))
    out <- numeric(4)
    out[s] <- 1
    return(out)
  }
  out <- rep(1, 4)
  for (ch in node) {
    Lc <- .felsen_prune(ch[[1]], site, pi)
    P <- .felsen_pij(as.numeric(ch[[2]]), pi)
    out <- out * as.numeric(P %*% Lc)
  }
  out
}

#' Felsenstein (1981) pruning likelihood under the F81 model
#'
#' Conditional likelihoods combine by postorder traversal
#' (L_s(k) = prod_children sum_j P_sj(v) L_j(child)); site
#' likelihood L = sum_s pi_s L_s(root); substitution
#' P_ij(t) = e^{-t} delta_ij + (1 - e^{-t}) pi_j.
#'
#' @param tree Nested list: leaf = taxon name (character); internal
#'   node = list of list(child, branch_length) pairs.
#' @param sites List of named lists {taxon = base}.
#' @param pi Stationary base frequencies (default uniform).
#' @return A list with elements \code{loglik},
#'   \code{site_likelihoods}, \code{n_sites}, \code{pi},
#'   \code{method}.
#' @references Felsenstein, J. (1981). Evolutionary trees from DNA
#'   sequences: a maximum likelihood approach. Journal of Molecular
#'   Evolution, 17(6), 368-376.
#' @export
morie_felsen <- function(tree, sites, pi = NULL) {
  if (is.null(pi)) pi <- rep(0.25, 4)
  pi <- as.numeric(pi)
  if (abs(sum(pi) - 1) > 1e-9 || any(pi <= 0)) {
    stop("pi must be positive and sum to 1")
  }
  if (!length(sites)) stop("need at least one site")
  liks <- numeric(length(sites))
  ll <- 0
  for (i in seq_along(sites)) {
    L0 <- .felsen_prune(tree, sites[[i]], pi)
    L <- sum(pi * L0)
    if (L <= 0) stop("zero likelihood site (bad pattern?)")
    liks[i] <- L
    ll <- ll + log(L)
  }
  list(loglik = ll, site_likelihoods = liks,
       n_sites = length(sites), pi = pi,
       method = "Felsenstein (1981) pruning, F81 model (Eqs. 5-7)")
}
