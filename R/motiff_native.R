# Network motif significance.
# Source: Milo et al. (2002), Science 298(5594), 824-827
# (fetched-wave3/Network motifs simple building blocks of complex
# networks.pdf).  Mirrors Python morie.fn.motiff exactly (same
# SplitMix64 swap-index stream, same triad enumeration).

.motiff_triads <- function(adj, n) {
  ff <- 0L
  cyc <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j == i || !adj[i, j]) next
      for (k in seq_len(n)) {
        if (k == i || k == j) next
        if (adj[i, k] && adj[j, k]) ff <- ff + 1L
        if (adj[j, k] && adj[k, i]) cyc <- cyc + 1L
      }
    }
  }
  list(ffl = ff, cycle3 = cyc %/% 3L)
}

.motiff_shuffle <- function(adj, n, e, swaps) {
  # row-major edge enumeration, matching the Python
  # [(i, j) for i in range(n) for j in range(n)] order exactly
  ei <- integer(0); ej <- integer(0)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i != j && adj[i, j]) { ei <- c(ei, i); ej <- c(ej, j) }
  }
  E <- cbind(ei, ej)
  present <- new.env(hash = TRUE)
  key <- function(i, j) paste0(i, "_", j)
  for (r in seq_len(nrow(E))) assign(key(E[r, 1], E[r, 2]), TRUE, present)
  m <- nrow(E)
  for (s in seq_len(swaps)) {
    a <- floor(.ghc_unif(e, 1) * m) + 1
    b <- floor(.ghc_unif(e, 1) * m) + 1
    if (a == b) next
    i1 <- E[a, 1]; j1 <- E[a, 2]; i2 <- E[b, 1]; j2 <- E[b, 2]
    if (i1 == i2 || j1 == j2 || i1 == j2 || i2 == j1) next
    if (exists(key(i1, j2), present, inherits = FALSE) ||
        exists(key(i2, j1), present, inherits = FALSE)) next
    rm(list = c(key(i1, j1), key(i2, j2)), envir = present)
    assign(key(i1, j2), TRUE, present)
    assign(key(i2, j1), TRUE, present)
    E[a, 2] <- j2; E[b, 2] <- j1
  }
  new <- matrix(0L, n, n)
  for (r in seq_len(nrow(E))) new[E[r, 1], E[r, 2]] <- 1L
  new
}

#' Milo et al. (2002) network-motif significance
#'
#' Z = (N_real - mean N_rand)/sd N_rand over a degree-preserving
#' randomized ensemble; p = fraction of randomizations with count
#' >= real (P < 0.01 motif cutoff).  Feed-forward loop and 3-cycle.
#'
#' @param adjacency Directed 0/1 adjacency (n x n).
#' @param motif "ffl" or "cycle3".
#' @param n_random Ensemble size.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param swaps Edge swaps per randomization (default 10 * edges).
#' @return A list with elements \code{count}, \code{z_score},
#'   \code{p_value}, \code{rand_mean}, \code{rand_sd}, \code{motif},
#'   \code{n_random}, \code{seed}, \code{method}.
#' @references Milo, R. et al. (2002). Network motifs: simple
#'   building blocks of complex networks. Science, 298(5594),
#'   824-827.
#' @export
morie_motiff <- function(adjacency, motif = "ffl", n_random = 100,
                         seed = 0, swaps = NULL) {
  A <- matrix(as.integer(as.matrix(adjacency) != 0),
              nrow = nrow(adjacency))
  n <- nrow(A)
  if (ncol(A) != n || n < 3) stop("adjacency must be square, n >= 3")
  if (!motif %in% c("ffl", "cycle3")) stop("motif must be 'ffl' or 'cycle3'")
  real <- .motiff_triads(A, n)[[motif]]
  m_edges <- sum(A)
  if (is.null(swaps)) swaps <- 10 * max(m_edges, 1)
  e <- .ghc_rng(seed)
  rand <- integer(n_random)
  for (r in seq_len(n_random)) {
    Ar <- .motiff_shuffle(A, n, e, as.integer(swaps))
    rand[r] <- .motiff_triads(Ar, n)[[motif]]
  }
  mu <- mean(rand)
  sd_ <- if (length(rand) > 1) stats::sd(rand) else 0
  z <- if (sd_ > 0) (real - mu) / sd_ else if (real == mu) 0 else Inf
  p <- (sum(rand >= real) + 1) / (length(rand) + 1)
  list(count = real, z_score = z, p_value = p, rand_mean = mu,
       rand_sd = sd_, motif = motif, n_random = as.integer(n_random),
       seed = seed,
       method = "Milo et al. (2002) motif Z score / p-value")
}
