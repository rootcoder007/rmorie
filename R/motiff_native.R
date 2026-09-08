# Network motif significance.
# Source: Milo et al. (2002), Science 298(5594), 824-827
# (fetched-wave3/Network motifs simple building blocks of complex
# networks.pdf).  Mirrors Python morie.fn.motiff exactly (same
# SplitMix64 swap-index stream, same triad enumeration).

#' .motiff_triads
#'
#' A step of the motiff_native implementation. Called by \code{morie_motiff}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{ffl}, \code{cycle3}.
#' @export
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

#' Mfinder switching (Milo refs. 17-18) mirroring Python exactly:
#'
#' row-major classify into single edges and mutual (bidirectional)
#' pairs; single<->single and mutual<->mutual switches only, so the
#' mutual-edge count is invariant; identical RNG draw order.
#'
#' @param adj A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param e Passed to \code{.ghc_unif}.
#' @param swaps A count; the body uses it as \code{seq_len(...)}.
#' @param preserve_mutual A flag; the body branches on it.
#' @return The value of \code{new}, as built in the body.
#' @export
.motiff_shuffle <- function(adj, n, e, swaps, preserve_mutual) {
  # mfinder switching (Milo refs. 17-18) mirroring Python exactly:
  # row-major classify into single edges and mutual (bidirectional)
  # pairs; single<->single and mutual<->mutual switches only, so the
  # mutual-edge count is invariant; identical RNG draw order.
  key <- function(i, j) paste0(i, "_", j)
  present <- new.env(hash = TRUE)
  si <- integer(0)
  sj <- integer(0)          # single edges
  mi <- integer(0)
  mj <- integer(0)          # mutual pairs [min,max]
  seen <- new.env(hash = TRUE)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j || !adj[i, j]) next
    if (adj[j, i]) {
      lo <- min(i, j)
      hi <- max(i, j)
      kk <- key(lo, hi)
      if (!exists(kk, seen, inherits = FALSE)) {
        assign(kk, TRUE, seen)
        mi <- c(mi, lo)
        mj <- c(mj, hi)
      }
    } else {
      si <- c(si, i)
      sj <- c(sj, j)
    }
  }
  S <- cbind(si, sj)
  M <- cbind(mi, mj)
  for (r in seq_len(nrow(S))) assign(key(S[r, 1], S[r, 2]), TRUE, present)
  for (r in seq_len(nrow(M))) {
    assign(key(M[r, 1], M[r, 2]), TRUE, present)
    assign(key(M[r, 2], M[r, 1]), TRUE, present)
  }
  ns <- nrow(S)
  nm <- nrow(M)
  tot <- ns + nm
  frac_m <- if (tot > 0) nm / tot else 0
  has <- function(i, j) exists(key(i, j), present, inherits = FALSE)
  for (s in seq_len(swaps)) {
    pool_mut <- FALSE
    if (isTRUE(preserve_mutual)) pool_mut <- (.ghc_unif(e, 1) < frac_m)
    ua <- .ghc_unif(e, 1)
    ub <- .ghc_unif(e, 1)
    if (pool_mut) {
      if (nm < 2) next
      a <- floor(ua * nm) + 1
      b <- floor(ub * nm) + 1
      if (a == b) next
      i1 <- M[a, 1]
      j1 <- M[a, 2]
      i2 <- M[b, 1]
      j2 <- M[b, 2]
      if (length(unique(c(i1, j1, i2, j2))) < 4) next
      if (has(i1, j2) || has(j2, i1) || has(i2, j1) || has(j1, i2)) next
      rm(list = c(key(i1, j1), key(j1, i1), key(i2, j2), key(j2, i2)),
         envir = present)
      assign(key(i1, j2), TRUE, present)
      assign(key(j2, i1), TRUE, present)
      assign(key(i2, j1), TRUE, present)
      assign(key(j1, i2), TRUE, present)
      M[a, ] <- c(min(i1, j2), max(i1, j2))
      M[b, ] <- c(min(i2, j1), max(i2, j1))
    } else {
      if (ns < 2) next
      a <- floor(ua * ns) + 1
      b <- floor(ub * ns) + 1
      if (a == b) next
      i1 <- S[a, 1]
      j1 <- S[a, 2]
      i2 <- S[b, 1]
      j2 <- S[b, 2]
      if (length(unique(c(i1, j1, i2, j2))) < 4) next
      if (has(i1, j2) || has(i2, j1)) next
      if (isTRUE(preserve_mutual) && (has(j2, i1) || has(j1, i2))) next
      rm(list = c(key(i1, j1), key(i2, j2)), envir = present)
      assign(key(i1, j2), TRUE, present)
      assign(key(i2, j1), TRUE, present)
      S[a, 2] <- j2
      S[b, 2] <- j1
    }
  }
  new <- matrix(0L, n, n)
  for (nm_key in ls(present)) {
    ij <- as.integer(strsplit(nm_key, "_")[[1]])
    new[ij[1], ij[2]] <- 1L
  }
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
#' @param preserve_mutual TRUE (default) preserves the mutual-edge
#'   count (mfinder, Milo refs. 17-18); FALSE fixes only in/out
#'   degree.
#' @return A list with elements \code{count}, \code{z_score},
#'   \code{p_value}, \code{rand_mean}, \code{rand_sd}, \code{motif},
#'   \code{n_random}, \code{seed}, \code{method}.
#' @references Milo, R. et al. (2002). Network motifs: simple
#'   building blocks of complex networks. Science, 298(5594),
#'   824-827.
#' @export
morie_motiff <- function(adjacency, motif = "ffl", n_random = 100,
                         seed = 0, swaps = NULL, preserve_mutual = TRUE) {
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
    Ar <- .motiff_shuffle(A, n, e, as.integer(swaps),
                          isTRUE(preserve_mutual))
    rand[r] <- .motiff_triads(Ar, n)[[motif]]
  }
  mu <- mean(rand)
  sd_ <- if (length(rand) > 1) stats::sd(rand) else 0
  z <- if (sd_ > 0) (real - mu) / sd_ else if (real == mu) 0 else Inf
  p <- (sum(rand >= real) + 1) / (length(rand) + 1)
  list(count = real, z_score = z, p_value = p, rand_mean = mu,
       rand_sd = sd_, motif = motif, n_random = as.integer(n_random),
       seed = seed, preserve_mutual = isTRUE(preserve_mutual),
       method = if (isTRUE(preserve_mutual))
         paste("Milo et al. (2002) motif Z score / p-value",
               "(mfinder degree+mutual ensemble)")
       else paste("Milo et al. (2002) motif Z score / p-value",
                  "(in/out-degree ensemble)"))
}
