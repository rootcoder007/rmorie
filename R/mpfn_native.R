# Message passing neural networks: one framework, eight models.
# Sources: Gilmer, J. et al. (2017), "Neural Message Passing for
# Quantum Chemistry", ICML 2017, arXiv:1704.01212; Li, Y. et al.
# (2016), "Gated Graph Sequence Neural Networks", ICLR 2016,
# arXiv:1511.05493; Vinyals, O. et al. (2016), "Order Matters:
# Sequence to sequence for sets", ICLR 2016, arXiv:1511.06391.
#
# Native implementation mirroring Python morie.fn.mpfn exactly: the
# same eq. (1) message phase, the same GRU update with weights tied
# across steps, the same three readouts, and the same
# permutation-invariance check.

.GHC_MPFN_EPS <- 1e-12
.GHC_MPFN_READOUTS <- c("sum", "mean", "gated")

#' @keywords internal
#' @noRd
.ghc_mpfn_sig <- function(x) {
  if (x > -700) 1 / (1 + exp(-x)) else 0
}

#' Eq. (1)'s M_t
#'
#' @param h_v Source node state (unused; kept for API symmetry).
#' @param h_w Neighbour state.
#' @param e_vw Edge feature (scalar when A is NULL).
#' @param A Optional edge network: a function of e_vw returning a matrix.
#' @return A message vector.
#' @export
morie_mpfn_message <- function(h_v, h_w, e_vw, A = NULL) {
  hw <- as.numeric(h_w)
  if (is.null(A)) {
    e <- if (is.list(e_vw) || length(e_vw) > 1L) as.numeric(e_vw)[1]
         else as.numeric(e_vw)
    return(e * hw)
  }
  M <- as.matrix(A(e_vw))
  as.numeric(M %*% hw)
}

#' U_t as a GRU
#'
#' @param h Current state.
#' @param m Message.
#' @param Wz,Uz,Wr,Ur,Wh,Uh Update and reset gate and candidate
#'   projections.
#' @return The updated state.
#' @export
morie_mpfn_update_gru <- function(h, m, Wz, Uz, Wr, Ur, Wh, Uh) {
  h <- as.numeric(h); m <- as.numeric(m)
  Wz <- as.matrix(Wz); Uz <- as.matrix(Uz)
  Wr <- as.matrix(Wr); Ur <- as.matrix(Ur)
  Wh <- as.matrix(Wh); Uh <- as.matrix(Uh)
  n <- length(h)
  lin <- function(W, U, a, b) {
    as.numeric(W %*% a + U %*% b)
  }
  z <- vapply(lin(Wz, Uz, m, h), .ghc_mpfn_sig, numeric(1))
  r <- vapply(lin(Wr, Ur, m, h), .ghc_mpfn_sig, numeric(1))
  hh <- tanh(lin(Wh, Uh, m, r * h))
  (1 - z) * h + z * hh
}

#' T rounds of eq. (1)
#'
#' @param H0 Initial states (n x d matrix).
#' @param adj Adjacency list (list of integer vectors).
#' @param edge_features Edge feature map.
#' @param T Number of steps.
#' @param A Optional edge network.
#' @param update Optional update function.
#' @return Final states.
#' @export
morie_mpfn_message_passing <- function(H0, adj, edge_features, T = 3L,
                                        A = NULL, update = NULL) {
  T <- as.integer(T)
  if (T < 1L) stop("mpfn: T must be at least 1")
  H <- apply(as.matrix(H0), 1, as.numeric)
  if (is.null(dim(H))) H <- matrix(H, nrow = 1)
  H <- t(H)
  for (t in seq_len(T)) {
    new <- matrix(0, nrow = nrow(H), ncol = ncol(H))
    for (v in seq_len(nrow(H)) - 1L) {
      nb <- sort(adj[[as.character(v)]])
      if (is.null(nb)) nb <- integer(0)
      m <- rep(0, ncol(H))
      for (w in nb) {
        e <- edge_features[[paste0(v, "_", w)]]
        if (is.null(e))
          e <- edge_features[[paste0(w, "_", v)]]
        if (is.null(e)) e <- 1
        mm <- morie_mpfn_message(H[v + 1L, ], H[w + 1L, ], e, A)
        m <- m + mm
      }
      new[v + 1L, ] <- if (!is.null(update)) update(H[v + 1L, ], m)
                       else H[v + 1L, ] + m
    }
    H <- new
  }
  H
}

#' Readout R
#'
#' @param H n x d matrix.
#' @param how "sum", "mean" or "gated".
#' @param H0,i_fn,j_fn Required for the gated readout.
#' @return A vector of length d.
#' @export
morie_mpfn_readout <- function(H, how = "sum", H0 = NULL, i_fn = NULL,
                                j_fn = NULL) {
  if (!(how %in% .GHC_MPFN_READOUTS))
    stop(paste0("mpfn: readout must be one of ",
                paste(.GHC_MPFN_READOUTS, collapse = ", "), ", got ",
                how))
  Hm <- as.matrix(H)
  d <- ncol(Hm)
  if (how == "sum")
    return(as.numeric(colSums(Hm)))
  if (how == "mean")
    return(as.numeric(colSums(Hm) / nrow(Hm)))
  if (is.null(H0) || is.null(i_fn) || is.null(j_fn))
    stop("mpfn: the gated readout needs H0, i_fn and j_fn")
  acc <- rep(0, d)
  for (v in seq_len(nrow(Hm))) {
    g <- i_fn(Hm[v, ], as.numeric(H0[v, ]))
    jv <- j_fn(Hm[v, ])
    acc <- acc + vapply(g, .ghc_mpfn_sig, numeric(1)) * as.numeric(jv)
  }
  acc
}

#' Check permutation invariance of a graph-level prediction
#'
#' @param H Initial states.
#' @param adj Adjacency list.
#' @param edge_features Edge feature map keyed by "i_j" (integer pair).
#' @param perm 0-based permutation of node indices.
#' @param T,how,readout arguments.
#' @param tol Tolerance.
#' @return A list with invariant, max_deviation, readout.
#' @export
morie_mpfn_is_permutation_invariant <- function(H, adj, edge_features,
                                                 perm, T = 3L,
                                                 how = "sum",
                                                 tol = 1e-9) {
  base <- morie_mpfn_readout(morie_mpfn_message_passing(H, adj,
                                                          edge_features, T),
                              how)
  n <- length(H)
  inv <- integer(n)
  for (i in seq_len(n)) inv[perm[i] + 1L] <- i
  Hp <- H[inv + 1L]
  adjp <- list()
  for (v in seq_along(adj)) {
    vv <- as.integer(names(adj)[v])
    adjp[[as.character(perm[vv + 1L] + 1L)]] <-
      sort(perm[adj[[v]] + 1L] + 1L)
  }
  efp <- list()
  for (nm in names(edge_features)) {
    ij <- as.integer(strsplit(nm, "_")[[1]])
    efp[[paste0(perm[ij[1] + 1L] + 1L, "_",
                perm[ij[2] + 1L] + 1L)]] <- edge_features[[nm]]
  }
  other <- morie_mpfn_readout(morie_mpfn_message_passing(Hp, adjp, efp, T),
                              how)
  dev <- max(abs(base - other))
  list(invariant = dev < as.numeric(tol),
       max_deviation = dev, readout = base)
}

morie_mpfn <- morie_mpfn_message_passing
morie_mpfn_messagepassing <- morie_mpfn_message_passing
