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
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1 / (1 + exp(-xc))
}

#' Eq. (1)'s M_t
#'
#' @param h_v Source node state (unused; kept for API symmetry).
#' @param h_w Neighbour state.
#' @param e_vw Edge feature (scalar when A is NULL).
#' @param A Optional edge network: a function of e_vw returning a matrix.
#' @return A message vector.
#' @export
#' @examples
#' morie_mpfn_message(h_v = c(1, 2, 3, 4, 5, 6, 7, 8), h_w = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   e_vw = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
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
#' @keywords internal
morie_mpfn_update_gru <- function(h, m, Wz, Uz, Wr, Ur, Wh, Uh) {
  h <- as.numeric(h)
  m <- as.numeric(m)
  Wz <- as.matrix(Wz)
  Uz <- as.matrix(Uz)
  Wr <- as.matrix(Wr)
  Ur <- as.matrix(Ur)
  Wh <- as.matrix(Wh)
  Uh <- as.matrix(Uh)
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
#' @examples
#' morie_mpfn_message_passing(H0 = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)),
#'   edge_features = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mpfn_readout(V)
#' @keywords internal
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
#' @param T,how arguments.
#' @param tol Tolerance.
#' @return A list with invariant, max_deviation, readout.
#' @export
#' @examples
#' morie_mpfn_is_permutation_invariant(H = 0.5,
#'   adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)),
#'   edge_features = c(1, 2, 3, 4, 5, 6, 7, 8), perm = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
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

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_cheatsheet
#'
#' A step of the mpfn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
mpfn_cheatsheet <- function() {
  paste(paste0(
    "mpfn: at least EIGHT published graph models are the same alg",
    "orithm with different M_t, U_t and R. Message phase: m_v = s",
    "um_{w in N(v)} M_t(h_v, h_w, e_vw), then h_v <- U_t(h_v, m_v",
    "); readout R over the final states. The sum makes messages p",
    "ermutation-invariant and the READOUT MUST BE TOO, or the gra",
    "ph prediction changes when atoms are renumbered. Edge featur",
    "es carry bond type -- without them a single and a double bon",
    "d between the same atoms are identical."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_is_permutation_invariant
#'
#' A step of the mpfn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H A vector; its length is taken and its elements indexed.
#' @param adj A vector; its length is taken and its elements indexed.
#' @param edge_features A vector; its length is taken and its elements indexed.
#' @param perm A vector; indexed elementwise.
#' @param T Passed to \code{mpfn_message_passing}. Defaults to \code{3}.
#' @param how Passed to \code{mpfn_readout}. Defaults to \code{"sum"}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.
#' @return A list with \code{invariant}, \code{max_deviation}, \code{readout}.
#' @export
mpfn_is_permutation_invariant <- function(H, adj, edge_features, perm, T = 3,
                                          how = "sum", tol = 1e-9) {
  base <- mpfn_readout(mpfn_message_passing(H, adj, edge_features, T), how)
  n <- length(H)
  inv <- integer(n)
  for (i in seq_len(n)) inv[perm[i]] <- i
  Hp <- lapply(seq_len(n), function(i) H[[inv[i]]])
  adjp <- list()
  for (v in seq_along(adj)) {
    key <- as.character(v - 1L)
    if (!is.null(adj[[key]])) {
      adjp[[as.character(perm[v - 1L])]] <- sort(sapply(adj[[key]], function(w) perm[w + 1L] - 1L))
    }
  }
  efp <- list()
  for (k in seq_along(edge_features)) {
    a <- as.integer(strsplit(names(edge_features)[k], ",")[[1]][1])
    b <- as.integer(strsplit(names(edge_features)[k], ",")[[1]][2])
    efp[[paste(perm[a + 1L] - 1L, perm[b + 1L] - 1L, sep = ",")]] <- edge_features[[k]]
  }
  other <- mpfn_readout(mpfn_message_passing(Hp, adjp, efp, T), how)
  dev <- max(abs(base - other))
  list(invariant = dev < as.numeric(tol), max_deviation = dev, readout = base)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_message
#'
#' A step of the mpfn_native implementation. Called by \code{mpfn_message_passing}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h_v Accepted by the signature and not used anywhere in the body.
#' @param h_w Coerced to numeric by the body, with \code{as.numeric}.
#' @param e_vw A vector; its length is taken and its elements indexed.
#' @param A Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A vector, from \code{as.numeric}.
#' @export
mpfn_message <- function(h_v, h_w, e_vw, A = NULL) {
  hw <- as.numeric(h_w)
  if (is.null(A)) {
    e <- if (is.numeric(e_vw) && length(e_vw) > 1L) e_vw[1] else as.numeric(e_vw)
    return(e * hw)
  }
  M <- A(e_vw)
  as.numeric(M %*% hw)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_message_passing
#'
#' A step of the mpfn_native implementation. Called by \code{mpfn_is_permutation_invariant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H0 Iterated over elementwise, with \code{lapply}.
#' @param adj A vector; indexed elementwise.
#' @param edge_features A vector; indexed elementwise.
#' @param T Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @param A Passed to \code{mpfn_message}.
#' @param update Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{H}, as built in the body.
#' @export
mpfn_message_passing <- function(H0, adj, edge_features, T = 3, A = NULL,
                                 update = NULL) {
  H <- lapply(H0, as.numeric)
  if (as.integer(T) < 1L) stop("mpfn: T must be at least 1")
  for (step in seq_len(as.integer(T))) {
    new <- list()
    for (v in seq_along(H)) {
      nb <- if (!is.null(adj[[as.character(v)]])) adj[[as.character(v)]] else integer(0)
      m <- rep(0, length(H[[v]]))
      for (w in nb) {
        e <- edge_features[[paste(v, w, sep = ",")]]
        if (is.null(e)) e <- edge_features[[paste(w, v, sep = ",")]]
        if (is.null(e)) e <- 1
        mm <- mpfn_message(H[[v]], H[[w]], e, A)
        m <- m + mm
      }
      if (!is.null(update)) {
        new[[v]] <- update(H[[v]], m)
      } else {
        new[[v]] <- H[[v]] + m
      }
    }
    H <- new
  }
  H
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_readout
#'
#' A step of the mpfn_native implementation. Called by \code{mpfn_is_permutation_invariant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Iterated over elementwise, with \code{lapply}.
#' @param how One of \code{"gated"}, \code{"mean"}, \code{"sum"}. Defaults to \code{"sum"}.
#' @param H0 Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param i_fn The body requires: mpfn: the gated readout needs H0, i_fn and j_fn.
#' @param j_fn The body requires: mpfn: the gated readout needs H0, i_fn and j_fn.
#' @return The value of \code{acc}, as built in the body.
#' @export
mpfn_readout <- function(H, how = "sum", H0 = NULL, i_fn = NULL, j_fn = NULL) {
  if (!(how %in% c("sum", "mean", "gated"))) {
    stop(sprintf("mpfn: readout must be one of sum, mean, gated, got %s", how))
  }
  rows <- lapply(H, as.numeric)
  d <- length(rows[[1]])
  if (how == "sum") {
    out <- rep(0, d)
    for (r in rows) out <- out + r
    return(out)
  }
  if (how == "mean") {
    out <- rep(0, d)
    for (r in rows) out <- out + r
    return(out / length(rows))
  }
  if (is.null(H0) || is.null(i_fn) || is.null(j_fn)) {
    stop("mpfn: the gated readout needs H0, i_fn and j_fn")
  }
  acc <- rep(0, d)
  for (v in seq_along(rows)) {
    g <- i_fn(rows[[v]], as.numeric(H0[[v]]))
    jv <- j_fn(rows[[v]])
    acc <- acc + sapply(g, mpfn_sig) * jv
  }
  acc
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_sig
#'
#' A step of the mpfn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
mpfn_sig <- function(x) {
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1 / (1 + exp(-xc))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mpfn_update_gru
#'
#' A step of the mpfn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h A vector; its length is taken.
#' @param m Passed to \code{lin}.
#' @param Wz Passed to \code{lin}.
#' @param Uz Passed to \code{lin}.
#' @param Wr Passed to \code{lin}.
#' @param Ur Passed to \code{lin}.
#' @param Wh Passed to \code{lin}.
#' @param Uh Passed to \code{lin}.
#' @return A numeric value.
#' @export
mpfn_update_gru <- function(h, m, Wz, Uz, Wr, Ur, Wh, Uh) {
  n <- length(h)
  lin <- function(W, U, a, b) {
    as.numeric(W %*% a + U %*% b)
  }
  z <- sapply(lin(Wz, Uz, m, h), mpfn_sig)
  r <- sapply(lin(Wr, Ur, m, h), mpfn_sig)
  hh <- tanh(lin(Wh, Uh, m, r * h))
  (1 - z) * h + z * hh
}
