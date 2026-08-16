# dmlqs -- D-MPNN: messages on DIRECTED bonds (Yang et al. 2019).
#
# Sources:
#   Yang, K., Swanson, K., Jin, W., Coley, C., Eiden, P., Gao, H.,
#   Guzman-Perez, A., Hopper, T., Kelley, B., Mathea, M., Palmer, A.,
#   Settels, V., Jaakkola, T., Jensen, K. & Barzilay, R. (2019)
#   "Analyzing Learned Molecular Representations for Property
#   Prediction", *Journal of Chemical Information and Modeling* 59(8),
#   3370-3388, doi:10.1021/acs.jcim.9b00237, arXiv:1904.01561. Messages
#   on DIRECTED bonds excluding the reverse edge -- the single
#   exclusion that prevents TOTTERS (paths v_1 v_2 ... v_n with
#   v_i = v_{i+2}); atom representations from incoming bond messages;
#   computed molecule-level descriptors concatenated with the learned
#   representation.
#   Gilmer, J., Schoenholz, S. S., Riley, P. F., Vinyals, O. & Dahl, G. E.
#   (2017) "Neural Message Passing for Quantum Chemistry", ICML 2017,
#   PMLR 70, 1263-1272, arXiv:1704.01212. The framework this paper
#   specialises.
#   Dai, H., Dai, B. & Song, L. (2016) "Discriminative Embeddings of
#   Latent Variable Models for Structured Data", ICML 2016, PMLR 48,
#   2702-2711, arXiv:1603.05629. structure2vec, the directed variant
#   this paper renames D-MPNN.
#
# Native R only, mirroring morie.fn.dmlqs in the same order, so the
# three-way parity harness can assert agreement at 1e-9 rather than at
# some looser tolerance. No external packages. Private helpers are
# prefixed .dmlqs_ to keep R/ shared-environment namespacing clean.

.dmlqs_edge_key <- function(v, w) paste(as.character(v), as.character(w),
                                       sep = ",")

.dmlqs_norm_adj <- function(adj) {
  # Coerce an adjacency mapping to a list keyed by character atom id,
  # each value a sorted unique integer vector of neighbours (with v
  # itself excluded -- mirrors Python `set(adj[v]) - {v}`).
  if (is.null(adj)) return(list())
  if (is.list(adj)) {
    keys <- names(adj)
    if (is.null(keys)) keys <- as.character(seq_along(adj))
    out <- list()
    for (k in seq_along(keys)) {
      key <- keys[k]
      v_int <- suppressWarnings(as.integer(key))
      nb <- adj[[k]]
      if (is.null(nb)) nb <- integer(0)
      nb <- as.integer(nb)
      nb <- nb[!is.na(nb) & nb != v_int]
      out[[as.character(v_int)]] <- sort(unique(nb))
    }
    return(out)
  }
  # matrix: rows = source, cols = target; non-zero entries are edges.
  if (is.matrix(adj)) {
    out <- list()
    nr <- nrow(adj)
    for (v in seq_len(nr)) {
      nb <- which(as.numeric(adj[v, ]) != 0)
      nb <- as.integer(nb[nb != v])
      out[[as.character(v)]] <- sort(unique(nb))
    }
    return(out)
  }
  stop("dmlqs: adj must be a named list, named integer vector, or matrix")
}

.dmlqs_directed_edges <- function(adj) {
  A <- .dmlqs_norm_adj(adj)
  out <- list()
  if (length(A) == 0L) return(out)
  vs <- sort(names(A))
  for (v in vs) {
    v_int <- as.integer(v)
    nb <- A[[v]]
    ws <- sort(setdiff(nb, v_int))
    for (w in ws) out[[length(out) + 1L]] <- c(v_int, as.integer(w))
  }
  out
}

.dmlqs_count_totters <- function(adj, length = 3L, exclude_reverse = TRUE) {
  L <- as.integer(length)
  if (L < 3L) {
    stop("dmlqs: a totter needs at least 3 steps")
  }
  A <- .dmlqs_norm_adj(adj)
  excl <- isTRUE(as.logical(exclude_reverse))
  vs <- if (length(A) > 0L) sort(names(A)) else character(0)

  state <- new.env(parent = emptyenv())
  state$paths <- 0L
  state$tot <- 0L

  walk <- function(path) {
    if (length(path) == L) {
      state$paths <- state$paths + 1L
      has_tot <- FALSE
      for (i in seq_len(L - 2L)) {
        if (path[i] == path[i + 2L]) {
          has_tot <- TRUE
          break
        }
      }
      if (has_tot) state$tot <- state$tot + 1L
      return(invisible(NULL))
    }
    v <- path[length(path)]
    nb <- if (is.null(A[[as.character(v)]])) integer(0) else A[[as.character(v)]]
    nb <- sort(setdiff(nb, v))
    for (w in nb) {
      if (excl && length(path) >= 2L && w == path[length(path) - 1L]) next
      walk(c(path, w))
    }
    invisible(NULL)
  }

  for (s in vs) walk(c(as.integer(s)))

  paths <- state$paths
  tot <- state$tot
  frac <- if (paths > 0L) as.numeric(tot) / as.numeric(paths) else 0.0
  list(paths = paths, totters = tot,
       fraction = frac, excluded_reverse = excl)
}

.dmlqs_act <- function(x, activation) {
  act <- as.character(activation)
  if (act == "relu") return(pmax(0, as.numeric(x)))
  if (act == "tanh") return(tanh(as.numeric(x)))
  stop(sprintf("dmlqs: activation must be relu or tanh, got %s", act))
}

.dmlqs_message_pass <- function(h0, adj, T = 3L, W = NULL,
                                activation = "relu",
                                exclude_reverse = TRUE) {
  if (is.null(h0) || length(h0) == 0L) {
    stop("dmlqs: h0 must be a non-empty mapping of directed edges to vectors")
  }
  A <- .dmlqs_norm_adj(adj)
  excl <- isTRUE(as.logical(exclude_reverse))
  Ts <- as.integer(T)
  if (Ts < 1L) stop("dmlqs: T must be at least 1")
  act <- as.character(activation)

  nm <- names(h0)
  if (is.null(nm)) {
    stop("dmlqs: h0 must be a named mapping keyed by 'v,w'")
  }
  H <- list()
  for (i in seq_along(h0)) {
    parts <- strsplit(nm[i], ",", fixed = TRUE)[[1]]
    if (length(parts) != 2L) {
      stop(sprintf("dmlqs: h0 key '%s' is not 'v,w'", nm[i]))
    }
    v <- as.integer(parts[1L]); w <- as.integer(parts[2L])
    H[[.dmlqs_edge_key(v, w)]] <- as.numeric(h0[[i]])
  }
  d <- length(H[[1L]])

  H0 <- H

  hasW <- !is.null(W)
  if (hasW) {
    if (is.matrix(W)) {
      Wm <- W
      storage.mode(Wm) <- "double"
      if (nrow(Wm) != d || ncol(Wm) != d) {
        stop(sprintf("dmlqs: W must be a %d x %d matrix", d, d))
      }
    } else if (is.list(W)) {
      # list of rows: W[[o]] is the o-th row vector
      Wm <- matrix(as.numeric(unlist(W)), nrow = d, ncol = d, byrow = TRUE)
    } else {
      # assume flat vector in row-major order (mirrors Python W[o][a])
      Wm <- matrix(as.numeric(W), nrow = d, ncol = d, byrow = TRUE)
    }
  } else {
    Wm <- NULL
  }

  for (t in seq_len(Ts)) {
    new <- list()
    keys <- names(H)
    for (k in seq_along(keys)) {
      parts <- strsplit(keys[k], ",", fixed = TRUE)[[1]]
      v <- as.integer(parts[1L]); w <- as.integer(parts[2L])
      m <- rep(0.0, d)
      nb <- A[[as.character(v)]]
      nb <- if (is.null(nb)) integer(0) else nb
      nb <- sort(setdiff(nb, v))
      for (u in nb) {
        if (excl && u == w) next
        ukey <- .dmlqs_edge_key(u, v)
        if (!is.null(H[[ukey]])) {
          m <- m + H[[ukey]]
        }
      }
      h0vw <- H0[[keys[k]]]
      if (hasW) {
        Wm_v <- as.numeric(Wm %*% m)
        new[[keys[k]]] <- .dmlqs_act(h0vw + Wm_v, act)
      } else {
        new[[keys[k]]] <- .dmlqs_act(h0vw + m, act)
      }
    }
    H <- new
  }

  list(edge_states = H, T = Ts,
       excluded_reverse = excl,
       note = "the exclusion of the reverse edge IS the anti-tottering mechanism")
}

.dmlqs_atom_readout <- function(edge_states, adj, n) {
  N <- as.integer(n)
  A <- .dmlqs_norm_adj(adj)
  out <- list()
  if (is.null(edge_states) || length(edge_states) == 0L) {
    for (v in seq_len(N)) out[[v]] <- numeric(0)
    return(out)
  }
  d <- length(edge_states[[1L]])
  for (v in seq_len(N)) {
    acc <- rep(0.0, d)
    nb <- A[[as.character(v)]]
    nb <- if (is.null(nb)) integer(0) else nb
    nb <- sort(setdiff(nb, v))
    for (u in nb) {
      ukey <- .dmlqs_edge_key(u, v)
      if (!is.null(edge_states[[ukey]])) {
        acc <- acc + edge_states[[ukey]]
      }
    }
    out[[v]] <- acc
  }
  out
}

.dmlqs_concat_descriptors <- function(learned, descriptors) {
  a <- as.numeric(learned)
  b <- as.numeric(descriptors)
  list(estimate = c(a, b),
       representation = c(a, b),
       learned_dim = length(a),
       descriptor_dim = length(b),
       method = "D-MPNN representation with computed features; Yang et al. (2019)")
}

.dmlqs_cheatsheet <- function() {
  paste("dmlqs: pass messages along DIRECTED BONDS, not atoms. The ",
        "stated reason is TOTTERS -- paths v1 v2 ... vn with ",
        "v_i = v_{i+2}, where a message goes A -> B and comes ",
        "straight back carrying A's own information as news, ",
        "adding noise. The mechanism is one exclusion: ",
        "m_vw = sum over u in N(v) EXCLUDING w. Atom ",
        "representations are formed at the end from incoming bond ",
        "messages, and computed molecule-level descriptors are ",
        "concatenated with the learned representation -- expert ",
        "features and learned ones are not rivals.")
}

# The entry point: dispatch on a `what` argument to mirror the Python
# module-level functions. Defaults to the message pass to match the
# "deepml_qsar" / "directedmpnn" aliases.
#' The entry point: dispatch on a `what` argument to mirror the Python
#'
#' module-level functions. Defaults to the message pass to match the
#' "deepml_qsar" / "directedmpnn" aliases.
#'
#' @param what Defaults to \code{"dmpnn_message_pass"}.
#' @param ... Passed through.
#' @return Nothing; this branch always raises.
#' @export
morie_dmlqs <- function(what = "dmpnn_message_pass", ...) {
  w <- as.character(what)
  if (w == "directed_edges" || w == "directededges") {
    return(.dmlqs_directed_edges(...))
  }
  if (w == "count_totters" || w == "counttotters") {
    return(.dmlqs_count_totters(...))
  }
  if (w == "dmpnn_message_pass" || w == "dmpnn_messagepass" ||
      w == "directedmpnn" || w == "deepml_qsar" || w == "deepmlqsar" ||
      w == "message_pass" || w == "messagepass") {
    return(.dmlqs_message_pass(...))
  }
  if (w == "atom_readout" || w == "atomreadout") {
    return(.dmlqs_atom_readout(...))
  }
  if (w == "concat_descriptors" || w == "concatdescriptors") {
    return(.dmlqs_concat_descriptors(...))
  }
  if (w == "cheatsheet") {
    return(.dmlqs_cheatsheet())
  }
  stop(sprintf("dmlqs: unknown function '%s'", w))
}
