# SPDX-License-Identifier: AGPL-3.0-or-later
#' Refine node colours until neighbourhoods stop distinguishing them
#'
#' The standard cheap check for non-isomorphism: different final colour
#' multisets prove the graphs differ. The converse fails -- regular graphs
#' of equal degree are indistinguishable -- and that failure is exactly
#' the expressiveness ceiling of message-passing graph networks.
#'
#' Determinism: each round hashes the sorted neighbour-colour multiset to
#' a canonical string and reassigns colours by first appearance, so no
#' hash-seed randomness enters.
#'
#' Formula: \code{h^(t+1)(v) = hash(h^t(v), {{h^t(u): u in N(v)}})}.
#'
#' @param A Adjacency.
#' @param labels0 Starting colours; all zero by default.
#' @param max_iter Refinement rounds.
#' @return List with \code{labels_t}, \code{estimate}, \code{history}, \code{n}.
#' @references Weisfeiler & Leman (1968) Nauchno-Tekh Inform 2(9):12-16;
#'   Xu, Hu, Leskovec & Jegelka (2019) ICLR.
#' @export
Sgtwlk <- function(A, labels0 = NULL, max_iter = 3) {
  Am <- as.matrix(A); n <- nrow(Am)
  lab <- if (is.null(labels0)) rep(0L, n) else as.integer(round(as.numeric(labels0)))
  hist <- length(unique(lab))
  for (it in seq_len(as.integer(max_iter))) {
    keys <- character(n)
    for (v in seq_len(n)) {
      nb <- sort(lab[setdiff(which(Am[v, ] != 0), v)])
      keys[v] <- paste0(lab[v], "|", paste(nb, collapse = ","))
    }
    ord <- unique(keys)
    lab <- match(keys, ord) - 1L
    hist <- c(hist, length(ord))
  }
  .t1_result(labels_t = lab, estimate = length(unique(lab)), history = hist,
             n = n, method = "Weisfeiler-Leman colour refinement")
}
