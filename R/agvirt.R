# SPDX-License-Identifier: AGPL-3.0-or-later
#' Virtual loss for parallel Monte-Carlo tree search.
#'
#' N' = N + nvl * pending; W' = W - nvl * pending; Q' = W'/N'.
#'
#' @param W Accumulated action values (total, not mean).
#' @param N Visit counts.
#' @param pending Threads currently inside each child.
#' @param nvl Virtual losses charged per in-flight thread.
#'
#' @return List with Q, N, W, Qclean, k, nvl.
#' @references Chaslot, Winands and van den Herik (2008), Computers and
#'   Games 2008, LNCS 5131, 60-71, Sect. 3.3, read from the authors' own
#'   PDF.  They describe the rule qualitatively; the arithmetic here is
#'   its counter form and nothing further is claimed.
#' @export
#' @examples
#' Virtloss(W = c(1, 2, 3, 4, 5, 6, 7, 8), N = c(1, 2, 3, 4, 5, 6, 7, 8), pending = c(1, 2, 3, 4, 5, 6, 7, 8))
Virtloss <- function(W, N, pending, nvl = 1) {
  W <- .t1_vec(W); N <- .t1_vec(N); P <- .t1_vec(pending)
  k <- length(W)
  if (length(N) != k || length(P) != k)
    stop("W, N and pending must have the same length")
  if (any(N < 0) || any(P < 0)) stop("counts must be non-negative")
  nvl <- as.numeric(nvl)
  Nv <- N + nvl * P
  Wv <- W - nvl * P
  .t1_result(Q = ifelse(Nv == 0, 0, Wv / Nv), N = Nv, W = Wv,
             Qclean = ifelse(N == 0, 0, W / N), k = k, nvl = nvl,
             method = "Virtual loss in parallel MCTS (Chaslot et al. 2008 Sect. 3.3)")
}
