# SPDX-License-Identifier: AGPL-3.0-or-later
#' Configuration model with a prescribed degree sequence.
#'
#' Formula: attach half-edges (stubs) uniformly at random in pairs; every vertex ends with its requested degree
#'
#' @param degrees Requested degree for each vertex.
#' @param seed Seed of the shared minstd stream.

#' @return List with ``edges``, ``degree`` (realised), ``self_loops``, ``multi_edges``, ``n``.
#' @references Bender and Canfield (1978), The asymptotic number of labeled graphs with given degree sequences, JCTA 24:296-307; Molloy and Reed (1995), A critical point for random graphs with a given degree sequence, Random Structures and Algorithms 6:161-180. Neither is held locally; uniform stub pairing is the standard published construction.
#' @export
Configmodel <- function(degrees, seed = 1) {
  d <- as.integer(.t1_vec(degrees)); n <- length(d)
  if (any(d < 0)) stop("degrees must be non-negative")
  if (sum(d) %% 2 != 0) stop("sum of degrees must be even")
  stubs <- rep(seq_len(n), d)
  g <- .t1_lcg(seed)
  ea <- integer(0); eb <- integer(0); loops <- 0L
  while (length(stubs)) {
    i <- as.integer(g$unif() * length(stubs)) + 1L
    if (i > length(stubs)) i <- length(stubs)
    a <- stubs[i]; stubs <- stubs[-i]
    j <- as.integer(g$unif() * length(stubs)) + 1L
    if (j > length(stubs)) j <- length(stubs)
    b <- stubs[j]; stubs <- stubs[-j]
    if (a == b) loops <- loops + 1L
    ea <- c(ea, min(a, b)); eb <- c(eb, max(a, b))
  }
  key <- paste(ea, eb, sep = "-")
  tb <- table(key)
  dup <- tb[tb > 1]
  multi <- 0L
  if (length(dup)) for (k in names(dup)) {
    pr <- as.integer(strsplit(k, "-")[[1]])
    if (pr[1] != pr[2]) multi <- multi + as.integer(dup[[k]]) - 1L
  }
  real <- tabulate(c(ea, eb), nbins = n)
  .t1_result(edges = cbind(ea, eb) - 1L, degree = real, self_loops = loops,
             multi_edges = multi, n = n,
             method = "Configuration model (uniform stub pairing)")
}
