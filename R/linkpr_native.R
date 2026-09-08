# SPDX-License-Identifier: AGPL-3.0-or-later
#' Link prediction scores: common neighbours, Adamic-Adar, resource allocation
#'
#' Neighbourhood scores for a node pair (u, v). With Gamma(x) the
#' neighbour set of x (nonzero entries of row x, self excluded):
#' common neighbours \eqn{CN = |\Gamma(u) \cap \Gamma(v)|}; Adamic-Adar
#' \eqn{AA = \sum_z 1/\log|\Gamma(z)|}; resource allocation
#' \eqn{RA = \sum_z 1/|\Gamma(z)|}, the sums over the common
#' neighbours z. The graph is treated as unweighted and undirected.
#'
#' @param G Adjacency matrix, n by n.
#' @param u,v Node indices (1-based).
#' @param method One of cn, aa, ra, all.
#' @return List with the requested scores, common_neighbours (1-based),
#'   estimate, u, v, n.
#' @references Liben-Nowell, D. and Kleinberg, J. (2007). The
#'   link-prediction problem for social networks. Journal of the
#'   American Society for Information Science and Technology, 58(7),
#'   1019-1031, Sec. 2. Archived:
#'   fetched-wave3/libennowell-kleinberg-2007-link-prediction.pdf.
#'
#'   Zhou, T., Lu, L. and Zhang, Y.-C. (2009). Predicting missing links
#'   via local information. European Physical Journal B, 71, 623-630,
#'   eq. (2). Archived:
#'   fetched-wave3/zhou-2009-resource-allocation-link-prediction.pdf.
#' @examples
#' A <- matrix(0, 4, 4); A[1, 2] <- A[2, 1] <- 1
#' A[1, 3] <- A[3, 1] <- 1; A[2, 4] <- A[4, 2] <- 1; A[3, 4] <- A[4, 3] <- 1
#' Linkpr(A, 1, 4)
#' @export
Linkpr <- function(G, u, v, method = "all") {
  A <- as.matrix(G)
  n <- nrow(A)
  if (ncol(A) != n) stop("G must be square")
  u <- as.integer(u)
  v <- as.integer(v)
  if (u < 1L || u > n || v < 1L || v > n) stop("u, v must be valid node indices")
  method <- tolower(as.character(method))
  if (!method %in% c("cn", "aa", "ra", "all")) stop("method must be one of cn, aa, ra, all")
  nbr <- lapply(seq_len(n), function(i) setdiff(which(A[i, ] != 0), i))
  common <- sort(intersect(nbr[[u]], nbr[[v]]))
  deg <- vapply(nbr, length, 0L)
  cn <- as.numeric(length(common))
  aa <- 0
  ra <- 0
  for (z in common) {
    if (deg[z] > 1L) aa <- aa + 1 / log(deg[z])
    ra <- ra + 1 / deg[z]
  }
  scores <- list(cn = cn, aa = aa, ra = ra)
  out <- list(common_neighbours = common, u = u, v = v, n = n,
              method = "Liben-Nowell-Kleinberg CN/AA + Zhou RA link prediction")
  if (method == "all") {
    out <- c(out, scores)
    out$estimate <- cn
  } else {
    out[[method]] <- scores[[method]]
    out$estimate <- scores[[method]]
  }
  out
}

#' @rdname Linkpr
#' @export
link_prediction <- Linkpr
