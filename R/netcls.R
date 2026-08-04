# SPDX-License-Identifier: AGPL-3.0-or-later
#' Closeness centrality of a node (re-export).
#'
#' Formula: see clocent
#'
#' @param A Adjacency matrix.

#' @return List with the payload of :func:`morie.fn.clocen.clocent`.
#' @references Sabidussi (1966) for the sum-distance form and Freeman (1979), Centrality in social networks: conceptual clarification, Social Networks 1:215-239, for the (n-1)-normalised measure. Freeman's article is paywalled; the normalisation C(v) = (n-1)/sum_u d(v,u) is as restated in the centrality literature that cites him.
#' @export
Netclocent <- function(A) {
  Clocent(A)
}
