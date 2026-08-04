# SPDX-License-Identifier: AGPL-3.0-or-later
#' Eccentricity centrality (Hage and Harary 1995)
#'
#' The eccentricity of a vertex is its greatest geodesic distance to any other
#' vertex, e(v) = max_u d(v, u); the eccentricity centrality is 1 / e(v).  The
#' minimum eccentricity is the radius, the maximum the diameter.  Source
#' consulted: Hage and Harary (1995), Eccentricity and centrality in networks,
#' Social Networks 17, 57-63.
#'
#' @param A square adjacency matrix; non-zero entries are edges.
#' @param node optional 0-based vertex index to report; mean if NULL.
#' @return list: estimate, ecc, centrality, eccentricity, radius, diameter,
#'   centre, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4, byrow = TRUE)
#' ecccen(A, node = 1)
#' @export
ecccen <- function(A, node = NULL) {
  A <- as.matrix(A); n <- nrow(A)
  e <- numeric(n)
  for (v in seq_len(n)) e[v] <- max(t3bfs(A, v))
  cent <- ifelse(e > 0, 1 / e, Inf)
  radius <- min(e); diameter <- max(e)
  centre <- which(e == radius) - 1L
  if (is.null(node)) { est <- mean(cent); ecc <- mean(e) }
  else { est <- cent[node + 1L]; ecc <- e[node + 1L] }
  list(estimate = as.numeric(est), ecc = as.numeric(ecc), centrality = cent,
       eccentricity = e, radius = as.numeric(radius),
       diameter = as.numeric(diameter), centre = as.integer(centre),
       n = as.integer(n),
       method = "Eccentricity centrality (Hage & Harary 1995)")
}

# CANONICAL TEST
# A <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4, byrow = TRUE)
# r <- ecccen(A, node = 1); stopifnot(abs(r$ecc - 2) < 1e-12, abs(r$radius - 2) < 1e-12)

#' @rdname ecccen
#' @keywords internal
#' @export
morie_eccentricity_centrality <- ecccen
