.sw_kernel <- function(z, name) {
  switch(name,
         gaussian = stats::dnorm(z),
         uniform = ifelse(z > 1, 0, 0.5),
         triangular = ifelse(z > 1, 0, 1 - z),
         epanechnikov = ifelse(z > 1, 0, 0.75 * (1 - z^2)),
         quartic = ifelse(z > 1, 0, 15 / 16 * (1 - z^2)^2))
}

#' Spatial weight matrix from point coordinates
#'
#' Neighbour graphs: \code{knn} (k nearest, ties by index, as
#' \code{spdep::knn2nb(knearneigh())}), \code{distance} (\eqn{d_{min} <
#' d_{ij} \le threshold}, \code{spdep::dnearneigh}), \code{gabriel} and
#' \code{relative} (\code{spdep::gabrielneigh}, \code{relativeneigh}) and
#' \code{delaunay}; valued weights \code{inverse} (\eqn{d^{-\alpha}} in the
#' band) and \code{kernel} (uniform, triangular, Epanechnikov, quartic or
#' Gaussian kernel of \eqn{d/h}, h adaptive k-th neighbour distance by
#' default). Coding styles of \code{spdep::nb2listw}: B, W, C, U, S
#' (Tiefelsdorf, Griffith and Boots 1999) and minmax. Also returns the
#' neighbour lists, islands, connected components, sparsity and the
#' constants S0, S1, S2.
#'
#' @param coords Coordinates (n x 2).
#' @param method \code{"knn"}, \code{"distance"}, \code{"inverse"},
#'   \code{"kernel"}, \code{"gabriel"}, \code{"relative"} or
#'   \code{"delaunay"}.
#' @param k Neighbours for knn and the adaptive bandwidth.
#' @param threshold Upper distance (default median inter-point distance).
#' @param row_standardize Row-standardise when \code{style} is \code{NULL}.
#' @param style Coding style.
#' @param d_min Lower distance.
#' @param alpha Inverse-distance power.
#' @param bandwidth Kernel bandwidth.
#' @param kernel Kernel name.
#' @return List with \code{W}, \code{neighbours}, \code{style},
#'   \code{n_islands}, \code{components}, \code{n_components},
#'   \code{sparsity}, \code{S0}, \code{S1}, \code{S2}, \code{symmetric},
#'   \code{mean_neighbours}.
#' @references Anselin L (1988). Spatial Econometrics: Methods and Models.
#'   Kluwer.
#'
#'   Tiefelsdorf M, Griffith D A, Boots B (1999). A variance-stabilizing
#'   coding scheme for spatial link matrices. Environment and Planning A 31,
#'   165-180.
#' @examples
#' SpatialWeights(rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.5)), "gabriel", style = "B")$neighbours
#' @export
SpatialWeights <- function(coords, method = "knn", k = 5L, threshold = NULL, row_standardize = TRUE, style = NULL,
                           d_min = 0, alpha = 1, bandwidth = NULL, kernel = "gaussian") {
  P <- as.matrix(coords)
  n <- nrow(P)
  D <- as.matrix(stats::dist(P))
  if (is.null(style)) style <- if (row_standardize) "W" else "B"
  G <- matrix(0, n, n)
  if (method %in% c("distance", "inverse") && is.null(threshold)) {
    dd <- sort(D[upper.tri(D)])
    threshold <- dd[(length(dd) - 1) %/% 2 + 1]
  }
  if (method == "knn") {
    for (i in seq_len(n)) {
      o <- order(D[i, ], seq_len(n))
      o <- o[o != i][seq_len(k)]
      G[i, o] <- 1
    }
  } else if (method %in% c("distance", "inverse")) {
    band <- D > d_min & D <= threshold
    diag(band) <- FALSE
    G[band] <- if (method == "distance") 1 else D[band]^(-alpha)
  } else if (method == "kernel") {
    for (i in seq_len(n)) {
      h <- if (is.null(bandwidth)) sort(D[i, -i])[k] else bandwidth
      j <- which(seq_len(n) != i & D[i, ] > 0)
      G[i, j] <- .sw_kernel(D[i, j] / h, kernel)
    }
  } else if (method %in% c("gabriel", "relative")) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        m <- setdiff(seq_len(n), c(i, j))
        blocked <- if (method == "gabriel") any(D[i, m]^2 + D[j, m]^2 < D[i, j]^2) else any(pmax(D[i, m], D[j, m]) < D[i, j])
        if (!blocked) G[i, j] <- G[j, i] <- 1
      }
    }
  } else if (method == "delaunay") {
    E <- DelaunayTriangulation(P)$edges
    G[E] <- 1
    G[E[, 2:1]] <- 1
  } else {
    stop("unknown method", call. = FALSE)
  }
  rows <- rowSums(G)
  eff <- n - sum(rows == 0)
  W <- switch(style,
              B = G,
              W = G / ifelse(rows > 0, rows, 1),
              C = G * eff / sum(G),
              U = G / sum(G),
              minmax = G / min(max(rows), max(colSums(G))),
              S = {
                q <- sqrt(rowSums(G^2))
                V <- G / ifelse(q > 0, q, 1)
                V * n / sum(V)
              },
              stop("unknown style", call. = FALSE))
  nbrs <- lapply(seq_len(n), function(i) which(W[i, ] != 0))
  lab <- rep(NA_integer_, n)
  comp <- 0L
  A <- (G != 0) | t(G != 0)
  for (s in seq_len(n)) {
    if (!is.na(lab[s])) next
    comp <- comp + 1L
    stack <- s
    lab[s] <- comp
    while (length(stack)) {
      a <- stack[1]
      stack <- stack[-1]
      new <- which(A[a, ] & is.na(lab))
      lab[new] <- comp
      stack <- c(stack, new)
    }
  }
  list(W = W, neighbours = nbrs, style = style, n_islands = sum(rows == 0), components = lab, n_components = comp,
       sparsity = sum(lengths(nbrs)) / (n * (n - 1)), S0 = sum(W), S1 = 0.5 * sum((W + t(W))^2),
       S2 = sum((rowSums(W) + colSums(W))^2), symmetric = isSymmetric(unname(W)), mean_neighbours = mean(lengths(nbrs)))
}
