# SPDX-License-Identifier: AGPL-3.0-or-later

#' Spatial weights matrix from point coordinates
#'
#' The spatial weights matrix formalises "neighbour" for a spatial model:
#' \eqn{w_{ij}} is non-zero when \eqn{j} is a neighbour of \eqn{i} and
#' zero otherwise. Everything downstream --- the spatial lag \eqn{Wy},
#' Moran's I, a spatial autoregressive model --- is defined relative to
#' this choice, which is why the choice is an assumption rather than an
#' estimate.
#'
#' Three coordinate-based schemes are offered.
#' \describe{
#'   \item{\code{"distance"}}{Distance band: \eqn{w_{ij} = 1} when
#'     \eqn{0 < d_{ij} \le} \code{k_or_threshold}, else 0. A unit with no
#'     neighbour inside the band keeps an all-zero row.}
#'   \item{\code{"knn"}}{k nearest neighbours: \eqn{w_{ij} = 1} for the
#'     \code{k_or_threshold} units nearest to \eqn{i}. This relation is
#'     not symmetric.}
#'   \item{\code{"inverse"}}{Inverse distance:
#'     \eqn{w_{ij} = d_{ij}^{-\alpha}} for
#'     \eqn{0 < d_{ij} \le} \code{k_or_threshold}, else 0. The threshold
#'     may be \code{Inf}, which connects every pair.}
#' }
#'
#' Distances are Euclidean and the diagonal is always zero: a unit is not
#' its own neighbour. With \code{row_standardize} the rows that have at
#' least one neighbour are divided by their sum, so a spatial lag is a
#' weighted average of neighbours; all-zero rows are left alone.
#'
#' Contiguity weights (rook, queen) are not constructed here. They need a
#' lattice or polygon topology rather than points --- two units are
#' rook-contiguous when their boundaries share an edge, which a
#' coordinate pair does not determine.
#'
#' Mirrors \code{morie.fn.wmtwgt} on the Python side.
#'
#' @param coords Numeric matrix of \eqn{(n, d)} coordinates, one row per
#'   unit. A plain vector is read as \eqn{n} points on a line.
#' @param method One of \code{"distance"}, \code{"knn"},
#'   \code{"inverse"}.
#' @param k_or_threshold Distance band for \code{"distance"} and
#'   \code{"inverse"}, or the number of neighbours \eqn{k} for
#'   \code{"knn"}.
#' @param alpha Distance decay exponent for \code{"inverse"}. Default 1.
#' @param row_standardize Divide each non-empty row by its sum. Default
#'   \code{TRUE}.
#' @return Named list with \code{weights}, \code{n}, \code{scheme},
#'   \code{row_standardized}, \code{n_links}, \code{n_islands},
#'   \code{pct_nonzero}, \code{method}.
#' @references Anselin L (1988). \emph{Spatial Econometrics: Methods and
#'   Models}. Kluwer, Dordrecht, Chapter 3.
#' @examples
#' Wmtwgt(cbind(c(0, 1, 2, 3), c(0, 0, 1, 1)), "knn", 2)$n_links
#' @export
Wmtwgt <- function(coords, method = "distance", k_or_threshold = 1,
                   alpha = 1, row_standardize = TRUE) {
  if (is.null(dim(coords))) {
    pts <- matrix(as.numeric(coords), ncol = 1L)
  } else {
    pts <- matrix(as.numeric(as.matrix(coords)), nrow = nrow(coords))
  }
  n <- nrow(pts)
  if (n < 2L) stop("need at least two units", call. = FALSE)
  method <- match.arg(method, c("distance", "knn", "inverse"))

  d <- matrix(0, n, n)
  for (i in seq_len(n)) {
    if (i == n) break
    for (j in seq.int(i + 1L, n)) {
      v <- sqrt(sum((pts[i, ] - pts[j, ])^2))
      d[i, j] <- v
      d[j, i] <- v
    }
  }

  w <- matrix(0, n, n)
  if (method == "knn") {
    k <- as.integer(k_or_threshold)
    if (is.na(k) || k < 1L || k > n - 1L) {
      stop("k must lie between 1 and n - 1", call. = FALSE)
    }
    for (i in seq_len(n)) {
      others <- setdiff(seq_len(n), i)
      ## ties broken by index, matching the Python arm
      ord <- others[order(d[i, others], others)]
      w[i, ord[seq_len(k)]] <- 1
    }
  } else {
    thr <- as.numeric(k_or_threshold)[1L]
    if (is.na(thr) || thr <= 0) {
      stop("threshold must be positive", call. = FALSE)
    }
    alpha <- as.numeric(alpha)[1L]
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) next
        dij <- d[i, j]
        if (dij <= 0 || dij > thr) next
        w[i, j] <- if (method == "distance") 1 else dij^(-alpha)
      }
    }
  }

  n_links <- 0L
  n_islands <- 0L
  for (i in seq_len(n)) {
    nz <- w[i, ] != 0
    cnt <- sum(nz)
    n_links <- n_links + as.integer(cnt)
    if (cnt == 0L) {
      n_islands <- n_islands + 1L
    } else if (isTRUE(row_standardize)) {
      w[i, nz] <- w[i, nz] / sum(w[i, nz])
    }
  }

  list(weights = w,
       n = n,
       scheme = method,
       row_standardized = isTRUE(row_standardize),
       n_links = n_links,
       n_islands = n_islands,
       pct_nonzero = 100 * n_links / (n * n),
       method = "Spatial weights matrix (Anselin 1988, ch.3)")
}
