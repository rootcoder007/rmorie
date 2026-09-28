.idw_aniso <- function(P, angle, ratio) {
  P <- as.matrix(P)
  if (ratio == 1) return(P)
  X <- cbind(cos(angle) * P[, 1] + sin(angle) * P[, 2], (-sin(angle) * P[, 1] + cos(angle) * P[, 2]) / ratio)
  if (ncol(P) > 2) X <- cbind(X, P[, -(1:2), drop = FALSE])
  X
}

#' Inverse distance weighting, cross-validation and modified Shepard weights
#'
#' \code{IdwPredict}: \eqn{\sum w z / \sum w}, \eqn{w = d^{-power}}, over the
#' \code{nmax} nearest within \code{maxdist}, exact at data points, block
#' averages of point predictions and optional geometric anisotropy, with the
#' weighted variance, as \code{gstat::idw} (Shepard 1968).
#' \code{IdwCv}: leave-one-out or k-fold cross-validation
#' (\code{gstat::krige.cv}). \code{IdwPowerSearch}: LOO RMSE over powers.
#' \code{ShepardPredict}: Franke and Nielson (1980) weights with fixed or
#' k-th-neighbour radius. Identical to the Python arm
#' \code{morie.fn.idwinterp}.
#'
#' @param z Observations.
#' @param coords,new_coords Coordinate matrices.
#' @param power Distance power.
#' @param nmax Maximum neighbours.
#' @param maxdist Maximum distance.
#' @param block Block offsets (matrix).
#' @param angle,ratio Anisotropy angle (radians) and minor/major ratio.
#' @param folds Fold labels.
#' @param powers Candidate powers.
#' @param radius Shepard radius.
#' @param k Neighbour order of the adaptive radius.
#' @return List.
#' @references Shepard, D. (1968). A two-dimensional interpolation function
#'   for irregularly-spaced data. Proceedings of the 23rd ACM National
#'   Conference, 517-524.
#'
#'   Franke, R. and Nielson, G. (1980). Smooth interpolation of large sets of
#'   scattered data. International Journal for Numerical Methods in
#'   Engineering 15, 1691-1704.
#' @examples
#' IdwPredict(c(1, 3), rbind(c(0, 0), c(2, 0)), rbind(c(0.5, 0)))
#' ShepardPredict(c(1, 3), rbind(c(0, 0), c(2, 0)), rbind(c(0.5, 0)), radius = 3)$prediction
#' @export
IdwPredict <- function(z, coords, new_coords, power = 2, nmax = NULL, maxdist = NULL, block = NULL, angle = 0,
                       ratio = 1) {
  P <- .idw_aniso(coords, angle, ratio)
  Q <- .idw_aniso(new_coords, angle, ratio)
  offs <- if (is.null(block)) matrix(0, 1, ncol(P)) else .idw_aniso(block, angle, ratio)
  point <- function(q) {
    d <- sqrt(colSums((t(P) - q)^2))
    idx <- order(d)
    if (!is.null(maxdist)) idx <- idx[d[idx] <= maxdist]
    if (!is.null(nmax)) idx <- utils::head(idx, nmax)
    if (!length(idx)) return(c(NaN, NaN))
    if (any(d[idx] == 0)) return(c(z[idx[d[idx] == 0][1]], 0))
    w <- d[idx]^(-power)
    zh <- sum(w * z[idx]) / sum(w)
    c(zh, sum(w * (z[idx] - zh)^2) / sum(w))
  }
  res <- t(vapply(seq_len(nrow(Q)), function(i) {
    v <- vapply(seq_len(nrow(offs)), function(o) point(Q[i, ] + offs[o, ]), numeric(2))
    rowMeans(matrix(v, 2))
  }, numeric(2)))
  list(prediction = res[, 1], variance = res[, 2])
}

#' @rdname IdwPredict
#' @export
IdwCv <- function(z, coords, power = 2, nmax = NULL, maxdist = NULL, folds = NULL) {
  P <- as.matrix(coords)
  n <- length(z)
  lab <- if (is.null(folds)) seq_len(n) else folds
  pred <- numeric(n)
  for (f in sort(unique(lab))) {
    o <- which(lab == f)
    pred[o] <- IdwPredict(z[-o], P[-o, , drop = FALSE], P[o, , drop = FALSE], power, nmax, maxdist)$prediction
  }
  res <- z - pred
  list(prediction = pred, residual = res, rmse = sqrt(mean(res^2)), mae = mean(abs(res)), me = mean(res))
}

#' @rdname IdwPredict
#' @export
IdwPowerSearch <- function(z, coords, powers = seq(0.5, 6, by = 0.5), nmax = NULL) {
  rm <- vapply(powers, function(p) IdwCv(z, coords, p, nmax)$rmse, 0)
  list(powers = powers, rmse = rm, best = powers[which.min(rm)])
}

#' @rdname IdwPredict
#' @export
ShepardPredict <- function(z, coords, new_coords, radius = NULL, k = NULL) {
  if (is.null(radius) == is.null(k)) stop("give exactly one of radius and k")
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  pred <- vapply(seq_len(nrow(Q)), function(i) {
    d <- sqrt(colSums((t(P) - Q[i, ])^2))
    if (any(d == 0)) return(z[which(d == 0)[1]])
    R <- if (!is.null(radius)) radius else sort(d)[min(k, length(d))] * (1 + 1e-12)
    w <- ifelse(d < R, ((R - d) / (R * d))^2, 0)
    if (sum(w) > 0) sum(w * z) / sum(w) else NaN
  }, 0)
  list(prediction = pred)
}
