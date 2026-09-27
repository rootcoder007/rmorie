# SPDX-License-Identifier: AGPL-3.0-or-later
.esl14_project <- function(x, pts) {
  best <- NULL
  acc <- 0
  for (k in seq_len(nrow(pts) - 1)) {
    a <- pts[k, ]
    d <- pts[k + 1, ] - a
    L2 <- sum(d^2)
    t <- if (L2 == 0) 0 else max(0, min(1, sum((x - a) * d) / L2))
    q <- a + t * d
    dist <- sum((x - q)^2)
    if (is.null(best) || dist < best$dist - 1e-15) best <- list(lambda = acc + t * sqrt(L2), dist = dist, point = q)
    acc <- acc + sqrt(L2)
  }
  best
}

#' Principal curves
#'
#' Hastie-Stuetzle algorithm (ESL eqs 14.61-14.62): from the first principal
#' component line, alternate smoothing each coordinate on the projection index
#' (cubic smoothing spline with the given penalty) and projecting the points
#' onto the fitted polyline, re-parametrised by arc length. The iteration can
#' settle into a small 2-cycle, so the defaults follow princurve
#' (thresh = 0.001, maxit = 10).
#'
#' @param X Data, N by p.
#' @param penalty Smoothing-spline penalty on the arc-length scale.
#' @param max_iter,tol Iteration controls.
#' @return Named list: lambda, fitted, curve, distance, distance_path,
#'   iterations, converged.
#' @references Hastie, T. & Stuetzle, W. (1989). JASA 84, 502-516.
#' @examples
#' th <- (0:39) / 39 * pi
#' morie_esl_principal_curve(cbind(cos(th), sin(th)), penalty = 0.05)$distance
#' @export
morie_esl_principal_curve <- function(X, penalty = 1, max_iter = 10, tol = 1e-3) {
  X <- as.matrix(X)
  N <- nrow(X)
  if (N < 4 || penalty < 0) stop("need at least 4 points and penalty >= 0", call. = FALSE)
  mu <- colMeans(X)
  v <- svd(sweep(X, 2, mu))$v[, 1]
  lam <- drop(sweep(X, 2, mu) %*% v)
  lo <- min(lam)
  lam <- lam - lo
  fitted <- outer(lam + lo, v) + matrix(mu, N, ncol(X), byrow = TRUE)
  dist <- sum((X - fitted)^2)
  path <- dist
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    fitted <- vapply(seq_len(ncol(X)), function(j) .esl9_spline_smooth(lam, X[, j], penalty, rep(1, N)), numeric(N))
    fitted <- matrix(fitted, N)
    o <- order(lam, seq_len(N))
    pts <- fitted[o, , drop = FALSE]
    keep <- c(TRUE, rowSums(abs(diff(pts))) > 0)
    pts <- pts[keep, , drop = FALSE]
    pr <- lapply(seq_len(N), function(i) .esl14_project(X[i, ], pts))
    lam <- vapply(pr, `[[`, numeric(1), "lambda")
    fitted <- do.call(rbind, lapply(pr, `[[`, "point"))
    new <- sum(vapply(pr, `[[`, numeric(1), "dist"))
    path <- c(path, new)
    if (abs(dist - new) <= tol * max(dist, 1e-300) || new < 1e-20) {
      dist <- new
      conv <- TRUE
      break
    }
    dist <- new
  }
  o <- order(lam, seq_len(N))
  list(lambda = lam, fitted = fitted, curve = fitted[o, , drop = FALSE], distance = dist, distance_path = path,
       iterations = it, converged = conv)
}
