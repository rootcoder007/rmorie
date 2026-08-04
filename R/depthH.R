# SPDX-License-Identifier: AGPL-3.0-or-later
#' Depth of a point as the thinnest halfspace through it
#'
#' Depth is what a median means in more than one dimension: the deepest
#' point is the one no halfspace can isolate, and the definition is
#' affine invariant.
#'
#' In the plane the count is constant on each arc between consecutive
#' normals to the vectors from theta to a data point, so the minimum is
#' attained in the INTERIOR of an arc. Testing the normals themselves is
#' the natural-looking mistake: a data point then lies exactly on the
#' closed boundary and is counted, so a point outside the convex hull
#' comes back with positive depth. Arc midpoints are used instead -- no
#' epsilon, and exact. Above two dimensions the minimum is taken over the
#' data directions, an UPPER bound, flagged by \code{exact = 0}.
#'
#' Formula: \code{depth(theta) = min_u #{i: u'(x_i - theta) >= 0} / n}.
#'
#' @param X Data cloud.
#' @param theta Point whose depth is wanted.
#' @return List with \code{estimate}, \code{count}, \code{exact}, \code{n}, \code{p}.
#' @references Tukey, J. W. (1975). Proc ICM Vancouver 2:523-531;
#'   Rousseeuw & Ruts (1996) Appl Statist 45:516-526; Rousseeuw & Struyf
#'   (1998) Statist Comput 8:193-203.
#' @export
DepthH <- function(X, theta) {
  Xm <- as.matrix(X); t_ <- as.numeric(theta)
  n <- nrow(Xm); p <- ncol(Xm)
  d <- Xm - matrix(t_, n, p, byrow = TRUE)
  dirs <- list()
  if (p == 2L) {
    crit <- numeric(0)
    for (i in seq_len(n)) {
      if (d[i, 1] == 0 && d[i, 2] == 0) next
      a <- atan2(d[i, 2], d[i, 1])
      crit <- c(crit, (a + pi / 2) %% (2 * pi), (a - pi / 2) %% (2 * pi))
    }
    crit <- sort(crit)
    m <- length(crit)
    for (i in seq_len(m)) {
      hi <- crit[if (i == m) 1L else i + 1L] + (if (i == m) 2 * pi else 0)
      mid <- 0.5 * (crit[i] + hi)
      dirs[[length(dirs) + 1L]] <- c(cos(mid), sin(mid))
    }
    exact <- 1L
  } else {
    for (i in seq_len(n)) {
      if (all(d[i, ] == 0)) next
      dirs[[length(dirs) + 1L]] <- d[i, ]
      dirs[[length(dirs) + 1L]] <- -d[i, ]
    }
    exact <- 0L
  }
  if (length(dirs) == 0L) {
    return(.t1_result(estimate = 1, count = n, exact = exact, n = n, p = p,
                      method = "Tukey halfspace depth"))
  }
  best <- n
  for (u in dirs) {
    cnt <- sum(as.numeric(d %*% u) >= 0)
    if (cnt < best) best <- cnt
  }
  .t1_result(estimate = best / n, count = best, exact = exact, n = n, p = p,
             method = "Tukey halfspace depth")
}
