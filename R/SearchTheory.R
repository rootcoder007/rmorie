#' Search theory for search and rescue planning
#'
#' \code{LateralRange}: definite range, M-Beta and inverse cube (Koopman)
#' lateral range curves with sweep width \code{W}. \code{SweepWidth}: area
#' under a sampled lateral range curve. \code{SearchPod}: POD from coverage
#' for random search, definite range and inverse cube parallel sweeps.
#' \code{ParallelSweepPod}: numerical parallel-sweep POD for any model, with
#' optional normal navigation error. \code{SearchSuccess}: POS, Bayesian POC
#' updating, cumulative POD and POS. \code{DatumProbabilityMap}: point, line
#' and area datum cell probabilities. \code{OptimalCircularSearch}: Koopman's
#' optimal effort for a circular normal datum. \code{OptimalEffortAllocation}:
#' optimal effort over cells (Stone 1975). Identical to the Python arm
#' \code{morie.fn.searchth}.
#'
#' @param x Lateral ranges or abscissae.
#' @param W Sweep width.
#' @param model Detection model.
#' @param m M-Beta height.
#' @param p Lateral range values.
#' @param coverage Coverage values.
#' @param S Track spacing.
#' @param nav_sd Navigation error standard deviation.
#' @param n_points,n_tracks Quadrature controls.
#' @param poc,pod Containment and detection probabilities (\code{pod} a
#'   vector, or a matrix with one row per search).
#' @param x_edges,y_edges Grid cell edges.
#' @param datum \code{"point"}, \code{"line"} or \code{"area"}.
#' @param center,end Datum coordinates.
#' @param sigma Circular normal standard deviation.
#' @param nodes Gauss-Legendre nodes along a line datum.
#' @param effort Total effort (track length).
#' @param area Cell areas.
#' @param tol Bisection tolerance.
#' @return Numeric or list.
#' @references Koopman, B. O. (1946). Search and Screening. OEG Report 56,
#'   Office of the Chief of Naval Operations.
#'
#'   Frost, J. R. (1996). The Theory of Search: A Simplified Explanation. Soza
#'   and Company and U.S. Coast Guard Office of Search and Rescue.
#'
#'   Stone, L. D. (1975). Theory of Optimal Search. Academic Press.
#' @examples
#' LateralRange(c(0.5, 1, 2), 1)
#' SearchPod(1, model = "inverse_cube")
#' OptimalCircularSearch(1, 1, pi / 4)$pos
#' @export
LateralRange <- function(x, W, model = "inverse_cube", m = 1) {
  a <- abs(x)
  switch(model,
    definite = ifelse(a <= W / 2, 1, 0),
    mbeta = ifelse(a <= W / (2 * m), m, 0),
    inverse_cube = ifelse(a == 0, 1, 1 - exp(-W^2 / (4 * pi * a^2))),
    stop("model must be definite, mbeta or inverse_cube")
  )
}

#' @rdname LateralRange
#' @export
SweepWidth <- function(x, p) sum(diff(x) * (p[-1] + p[-length(p)]) / 2)

#' @rdname LateralRange
#' @export
SearchPod <- function(coverage, model = "random") {
  switch(model,
    random = 1 - exp(-coverage),
    definite = pmin(coverage, 1),
    inverse_cube = 2 * stats::pnorm(sqrt(pi) * coverage / 2 * sqrt(2)) - 1,
    stop("model must be random, definite or inverse_cube")
  )
}

.st_gauss <- function(n, kind) {
  i <- seq_len(n - 1)
  b <- if (kind == "hermite") sqrt(i / 2) else i / sqrt(4 * i^2 - 1)
  J <- matrix(0, n, n)
  J[cbind(i + 1, i)] <- b
  J[cbind(i, i + 1)] <- b
  e <- eigen(J, symmetric = TRUE)
  o <- order(e$values)
  list(t = e$values[o], w = (if (kind == "hermite") sqrt(pi) else 2) * e$vectors[1, o]^2)
}

#' @rdname LateralRange
#' @export
ParallelSweepPod <- function(W, S, model = "inverse_cube", m = 1, nav_sd = 0, n_points = 2001, n_tracks = 60) {
  if (n_points %% 2 == 0) n_points <- n_points + 1
  gh <- if (nav_sd > 0) .st_gauss(40, "hermite") else NULL
  p <- function(x) {
    if (is.null(gh)) return(LateralRange(x, W, model, m))
    sum(gh$w * LateralRange(x + sqrt(2) * nav_sd * gh$t, W, model, m)) / sqrt(pi)
  }
  h <- S / (n_points - 1)
  ks <- -n_tracks:n_tracks
  vals <- vapply(seq_len(n_points) - 1, function(i) {
    x <- i * h
    miss <- prod(1 - vapply(x - ks * S, p, 0))
    if (model == "inverse_cube" && miss > 0) {
      dd <- x - ks * S
      near <- sum(1 / dd[dd != 0]^2)
      sn <- sin(pi * x / S)
      full <- if (sn != 0) (pi / S)^2 / sn^2 else near
      miss <- miss * exp(-W^2 / (4 * pi) * max(0, full - near))
    }
    1 - miss
  }, 0)
  odd <- seq(2, n_points - 1, by = 2)
  even <- seq(3, n_points - 2, by = 2)
  (vals[1] + vals[n_points] + 4 * sum(vals[odd]) + 2 * sum(vals[even])) * h / 3 / S
}

#' @rdname LateralRange
#' @export
SearchSuccess <- function(poc, pod) {
  D <- if (is.matrix(pod)) pod else matrix(pod, 1)
  cur <- poc
  for (k in seq_len(nrow(D))) {
    pos <- sum(cur * D[k, ])
    cur <- cur * (1 - D[k, ]) / (1 - pos)
  }
  cum <- 1 - apply(1 - D, 2, prod)
  list(pos = sum(poc * cum), pos_by_area = poc * cum, poc_updated = cur, cumulative_pod = cum)
}

#' @rdname LateralRange
#' @export
DatumProbabilityMap <- function(x_edges, y_edges, datum = "point", center = c(0, 0), sigma = 1, end = NULL,
                                nodes = 64) {
  nx <- length(x_edges) - 1
  ny <- length(y_edges) - 1
  cell <- function(px, py) {
    outer(diff(stats::pnorm((y_edges - py) / sigma)), diff(stats::pnorm((x_edges - px) / sigma)))
  }
  grid <- switch(datum,
    point = cell(center[1], center[2]),
    line = {
      gl <- .st_gauss(nodes, "legendre")
      Reduce(`+`, lapply(seq_along(gl$t), function(k) {
        f <- (gl$t[k] + 1) / 2
        gl$w[k] / 2 * cell(center[1] + f * (end[1] - center[1]), center[2] + f * (end[2] - center[2]))
      }))
    },
    area = {
      xr <- sort(c(center[1], end[1]))
      yr <- sort(c(center[2], end[2]))
      ox <- pmax(0, pmin(xr[2], x_edges[-1]) - pmax(xr[1], x_edges[-(nx + 1)]))
      oy <- pmax(0, pmin(yr[2], y_edges[-1]) - pmax(yr[1], y_edges[-(ny + 1)]))
      outer(oy, ox) / (diff(xr) * diff(yr))
    },
    stop("datum must be point, line or area")
  )
  list(poc = grid, outside = 1 - sum(grid))
}

#' @rdname LateralRange
#' @export
OptimalCircularSearch <- function(sigma, W, effort) {
  R <- (4 * sigma^2 * W * effort / pi)^0.25
  u <- R^2 / (2 * sigma^2)
  list(radius = R, pos = 1 - (1 + u) * exp(-u), center_density = R^2 / (2 * sigma^2 * W))
}

#' @rdname LateralRange
#' @export
OptimalEffortAllocation <- function(poc, area, W, effort, tol = 1e-14) {
  r <- poc * W / area
  total <- function(lam) sum(ifelse(r / lam > 1, area / W * log(r / lam), 0))
  lo <- log(min(r[poc > 0])) - 50
  hi <- log(max(r))
  for (it in 1:400) {
    mid <- (lo + hi) / 2
    if (total(exp(mid)) > effort) lo <- mid else hi <- mid
    if (hi - lo < tol) break
  }
  lam <- exp((lo + hi) / 2)
  z <- ifelse(r / lam > 1, area / W * log(r / lam), 0)
  cov <- W * z / area
  pod <- 1 - exp(-cov)
  list(effort = z, coverage = cov, pod = pod, pos = sum(poc * pod), lambda = lam)
}
