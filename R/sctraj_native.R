# morie.fn -- function file (rootcoder007/morie)
# Slingshot: cell lineages and pseudotime from single-cell data.
#
# Street, K., Risso, D., Fletcher, R. B., Das, D., Ngai, J., Yosef,
# N., Purdom, E., & Dudoit, S. (2018) "Slingshot: cell lineage and
# pseudotime inference for single-cell transcriptomics", BMC Genomics
# 19:477. doi:10.1186/s12864-018-4772-0
#
# Stage 1, the global lineage structure: clusters are the nodes of a
# graph and a minimum spanning tree is drawn between them with the
# covariance-scaled distance (Equation 1)
#   d^2(C_i, C_j) = (xbar_i - xbar_j)' (S_i + S_j)^-1 (xbar_i - xbar_j),
# "essentially a multivariate t-statistic". cov="diagonal" is offered
# as the paper offers it for small clusters, along with "euclidean".
# A lineage is any path from the root cluster to a leaf. Known
# terminal states are imposed by building the MST on the non-terminal
# clusters and joining each terminal to its nearest non-terminal
# neighbour.
#
# Stage 2, pseudotime: each lineage gets a principal curve (Hastie &
# Stuetzle) fitted SIMULTANEOUSLY across lineages: an average curve
# per branching event c_avg(t) = (1/M) sum_m c_m(t) (Equation 2),
# shrinkage c_m_new(t) = w_m(t) c_avg(t) + (1 - w_m(t)) c_m(t)
# (Equation 3), and weights from the CDF of a cosine kernel over the
# non-outlier pseudotime range of the shared cells (Equation 4).
#
# Note on Equation 4 as printed: the argument is typeset as
# t/(t_max - t_min) - 1/2 without subtracting t_min in the numerator;
# read that way w_m is not continuous. The default "shifted" reading
# puts t - t_min in the numerator, which satisfies every property the
# paper states; weight_arg="as_printed" gives the literal form.

.sctraj_COV <- c("full", "diagonal", "euclidean")

#' .sctraj_matrix
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_cluster_distances}, \code{morie_sctraj_principal_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @return The value of \code{M}, as built in the body.
#' @export
.sctraj_matrix <- function(X) {
  M <- as.matrix(X)
  storage.mode(M) <- "double"
  if (nrow(M) == 0L) {
    stop("sctraj: X is empty")
  }
  if (ncol(M) == 0L) {
    stop("sctraj: X has no columns")
  }
  if (any(!is.finite(M))) {
    stop("sctraj: X contains a non-finite value")
  }
  M
}

#' Solve A x = b; singular pooled covariance gets the paper\'s advice
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj_cluster_distances}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.sctraj_solve <- function(A, b) {
  # Solve A x = b; singular pooled covariance gets the paper's advice.
  n <- nrow(A)
  M <- cbind(A, b)
  for (cc in seq_len(n)) {
    piv <- which.max(abs(M[cc:n, cc])) + cc - 1L
    if (abs(M[piv, cc]) < 1e-12) {
      stop(paste0("sctraj: the pooled covariance is singular; use ",
                  "cov='diagonal' as the paper suggests for small ",
                  "clusters"))
    }
    tmp <- M[cc, ]
    M[cc, ] <- M[piv, ]
    M[piv, ] <- tmp
    for (r in seq_len(n)) {
      if (r == cc) {
        next
      }
      f <- M[r, cc] / M[cc, cc]
      M[r, ] <- M[r, ] - f * M[cc, ]
    }
  }
  M[, n + 1L] / diag(M)[seq_len(n)]
}

#' morie_sctraj_cluster_distances
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.sctraj_matrix}.
#' @param labels Coerced to character by the body, with \code{as.character}.
#' @param cov One of \code{"diagonal"}, \code{"euclidean"}. Defaults to \code{"full"}.
#' @param weights Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return A list with \code{distances}, \code{clusters}, \code{centers}, \code{covariances}.
#' @export
morie_sctraj_cluster_distances <- function(X, labels, cov="full",
                                           weights=NULL) {
  # Equation 1: the covariance-scaled distance between clusters.
  # cov="full" uses each cluster's full empirical covariance,
  # "diagonal" its diagonal, "euclidean" drops the scaling. weights
  # accepts cluster membership probabilities.
  if (!(cov %in% .sctraj_COV)) {
    stop(sprintf("sctraj: cov must be one of %s",
                 paste(.sctraj_COV, collapse=", ")))
  }
  M <- .sctraj_matrix(X)
  n <- nrow(M)
  p <- ncol(M)
  lab <- as.character(labels)
  if (length(lab) != n) {
    stop("sctraj: one label per cell is required")
  }
  nms <- sort(unique(lab))
  if (length(nms) < 2L) {
    stop("sctraj: at least two clusters are needed")
  }
  if (is.null(weights)) {
    weights <- rep(1.0, n)
  }
  weights <- as.numeric(weights)
  if (length(weights) != n || any(weights < 0)) {
    stop("sctraj: one non-negative weight per cell is required")
  }
  centers <- list()
  covs <- list()
  for (cl in nms) {
    idx <- which(lab == cl)
    wsum <- sum(weights[idx])
    if (wsum <= 0) {
      stop(sprintf("sctraj: cluster %s has no weight", cl))
    }
    w <- weights[idx]
    Xi <- M[idx, , drop=FALSE]
    mu <- as.numeric(crossprod(Xi, w)) / wsum
    S <- matrix(0.0, p, p)
    if (cov != "euclidean" && length(idx) > 1L) {
      denom <- wsum - sum(w ^ 2) / wsum
      if (denom <= 1e-12) {
        denom <- 1.0
      }
      Ctr <- sweep(Xi, 2L, mu)
      S <- crossprod(Ctr, Ctr * w) / denom
      if (cov == "diagonal") {
        S <- diag(diag(S), p, p)
      }
    }
    centers[[cl]] <- mu
    covs[[cl]] <- S
  }
  D <- matrix(0.0, length(nms), length(nms), dimnames=list(nms, nms))
  for (a in nms) {
    for (b in nms) {
      if (a == b) {
        next
      }
      diff <- centers[[a]] - centers[[b]]
      if (cov == "euclidean") {
        D[a, b] <- sqrt(sum(diff * diff))
        next
      }
      P <- covs[[a]] + covs[[b]]
      for (i in seq_len(p)) {
        if (abs(P[i, i]) < 1e-12) {
          P[i, i] <- P[i, i] + 1e-12
        }
      }
      sol <- .sctraj_solve(P, diff)
      d2 <- sum(diff * sol)
      D[a, b] <- sqrt(max(d2, 0.0))
    }
  }
  list(distances=D, clusters=nms, centers=centers, covariances=covs)
}

#' Prim\'s MST, with the paper\'s optional terminal-state constraint
#'
#' With ends given, the tree is built on the non-terminal clusters and
#' each terminal is attached to its nearest non-terminal neighbour.
#'
#' @param D A matrix; indexed by row and column.
#' @param clusters Coerced to character by the body, with \code{as.character}.
#' @param ends Defaults to \code{NULL}.
#' @return A list with \code{edges}, \code{adjacency}, \code{nodes}.
#' @export
morie_sctraj_minimum_spanning_tree <- function(D, clusters, ends=NULL) {
  # Prim's MST, with the paper's optional terminal-state constraint.
  # With ends given, the tree is built on the non-terminal clusters
  # and each terminal is attached to its nearest non-terminal
  # neighbour.
  nodes <- as.character(clusters)
  if (length(nodes) < 2L) {
    stop("sctraj: a tree needs at least two clusters")
  }
  ends <- as.character(if (is.null(ends)) character(0) else ends)
  for (e in ends) {
    if (!(e %in% nodes)) {
      stop(sprintf("sctraj: terminal state %s is not a cluster", e))
    }
  }
  inner <- nodes[!(nodes %in% ends)]
  if (length(inner) == 0L) {
    stop("sctraj: every cluster was marked terminal")
  }
  edges <- list()
  seen <- inner[1L]
  while (length(seen) < length(inner)) {
    best <- NULL
    for (a in seen) {
      for (b in inner) {
        if (b %in% seen) {
          next
        }
        w <- D[a, b]
        if (is.null(best) || w < best$w) {
          best <- list(w=w, a=a, b=b)
        }
      }
    }
    edges <- c(edges, list(list(from=best$a, to=best$b, weight=best$w)))
    seen <- c(seen, best$b)
  }
  for (e in ends) {
    near <- inner[which.min(D[e, inner])]
    edges <- c(edges, list(list(from=near, to=e, weight=D[e, near])))
  }
  adj <- stats::setNames(vector("list", length(nodes)), nodes)
  for (edge in edges) {
    adj[[edge$from]] <- c(adj[[edge$from]], edge$to)
    adj[[edge$to]] <- c(adj[[edge$to]], edge$from)
  }
  list(edges=edges, adjacency=adj, nodes=nodes)
}

#' Every path from root to a leaf, in order
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree A list; the body reads \code{$adjacency} from it.
#' @param root Coerced to character by the body, with \code{as.character}.
#' @return The value of \code{[}.
#' @export
morie_sctraj_lineages_from_tree <- function(tree, root) {
  # Every path from root to a leaf, in order.
  adj <- tree$adjacency
  root <- as.character(root)
  if (!(root %in% names(adj))) {
    stop("sctraj: the root is not a cluster")
  }
  out <- list()
  stack <- list(list(node=root, path=root))
  while (length(stack) > 0L) {
    top <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    kids <- adj[[top$node]]
    kids <- kids[!(kids %in% top$path)]
    if (length(kids) == 0L) {
      out <- c(out, list(top$path))
      next
    }
    for (v in kids) {
      stack <- c(stack, list(list(node=v, path=c(top$path, v))))
    }
  }
  keys <- vapply(out, function(pth) paste(pth, collapse="\r"),
                 character(1))
  out[order(keys)]
}

# ------------------------------------------------------ principal curves

#' .sctraj_arc_length
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_principal_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param curve A matrix; indexed by row and column.
#' @return The value of \code{s}, as built in the body.
#' @export
.sctraj_arc_length <- function(curve) {
  m <- nrow(curve)
  s <- numeric(m)
  if (m > 1L) {
    d <- sqrt(rowSums((curve[-1L, , drop=FALSE] -
                       curve[-m, , drop=FALSE]) ^ 2))
    s[-1L] <- cumsum(d)
  }
  s
}

#' Nearest point on the polyline, and its arc length
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_principal_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param point Numeric; combined arithmetically in the body.
#' @param curve A matrix; indexed by row and column.
#' @param s A vector; indexed elementwise.
#' @return The value of \code{best}, as built in the body.
#' @export
.sctraj_project <- function(point, curve, s) {
  # Nearest point on the polyline, and its arc length.
  best <- NULL
  for (kk in seq_len(nrow(curve) - 1L)) {
    a <- curve[kk, ]
    b <- curve[kk + 1L, ]
    seg <- b - a
    L2 <- sum(seg * seg)
    if (L2 <= 0) {
      t <- 0.0
    } else {
      t <- sum((point - a) * seg) / L2
      t <- min(max(t, 0.0), 1.0)
    }
    proj <- a + t * seg
    d2 <- sum((point - proj) ^ 2)
    lam <- s[kk] + t * sqrt(L2)
    if (is.null(best) || d2 < best$d2) {
      best <- list(d2=d2, lam=lam, proj=proj)
    }
  }
  best
}

#' Local linear smoother of y on t -- the "smoothing spline" step,
#'
#' kept simple and weight-aware.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @param w A vector; indexed elementwise.
#' @param span Numeric; combined arithmetically in the body. Defaults to \code{0.4}.
#' @return The value of \code{out}, as built in the body.
#' @export
.sctraj_smooth <- function(t, y, w, span=0.4) {
  # Local linear smoother of y on t -- the "smoothing spline" step,
  # kept simple and weight-aware.
  n <- length(t)
  ord <- order(t)
  k <- max(3L, as.integer(span * n))
  out <- numeric(n)
  for (pos in seq_len(n)) {
    i <- ord[pos]
    lo <- max(1L, pos - k)
    hi <- min(n, pos + k)
    idx <- ord[lo:hi]
    sw <- sum(w[idx])
    if (sw <= 0) {
      out[i] <- y[i]
      next
    }
    tm <- sum(w[idx] * t[idx]) / sw
    ym <- sum(w[idx] * y[idx]) / sw
    num <- sum(w[idx] * (t[idx] - tm) * (y[idx] - ym))
    den <- sum(w[idx] * (t[idx] - tm) ^ 2)
    slope <- if (den > 1e-12) num / den else 0.0
    out[i] <- ym + slope * (t[i] - tm)
  }
  out
}

#' morie_sctraj_principal_curve
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.sctraj_matrix}.
#' @param init A matrix; passed to \code{as.matrix}.
#' @param weights Optional; may be \code{NULL}. A vector; its length is taken.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{15}.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{0.001}.
#' @param span Passed to \code{.sctraj_smooth}. Defaults to \code{0.4}.
#' @param n_knots Defaults to \code{NULL}.
#' @return A list with \code{pseudotime}, \code{curve}, \code{distance}, \code{sse}.
#' @export
morie_sctraj_principal_curve <- function(X, init, weights=NULL,
                                         max_iter=15, tol=1e-3, span=0.4,
                                         n_knots=NULL) {
  # The Hastie-Stuetzle iteration, initialised from a given path.
  # Returns pseudotimes (arc length along the curve, lowest set to
  # zero), the fitted curve as an ordered polyline (matrix), and the
  # distance of each cell to it.
  M <- .sctraj_matrix(X)
  n <- nrow(M)
  p <- ncol(M)
  if (is.null(weights)) {
    weights <- rep(1.0, n)
  }
  weights <- as.numeric(weights)
  if (length(weights) != n) {
    stop("sctraj: one weight per cell is required")
  }
  if (max_iter < 1) {
    stop("sctraj: max_iter must be at least 1")
  }
  curve <- as.matrix(init)
  storage.mode(curve) <- "double"
  if (nrow(curve) < 2L) {
    stop("sctraj: the initial curve needs two points")
  }
  prev <- NULL
  lam <- numeric(n)
  dist <- numeric(n)
  for (it in seq_len(as.integer(max_iter))) {
    s <- .sctraj_arc_length(curve)
    for (i in seq_len(n)) {
      pr <- .sctraj_project(M[i, ], curve, s)
      lam[i] <- pr$lam
      dist[i] <- sqrt(pr$d2)
    }
    lam <- lam - min(lam)
    sse <- sum(weights * dist ^ 2)
    if (!is.null(prev) && abs(prev - sse) <= tol * max(prev, 1e-12)) {
      break
    }
    prev <- sse
    fitted <- matrix(0.0, n, p)
    for (j in seq_len(p)) {
      fitted[, j] <- .sctraj_smooth(lam, M[, j], weights, span)
    }
    # The curve belongs to THIS lineage, so it is drawn only through
    # the cells assigned to it. Including zero-weight cells would let
    # a sibling branch pull the curve off its own trunk.
    live <- which(weights > 0)
    if (length(live) < 2L) {
      stop("sctraj: a lineage has fewer than two weighted cells")
    }
    ord <- live[order(lam[live])]
    curve <- fitted[ord, , drop=FALSE]
    # collapse duplicate points so the polyline stays well defined
    keep <- 1L
    for (q in seq.int(2L, nrow(curve))) {
      if (any(abs(curve[q, ] - curve[keep[length(keep)], ]) > 1e-12)) {
        keep <- c(keep, q)
      }
    }
    if (length(keep) < 2L) {
      break
    }
    curve <- curve[keep, , drop=FALSE]
  }
  list(pseudotime=lam, curve=curve, distance=dist,
       sse=sum(weights * dist ^ 2))
}

#' .sctraj_interp
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_average_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s A vector; its length is taken and its elements indexed.
#' @param curve A matrix; indexed by row and column.
#' @param u Numeric; combined arithmetically in the body.
#' @return The value of \code{[}.
#' @export
.sctraj_interp <- function(s, curve, u) {
  m <- length(s)
  if (u <= s[1L]) {
    return(curve[1L, ])
  }
  if (u >= s[m]) {
    return(curve[m, ])
  }
  for (kk in seq_len(m - 1L)) {
    if (s[kk] <= u && u <= s[kk + 1L]) {
      spn <- s[kk + 1L] - s[kk]
      t <- if (spn <= 0) 0.0 else (u - s[kk]) / spn
      return(curve[kk, ] + t * (curve[kk + 1L, ] - curve[kk, ]))
    }
  }
  curve[m, ]
}

#' morie_sctraj_average_curve
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param curves A vector; its length is taken and its elements indexed.
#' @param n_points A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.
#' @param return_grid A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_sctraj_average_curve <- function(curves, n_points=100,
                                       return_grid=FALSE) {
  # Equation 2: c_avg(t) = (1/M) sum_m c_m(t). The common domain is
  # [0, min_m len(c_m)] -- arc lengths, not fractions of a length.
  if (length(curves) == 0L) {
    stop("sctraj: nothing to average")
  }
  if (n_points < 2) {
    stop("sctraj: n_points must be at least 2")
  }
  p <- ncol(curves[[1L]])
  arcs <- lapply(curves, .sctraj_arc_length)
  t_max <- min(vapply(arcs, function(a) a[length(a)], numeric(1)))
  if (t_max <= 0) {
    t_max <- 1.0
  }
  n_points <- as.integer(n_points)
  grid <- (seq_len(n_points) - 1L) * t_max / (n_points - 1)
  out <- matrix(0.0, n_points, p)
  for (gi in seq_len(n_points)) {
    acc <- rep(0.0, p)
    for (ci in seq_along(curves)) {
      acc <- acc + .sctraj_interp(arcs[[ci]], curves[[ci]], grid[gi])
    }
    out[gi, ] <- acc / length(curves)
  }
  if (return_grid) {
    return(list(curve=out, grid=grid))
  }
  out
}

#' CDF of a cosine kernel of bandwidth 1/2: density 1 + cos(2 pi u)
#'
#' on [-1/2, 1/2], so F(u) = u + 1/2 + sin(2 pi u)/(2 pi).
#'
#' @param u Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
morie_sctraj_cosine_cdf <- function(u) {
  # CDF of a cosine kernel of bandwidth 1/2: density 1 + cos(2 pi u)
  # on [-1/2, 1/2], so F(u) = u + 1/2 + sin(2 pi u)/(2 pi).
  if (u <= -0.5) {
    return(0.0)
  }
  if (u >= 0.5) {
    return(1.0)
  }
  u + 0.5 + sin(2 * pi * u) / (2 * pi)
}

#' morie_sctraj_shrinkage_weight
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Numeric; combined arithmetically in the body.
#' @param t_min Numeric; combined arithmetically in the body.
#' @param t_max Numeric; combined arithmetically in the body.
#' @param arg One of \code{"as_printed"}, \code{"shifted"}. Defaults to \code{"shifted"}.
#' @return A numeric value.
#' @export
morie_sctraj_shrinkage_weight <- function(t, t_min, t_max,
                                          arg="shifted") {
  # Equation 4's weighting function. arg="shifted" (default) puts
  # t - t_min in the numerator, which is what makes the function
  # continuous and non-increasing; "as_printed" uses the literal t.
  if (!(arg %in% c("shifted", "as_printed"))) {
    stop("sctraj: arg must be 'shifted' or 'as_printed'")
  }
  if (t_max <= t_min) {
    return(if (t <= t_min) 1.0 else 0.0)
  }
  if (t < t_min) {
    return(1.0)
  }
  if (t > t_max) {
    return(0.0)
  }
  num <- if (arg == "shifted") t - t_min else t
  1.0 - morie_sctraj_cosine_cdf(num / (t_max - t_min) - 0.5)
}

#' Lowest and highest non-outlier values, by the 1.5 IQR rule
#'
#' A step of the sctraj_native implementation. Called by \code{morie_sctraj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param vals Coerced to numeric by the body, with \code{as.numeric}.
#' @return One of two values, depending on the branch taken.
#' @export
.sctraj_non_outlier_range <- function(vals) {
  # Lowest and highest non-outlier values, by the 1.5 IQR rule.
  v <- sort(as.numeric(vals))
  if (length(v) == 0L) {
    return(c(0.0, 0.0))
  }
  n <- length(v)
  qf <- function(f) {
    pos <- f * (n - 1)
    lo <- floor(pos)
    hi <- min(lo + 1, n - 1)
    v[lo + 1L] + (pos - lo) * (v[hi + 1L] - v[lo + 1L])
  }
  q1 <- qf(0.25)
  q3 <- qf(0.75)
  iqr <- q3 - q1
  keep <- v[v >= q1 - 1.5 * iqr & v <= q3 + 1.5 * iqr]
  if (length(keep) > 0L) {
    c(keep[1L], keep[length(keep)])
  } else {
    c(v[1L], v[n])
  }
}

# --------------------------------------------------------------- driver

#' morie_sctraj
#'
#' A step of the sctraj_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.sctraj_matrix}.
#' @param labels Coerced to character by the body, with \code{as.character}.
#' @param root Passed to \code{morie_sctraj_lineages_from_tree}.
#' @param ends Passed to \code{morie_sctraj_minimum_spanning_tree}.
#' @param cov Passed to \code{morie_sctraj_cluster_distances}. Defaults to \code{"full"}.
#' @param max_iter Passed to \code{morie_sctraj_principal_curve}. Defaults to \code{15}.
#' @param shrink A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param weight_arg Passed to \code{morie_sctraj_shrinkage_weight}. Defaults to \code{"shifted"}.
#' @param span Passed to \code{morie_sctraj_principal_curve}. Defaults to \code{0.4}.
#' @param n_points Passed to \code{morie_sctraj_average_curve}. Defaults to \code{100}.
#' @return A list with \code{estimate}, \code{pseudotime}, \code{weights}, \code{lineages}, \code{curves}, \code{tree}, \code{distance}, \code{clusters}, \code{centers}, \code{n_lineages}, \code{root}, \code{cov}, \code{shrink}, \code{method}, \code{note}.
#' @export
morie_sctraj <- function(X, labels, root, ends=NULL, cov="full",
                         max_iter=15, shrink=TRUE, weight_arg="shifted",
                         span=0.4, n_points=100) {
  # Infer lineages and pseudotime (Street et al. 2018). Returns one
  # pseudotime vector and one cell-weight vector per lineage; a
  # cell's weight is its assignment to that lineage, from its
  # projection distance to the lineage's curve.
  M <- .sctraj_matrix(X)
  n <- nrow(M)
  p <- ncol(M)
  lab <- as.character(labels)
  if (length(lab) != n) {
    stop("sctraj: one label per cell is required")
  }
  info <- morie_sctraj_cluster_distances(M, lab, cov)
  tree <- morie_sctraj_minimum_spanning_tree(info$distances,
                                             info$clusters, ends)
  lins <- morie_sctraj_lineages_from_tree(tree, root)
  if (length(lins) == 0L) {
    stop("sctraj: no lineage runs from the root")
  }
  # initial curves: the piecewise linear path through the cluster
  # centres of each lineage, not the first principal component
  curves <- list()
  pts <- list()
  dists <- list()
  for (m in seq_along(lins)) {
    path <- lins[[m]]
    init <- do.call(rbind, lapply(path, function(cl) info$centers[[cl]]))
    member <- as.numeric(lab %in% path)
    if (sum(member) < 2) {
      stop("sctraj: a lineage has too few cells")
    }
    fit <- morie_sctraj_principal_curve(M, init, member, max_iter,
                                        span=span)
    curves[[m]] <- fit$curve
    pts[[m]] <- fit$pseudotime
    dists[[m]] <- fit$distance
  }
  shrunk <- curves
  if (isTRUE(shrink) && length(lins) > 1L) {
    av <- morie_sctraj_average_curve(curves, n_points, return_grid=TRUE)
    avg <- av$curve
    avg_grid <- av$grid
    for (m in seq_along(lins)) {
      path <- lins[[m]]
      in_other <- rep(FALSE, n)
      for (k in seq_along(lins)) {
        if (k == m) {
          next
        }
        in_other <- in_other | (lab %in% lins[[k]])
      }
      shared <- which((lab %in% path) & in_other)
      if (length(shared) == 0L) {
        next
      }
      rng <- .sctraj_non_outlier_range(pts[[m]][shared])
      t_min <- rng[1L]
      t_max <- rng[2L]
      s <- .sctraj_arc_length(shrunk[[m]])
      new_ <- shrunk[[m]]
      for (kk in seq_len(nrow(new_))) {
        t <- s[kk]
        w <- morie_sctraj_shrinkage_weight(t, t_min, t_max, weight_arg)
        a <- .sctraj_interp(avg_grid, avg, t)
        new_[kk, ] <- w * a + (1.0 - w) * shrunk[[m]][kk, ]
      }
      shrunk[[m]] <- new_
    }
    pts <- list()
    dists <- list()
    for (m in seq_along(shrunk)) {
      cmat <- shrunk[[m]]
      s <- .sctraj_arc_length(cmat)
      lam <- numeric(n)
      dd <- numeric(n)
      for (i in seq_len(n)) {
        pr <- .sctraj_project(M[i, ], cmat, s)
        lam[i] <- pr$lam
        dd[i] <- sqrt(pr$d2)
      }
      pts[[m]] <- lam - min(lam)
      dists[[m]] <- dd
    }
  }
  # cell weights from projection distance: closer curve, higher weight
  W <- list()
  for (m in seq_along(lins)) {
    col <- numeric(n)
    for (i in seq_len(n)) {
      best <- min(vapply(seq_along(lins),
                         function(k) dists[[k]][i], numeric(1)))
      if (dists[[m]][i] <= best + 1e-12) {
        col[i] <- 1.0
      } else if (dists[[m]][i] > 0) {
        col[i] <- best / dists[[m]][i]
      } else {
        col[i] <- 1.0
      }
    }
    W[[m]] <- col
  }
  list(
    estimate=pts,
    pseudotime=pts,
    weights=W,
    lineages=lins,
    curves=shrunk,
    tree=tree$edges,
    distance=dists,
    clusters=info$clusters,
    centers=info$centers,
    n_lineages=length(lins),
    root=root,
    cov=cov,
    shrink=isTRUE(shrink),
    method=paste0("Slingshot (Street et al. 2018): covariance-scaled ",
                  "MST over cluster centres, then simultaneous ",
                  "principal curves with recursive averaging and ",
                  "shrinkage"),
    note=paste0("Equation 4 is typeset with t, not t - t_min, in the ",
                "numerator; the default 'shifted' reading is the one ",
                "that is continuous and non-increasing as the paper ",
                "states, and weight_arg='as_printed' gives the literal ",
                "form")
  )
}

morie_sctraj_pseudotime_trajectory <- morie_sctraj

#' morie_sctraj_cheatsheet
#'
#' A step of the sctraj_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_sctraj_cheatsheet <- function() {
  paste0(
    "sctraj: Slingshot (Street et al. 2018). Stage 1 draws an ",
    "MST over cluster centres using the covariance-scaled ",
    "distance d^2 = (xi-xj)'(Si+Sj)^-1(xi-xj), and every path ",
    "from the root to a leaf is a lineage; terminal states can ",
    "be imposed by building the MST without them and attaching ",
    "each to its nearest neighbour. Stage 2 fits simultaneous ",
    "principal curves: average curves recursively from the ",
    "leaves, then shrink from the root outward with a cosine ",
    "kernel weight that is 1 at the origin and 0 past the last ",
    "shared cell. Pseudotime is arc length along the curve."
  )
}

# public names resolved by fn/_lazy_map.json
morie_sctraj_scrnaseq_trajectory <- morie_sctraj
