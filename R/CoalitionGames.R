#' Coalition games in the spatial model
#'
#' \code{MedianLines}: median lines of a weighted voting game in the plane.
#' \code{WeightedGameCore}: core parties (no open half-plane through the
#' position holds a winning coalition). \code{SpatialHeart}: the polygon
#' bounded by the median lines (or the core point). \code{QuotaSolution}:
#' quota of a weighted majority game. \code{MinimalRangeCoalition}:
#' minimal-range winning coalition. \code{RoemerPune}: Roemer's party-unanimity
#' Nash equilibrium with factional bargaining. Identical to the Python arm
#' \code{morie.fn.coalgame}.
#'
#' @param positions Two-column matrix of party positions (vector for
#'   \code{MinimalRangeCoalition}).
#' @param weights Party weights (seats).
#' @param quota Winning threshold.
#' @param a,b Party ideal points (a < b).
#' @param mu,sigma Mean and standard deviation of the median voter.
#' @param alpha_a,alpha_b Opportunist bargaining weights.
#' @param iters Best-response iterations.
#' @return List (matrix of index pairs for \code{MedianLines}).
#' @references Schofield, N. (1999). The heart and the uncovered set. Journal
#'   of Economics, Supplement 8, 79-113.
#'
#'   Shapley, L. S. (1953). Quota solutions of n-person games. Contributions
#'   to the Theory of Games II, 343-359.
#'
#'   de Swaan, A. (1973). Coalition Theories and Cabinet Formations. Elsevier.
#'
#'   Roemer, J. E. (2001). Political Competition: Theory and Applications.
#'   Harvard University Press.
#' @examples
#' MedianLines(rbind(c(0, 0), c(1, 0), c(0, 1)), c(1, 1, 1), 2)
#' QuotaSolution(c(1, 1, 1), 2)$quota
#' @export
MedianLines <- function(positions, weights, quota) {
  P <- as.matrix(positions)
  n <- nrow(P)
  out <- matrix(integer(0), 0, 2)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (all(P[i, ] == P[j, ])) next
      s <- vapply(seq_len(n), function(k) .cg_side(P[k, ], P[i, ], P[j, ]), 0)
      if (.cg_ss(weights[s >= -1e-12]) >= quota && .cg_ss(weights[s <= 1e-12]) >= quota) out <- rbind(out, c(i - 1L, j - 1L))
    }
  }
  out
}

.cg_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.cg_side <- function(p, a, b) (b[1] - a[1]) * (p[2] - a[2]) - (b[2] - a[2]) * (p[1] - a[1])

#' @rdname MedianLines
#' @export
WeightedGameCore <- function(positions, weights, quota) {
  P <- as.matrix(positions)
  n <- nrow(P)
  core <- integer(0)
  for (j in seq_len(n)) {
    ang <- numeric(0)
    for (k in seq_len(n)) if (!all(P[k, ] == P[j, ])) ang <- c(ang, atan2(P[k, 2] - P[j, 2], P[k, 1] - P[j, 1]) %% pi)
    ang <- sort(unique(ang))
    tests <- ang
    for (a in seq_along(ang)) {
      nxt <- if (a < length(ang)) ang[a + 1] else ang[1] + pi
      tests <- c(tests, 0.5 * (ang[a] + nxt))
    }
    if (!length(tests)) tests <- 0
    ok <- TRUE
    for (th in tests) {
      d <- -sin(th) * (P[, 1] - P[j, 1]) + cos(th) * (P[, 2] - P[j, 2])
      if (.cg_ss(weights[d > 1e-12]) >= quota || .cg_ss(weights[d < -1e-12]) >= quota) {
        ok <- FALSE
        break
      }
    }
    if (ok) core <- c(core, j - 1L)
  }
  list(core = core, positions = P[core + 1, , drop = FALSE])
}

.cg_hull <- function(pts) {
  pts <- unique(pts)
  pts <- pts[order(pts[, 1], pts[, 2]), , drop = FALSE]
  if (nrow(pts) <= 2) return(pts)
  cr <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  lower <- matrix(0, 0, 2)
  for (i in seq_len(nrow(pts))) {
    while (nrow(lower) >= 2 && cr(lower[nrow(lower) - 1, ], lower[nrow(lower), ], pts[i, ]) <= 0) lower <- lower[-nrow(lower), , drop = FALSE]
    lower <- rbind(lower, pts[i, ])
  }
  upper <- matrix(0, 0, 2)
  for (i in rev(seq_len(nrow(pts)))) {
    while (nrow(upper) >= 2 && cr(upper[nrow(upper) - 1, ], upper[nrow(upper), ], pts[i, ]) <= 0) upper <- upper[-nrow(upper), , drop = FALSE]
    upper <- rbind(upper, pts[i, ])
  }
  unname(rbind(lower[-nrow(lower), , drop = FALSE], upper[-nrow(upper), , drop = FALSE]))
}

#' @rdname MedianLines
#' @export
SpatialHeart <- function(positions, weights, quota) {
  P <- as.matrix(positions)
  cr <- WeightedGameCore(P, weights, quota)$core
  if (length(cr)) return(list(vertices = P[cr[1] + 1, , drop = FALSE], core = TRUE, median_lines = matrix(0L, 0, 2)))
  L <- MedianLines(P, weights, quota)
  par <- .cg_hull(P)
  inside <- function(q) {
    m <- nrow(par)
    if (m < 3) return(TRUE)
    all(vapply(seq_len(m), function(i) .cg_side(q, par[i, ], par[if (i == m) 1 else i + 1, ]) >= -1e-9, TRUE))
  }
  xs <- matrix(0, 0, 2)
  nl <- nrow(L)
  if (nl >= 2) {
    for (u in seq_len(nl - 1)) {
      for (v in (u + 1):nl) {
        p1 <- P[L[u, 1] + 1, ]
        p2 <- P[L[u, 2] + 1, ]
        p3 <- P[L[v, 1] + 1, ]
        p4 <- P[L[v, 2] + 1, ]
        den <- (p1[1] - p2[1]) * (p3[2] - p4[2]) - (p1[2] - p2[2]) * (p3[1] - p4[1])
        if (abs(den) < 1e-15) next
        t <- ((p1[1] - p3[1]) * (p3[2] - p4[2]) - (p1[2] - p3[2]) * (p3[1] - p4[1])) / den
        q <- p1 + t * (p2 - p1)
        if (inside(q)) xs <- rbind(xs, round(q, 12) + 0)
      }
    }
  }
  list(vertices = .cg_hull(xs), core = FALSE, median_lines = L)
}

.cg_minwin <- function(w, quota) {
  n <- length(w)
  out <- list()
  for (r in seq_len(n)) {
    cmb <- utils::combn(n, r, simplify = FALSE)
    for (S in cmb) {
      tot <- .cg_ss(w[S])
      if (tot >= quota && all(tot - w[S] < quota)) out[[length(out) + 1]] <- S
    }
  }
  out
}

#' @rdname MedianLines
#' @export
QuotaSolution <- function(weights, quota) {
  w <- as.numeric(weights)
  n <- length(w)
  mw <- .cg_minwin(w, quota)
  A <- t(vapply(mw, function(S) as.numeric(seq_len(n) %in% S), numeric(n)))
  A <- matrix(A, length(mw), n)
  AtA <- crossprod(A)
  At1 <- colSums(A)
  e <- .s03jacobi(AtA)
  top <- max(abs(e$values))
  q <- numeric(n)
  for (c in seq_len(n)) {
    if (e$values[c] > 1e-10 * top) q <- q + e$vectors[, c] * .cg_ss(e$vectors[, c] * At1) / e$values[c]
  }
  res <- max(abs(as.vector(A %*% q) - 1))
  list(quota = q, minimal_winning = lapply(mw, function(S) S - 1L), max_residual = res, consistent = res < 1e-9)
}

#' @rdname MedianLines
#' @export
MinimalRangeCoalition <- function(positions, weights, quota) {
  x <- as.numeric(positions)
  w <- as.numeric(weights)
  mw <- .cg_minwin(w, quota)
  key <- t(vapply(mw, function(S) c(max(x[S]) - min(x[S]), length(S), .cg_ss(w[S])), numeric(3)))
  lex <- vapply(mw, function(S) paste(sprintf("%04d", S), collapse = ","), "")
  o <- order(key[, 1], key[, 2], key[, 3], lex)
  best <- mw[[o[1]]]
  list(coalition = best - 1L, range = max(x[best]) - min(x[best]), weight = .cg_ss(w[best]))
}

#' @rdname MedianLines
#' @export
RoemerPune <- function(a, b, mu, sigma, alpha_a, alpha_b, iters = 200L) {
  gold <- function(f, lo, hi) {
    gr <- (sqrt(5) - 1) / 2
    x1 <- hi - gr * (hi - lo)
    x2 <- lo + gr * (hi - lo)
    f1 <- f(x1)
    f2 <- f(x2)
    for (it in 1:300) {
      if (f1 >= f2) {
        hi <- x2
        x2 <- x1
        f2 <- f1
        x1 <- hi - gr * (hi - lo)
        f1 <- f(x1)
      } else {
        lo <- x1
        x1 <- x2
        f1 <- f2
        x2 <- lo + gr * (hi - lo)
        f2 <- f(x2)
      }
      if (hi - lo < 1e-13) break
    }
    0.5 * (lo + hi)
  }
  obj_a <- function(t, s) {
    gain <- (s - a)^2 - (t - a)^2
    p <- pnorm(((t + s) / 2 - mu) / sigma)
    if (gain <= 0 || p <= 0) -Inf else log(p) + (1 - alpha_a) * log(gain)
  }
  obj_b <- function(s, t) {
    gain <- (t - b)^2 - (s - b)^2
    p <- 1 - pnorm(((t + s) / 2 - mu) / sigma)
    if (gain <= 0 || p <= 0) -Inf else log(p) + (1 - alpha_b) * log(gain)
  }
  t <- a
  s <- b
  for (it in seq_len(iters)) {
    t_new <- gold(function(v) obj_a(v, s), a, s)
    s_new <- gold(function(v) obj_b(v, t_new), t_new, b)
    done <- abs(t_new - t) < 1e-12 && abs(s_new - s) < 1e-12
    t <- t_new
    s <- s_new
    if (done) break
  }
  list(t = t, s = s, win_probability_a = pnorm(((t + s) / 2 - mu) / sigma))
}
