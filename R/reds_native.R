# SPDX-License-Identifier: AGPL-3.0-or-later

# ---------------------------------------------------------------------
# Mirrors of the 2026-07 reds-triage corrections in morie.fn:
# ARCH-LM (volengle), multi-horizon distributional accuracy
# (volcorpst), convex hull (cvxhl), adjacency + non-backtracking
# matrices (sgtadj/sgtnbe), and the DCC front-end (dccgrch).
# The Python McNemar and Lilliefors fixes need no mirror: the R arms
# already used Edwards' correction and a proper Lilliefors null.
# ---------------------------------------------------------------------

#' Engle's ARCH-LM test
#'
#' Regresses the squared (demeaned) series on a constant and its own q
#' lags and forms \eqn{LM = m R^2} with \eqn{m = n - q}, asymptotically
#' \eqn{\chi^2_q} under the null of no ARCH. Rejection says the
#' variance is predictable from its own past while the level may stay
#' serially uncorrelated -- a KS normality check cannot detect this at
#' all, which is what the Python placeholder this mirrors replaced.
#'
#' Mirrors \code{morie.fn.volengle}.
#'
#' @param r Numeric series (returns or residuals).
#' @param q Lags in the auxiliary regression.
#' @param demean Subtract the sample mean before squaring.
#' @return Named list with \code{statistic}, \code{p_value}, \code{df},
#'   \code{r2}, \code{n}, \code{q}, \code{method}.
#' @references Engle, R. F. (1982). Autoregressive conditional
#'   heteroscedasticity with estimates of the variance of United
#'   Kingdom inflation. \emph{Econometrica}, 50(4), 987-1007. Sec. 8.
#' @examples
#' set.seed(1)
#' morie_arch_lm_test(rnorm(200), q = 2)$p_value
#' @export
morie_arch_lm_test <- function(r, q = 1L, demean = TRUE) {
  r <- as.numeric(r)
  n <- length(r)
  q <- as.integer(q)
  if (q < 1L) stop("q must be at least 1, got ", q, ".", call. = FALSE)
  if (n < q + 2L) stop("Need at least q + 2 = ", q + 2, " observations, got ", n, ".", call. = FALSE)
  if (!all(is.finite(r))) stop("r must be finite.", call. = FALSE)

  e2 <- (if (demean) r - mean(r) else r)^2
  Y <- e2[(q + 1L):n]
  X <- cbind(1, sapply(seq_len(q), function(j) e2[(q + 1L - j):(n - j)]))
  beta <- qr.solve(X, Y)
  resid <- Y - X %*% beta
  tss <- sum((Y - mean(Y))^2)
  if (tss <= 0) stop("squared series has zero variance; LM test undefined.", call. = FALSE)
  r2 <- 1 - sum(resid^2) / tss
  m <- n - q
  lm_stat <- m * r2
  list(statistic = lm_stat, p_value = stats::pchisq(lm_stat, q, lower.tail = FALSE),
       df = q, r2 = r2, n = n, q = q,
       method = sprintf("Engle ARCH-LM test (q=%d)", q))
}

# Internal: KS statistic for a sorted sample against CDF values.
.rn_ks_stat <- function(cdf_vals) {
  n <- length(cdf_vals)
  i <- seq_len(n)
  max(max(i / n - cdf_vals), max(cdf_vals - (i - 1) / n))
}

# Internal: Monte Carlo null of the KS statistic with fitted Gaussian
# parameters (the Lilliefors construction; parameter-free for a fitted
# location-scale family).
.rn_mc_p_fitted <- function(d_obs, n, n_mc) {
  count <- 0L
  for (b in seq_len(n_mc)) {
    z <- sort(stats::rnorm(n))
    d <- .rn_ks_stat(stats::pnorm(z, mean(z), stats::sd(z)))
    if (d >= d_obs) count <- count + 1L
  }
  (1 + count) / (1 + n_mc)
}

#' Multi-horizon distributional accuracy test
#'
#' For each horizon h the series is aggregated into non-overlapping
#' h-period sums and a Kolmogorov-type sup distance compares their
#' empirical distribution with the model's distribution at that
#' horizon -- the \eqn{V_{1T}}-type comparison of Corradi & Swanson
#' (2006). A model can fit the one-period distribution and still fail
#' at 20 periods; checking several horizons is what detects that.
#'
#' With \code{cdf} supplied the null is fully specified and classical
#' Kolmogorov p-values apply per horizon; with \code{cdf = NULL} a
#' Gaussian is fitted per horizon and the null is simulated (the
#' Lilliefors construction). The joint p-value is Bonferroni.
#'
#' Mirrors \code{morie.fn.volcorpst}.
#'
#' @param r Numeric return series.
#' @param horizons Positive integer aggregation horizons.
#' @param cdf Optional function \code{cdf(x, h)} giving the model CDF
#'   of an h-period aggregate.
#' @param n_mc Monte Carlo replicates for the fitted-parameter null.
#' @param seed Seed for the Monte Carlo.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{per_horizon} (data.frame: h, n_h, statistic, p_value),
#'   \code{n}, \code{method}.
#' @references Corradi, V. & Swanson, N. R. (2006). Predictive density
#'   and conditional confidence interval accuracy tests. \emph{Journal
#'   of Econometrics}, 135(1-2), 187-228. Lilliefors, H. W. (1967). On
#'   the Kolmogorov-Smirnov test for normality with mean and variance
#'   unknown. \emph{JASA}, 62(318), 399-402.
#' @examples
#' set.seed(1)
#' morie_multi_horizon_ks(rnorm(400), horizons = c(1, 5), n_mc = 100)$p_value
#' @export
morie_multi_horizon_ks <- function(r, horizons = c(1L, 5L, 20L), cdf = NULL,
                                   n_mc = 500L, seed = 0L) {
  r <- as.numeric(r)
  n <- length(r)
  horizons <- as.integer(horizons)
  if (any(horizons < 1L)) stop("horizons must be positive.", call. = FALSE)
  if (!all(is.finite(r))) stop("r must be finite.", call. = FALSE)
  hmax <- max(horizons)
  if (n < 8L * hmax) {
    stop("Need at least 8 aggregates at the longest horizon; n=", n,
         " gives ", n %/% hmax, " at h=", hmax, ".", call. = FALSE)
  }
  set.seed(seed)
  rows <- lapply(horizons, function(h) {
    m <- n %/% h
    agg <- sort(colSums(matrix(r[seq_len(m * h)], nrow = h)))
    if (is.null(cdf)) {
      d <- .rn_ks_stat(stats::pnorm(agg, mean(agg), stats::sd(agg)))
      p <- .rn_mc_p_fitted(d, m, as.integer(n_mc))
    } else {
      d <- .rn_ks_stat(vapply(agg, function(x) cdf(x, h), numeric(1)))
      # Classical Kolmogorov tail via the alternating series.
      lam <- (sqrt(m) + 0.12 + 0.11 / sqrt(m)) * d
      k <- 1:100
      p <- min(1, max(0, 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * lam^2))))
    }
    data.frame(h = h, n_h = m, statistic = d, p_value = p)
  })
  per <- do.call(rbind, rows)
  list(statistic = max(per$statistic),
       p_value = min(1, nrow(per) * min(per$p_value)),
       per_horizon = per, n = n,
       method = "Multi-horizon KS-type distributional accuracy (Corradi-Swanson type)")
}

#' Convex hull of planar points
#'
#' Hull vertices in traversal order plus the enclosed area by the
#' shoelace formula. Vertex ORDER matters: the area is only right if
#' the vertices trace the polygon, which is why it is part of the
#' return value rather than left to the caller.
#'
#' Mirrors \code{morie.fn.cvxhl} (whose NumPy-2 break -- np.cross on
#' 2-vectors -- has no analogue here; \code{grDevices::chull} does the
#' scan).
#'
#' @param points Two-column matrix of (x, y).
#' @return Named list with \code{hull_indices}, \code{hull_points},
#'   \code{n_vertices}, \code{area}, \code{method}.
#' @examples
#' morie_convex_hull(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(.5, .5)))$area
#' @export
morie_convex_hull <- function(points) {
  P <- as.matrix(points)
  if (ncol(P) != 2L) stop("points must be an (n, 2) matrix.", call. = FALSE)
  if (nrow(P) < 3L) stop("Need >= 3 points for a convex hull, got ", nrow(P), ".", call. = FALSE)
  idx <- grDevices::chull(P[, 1], P[, 2])
  hull <- P[idx, , drop = FALSE]
  x <- hull[, 1]; y <- hull[, 2]
  area <- 0.5 * abs(sum(x * c(y[-1], y[1])) - sum(y * c(x[-1], x[1])))
  list(hull_indices = idx, hull_points = hull, n_vertices = length(idx),
       area = area, method = "Convex hull (Andrew scan via grDevices::chull)")
}

#' Adjacency matrix from an edge list
#'
#' \eqn{A_{ij} = 1} iff \eqn{(i, j) \in E}, symmetrised for undirected
#' graphs. Labels may be arbitrary; they map to indices in sorted
#' order and the mapping is returned.
#'
#' Mirrors \code{morie.fn.sgtadj}.
#'
#' @param edges Two-column matrix or list of pairs.
#' @param n Optional number of nodes (integer labels then index
#'   directly).
#' @param directed Keep edges one-way.
#' @return Named list with \code{A}, \code{nodes}, \code{degree},
#'   \code{n}, \code{m}, \code{directed}, \code{method}.
#' @references Chung, F. R. K. (1997). \emph{Spectral Graph Theory}.
#'   CBMS 92, AMS. Ch. 1.
#' @examples
#' morie_adjacency_matrix(rbind(c("A", "B"), c("B", "C")))$degree
#' @export
morie_adjacency_matrix <- function(edges, n = NULL, directed = FALSE) {
  E <- if (is.matrix(edges)) edges else do.call(rbind, lapply(edges, function(e) {
    if (length(e) != 2L) stop("every edge must be a pair.", call. = FALSE)
    c(e[[1]], e[[2]])
  }))
  if (ncol(E) != 2L) stop("every edge must be a pair.", call. = FALSE)
  labs <- sort(unique(as.vector(E)))
  if (is.null(n)) {
    index <- stats::setNames(seq_along(labs), labs)
    size <- length(labs)
  } else {
    size <- as.integer(n)
    num <- suppressWarnings(as.numeric(labs))
    if (length(labs) && !anyNA(num) && all(num == floor(num))) {
      if (min(num) < 0 || max(num) >= size) {
        stop("integer labels must lie in [0, ", size - 1, "].", call. = FALSE)
      }
      index <- stats::setNames(num + 1, labs)  # 0-based labels -> 1-based rows
    } else {
      if (length(labs) > size) stop("n=", size, " but ", length(labs), " distinct labels.", call. = FALSE)
      index <- stats::setNames(seq_along(labs), labs)
    }
  }
  A <- matrix(0, size, size)
  for (k in seq_len(nrow(E))) {
    i <- index[[as.character(E[k, 1])]]
    j <- index[[as.character(E[k, 2])]]
    A[i, j] <- 1
    if (!directed) A[j, i] <- 1
  }
  m <- if (directed) nrow(unique(E)) else nrow(unique(t(apply(E, 1, sort))))
  list(A = A, nodes = index, degree = rowSums(A), n = size, m = m,
       directed = directed, method = "Adjacency matrix from edge list")
}

#' Non-backtracking (Hashimoto) matrix
#'
#' Each undirected edge becomes two directed edges; for e = (u, v) and
#' f = (w, x), \eqn{B_{ef} = 1} iff v = w and x != u. Powers of B
#' count non-backtracking walks, which is why its spectrum drives
#' clustering in sparse graphs where the adjacency spectrum fails.
#'
#' Mirrors \code{morie.fn.sgtnbe}.
#'
#' @param edges Two-column matrix or list of pairs (undirected).
#' @param n Optional number of nodes.
#' @return Named list with \code{B}, \code{directed_edges} (two-column
#'   matrix, row order of B), \code{n}, \code{m}, \code{method}.
#' @references Hashimoto, K. (1989). Zeta functions of finite graphs
#'   and representations of p-adic groups. \emph{Adv. Stud. Pure
#'   Math.}, 15, 211-280. Krzakala, F. et al. (2013). Spectral
#'   redemption in clustering sparse networks. \emph{PNAS}, 110(52),
#'   20935-20940.
#' @examples
#' morie_nonbacktracking_matrix(rbind(c(0, 1), c(1, 2)))$m
#' @export
morie_nonbacktracking_matrix <- function(edges, n = NULL) {
  adj <- morie_adjacency_matrix(edges, n = n, directed = FALSE)
  A <- adj$A
  size <- adj$n
  de <- which(A > 0 & !diag(TRUE, size), arr.ind = TRUE)
  de <- de[order(de[, 1], de[, 2]), , drop = FALSE]
  m2 <- nrow(de)
  B <- matrix(0, m2, m2)
  key <- paste(de[, 1], de[, 2])
  pos <- stats::setNames(seq_len(m2), key)
  for (i in seq_len(m2)) {
    u <- de[i, 1]; v <- de[i, 2]
    for (w in which(A[v, ] > 0)) {
      if (w != u && w != v) B[i, pos[[paste(v, w)]]] <- 1
    }
  }
  list(B = B, directed_edges = unname(de), n = size, m = m2 %/% 2L,
       method = "Non-backtracking (Hashimoto) matrix")
}

#' DCC(1,1) GARCH front-end
#'
#' Delegates to \code{\link{morie_dcc_multivariate_garch}}, which holds
#' the two-step Gaussian MLE; this alias exists under the historical
#' short name so the three language arms expose the same pair of entry
#' points.
#'
#' Mirrors \code{morie.fn.dccgrch}.
#'
#' @param x Multivariate return matrix, n >= 30 rows, k >= 2 columns.
#' @return Whatever \code{morie_dcc_multivariate_garch} returns.
#' @references Engle, R. F. (2002). Dynamic conditional correlation: a
#'   simple class of multivariate generalized autoregressive
#'   conditional heteroskedasticity models. \emph{JBES}, 20(3),
#'   339-350.
#' @examples
#' \donttest{
#' set.seed(1)
#' morie_dcc_garch(matrix(rnorm(200), ncol = 2))$a
#' }
#' @export
morie_dcc_garch <- function(x) {
  morie_dcc_multivariate_garch(x)
}
