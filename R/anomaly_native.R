# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Anomaly-detection shelf. R mirrors of the morie.fn Python modules
# abod, ecod, hbos, isof, lof, mcdAnm and jooutl, ported at full
# precision from the Python rather than from the papers, so the two
# languages agree to machine precision wherever the method is
# deterministic.
#
# The two randomised members (isolation forest, MCD) cannot agree
# numerically across languages -- R and numpy have different generators
# -- so their parity tests assert the invariants instead: score ranges,
# ranking of planted outliers, and the shape of the returned pieces.

#' Angle-based outlier detection (ABOD)
#'
#' The variance of the cosine-weighted angles a point subtends with all
#' pairs of other points. For a point inside the cloud the angles vary
#' widely; for a point far outside, every other point lies in roughly
#' the same direction and the variance collapses. LOW variance means
#' outlying, so the reported \code{score} negates the ABOF.
#'
#' Angles keep working where distances stop: in high dimensions all
#' pairwise distances concentrate, and that is what ABOD was built for.
#'
#' @param X numeric matrix, one row per observation.
#' @param k optional number of nearest neighbours to approximate over.
#'   The exact version is O(n^3); \code{k} makes it O(n k^2). NULL uses
#'   every other point.
#' @return list with \code{abof}, \code{score} (negated ABOF),
#'   \code{rank} (0-based, most outlying first) and \code{k}.
#' @references Kriegel, H.-P., Schubert, M. and Zimek, A. (2008).
#'   Angle-based outlier detection in high-dimensional data. \emph{KDD}.
#' @examples
#' X <- rbind(matrix(rnorm(60), ncol = 2), c(9, 9))
#' which.min(morie_abod(X)$abof)   # the planted point
#' @export
morie_abod <- function(X, k = NULL) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  if (n < 3L) stop("need at least 3 points to form an angle", call. = FALSE)
  nbrs <- NULL
  if (!is.null(k)) {
    k <- as.integer(k)
    if (k < 2L || k > n - 1L) {
      stop(sprintf("k must be between 2 and %d", n - 1L), call. = FALSE)
    }
    D <- as.matrix(stats::dist(X))
    diag(D) <- Inf
    nbrs <- t(apply(D, 1L, function(r) order(r)[seq_len(k)]))
    if (k == 1L) nbrs <- matrix(nbrs, ncol = 1L)
  }
  abof <- numeric(n)
  for (i in seq_len(n)) {
    idx <- if (is.null(k)) setdiff(seq_len(n), i) else nbrs[i, ]
    V <- sweep(X[idx, , drop = FALSE], 2L, X[i, ], "-")
    nrm2 <- rowSums(V^2)
    good <- nrm2 > 1e-24
    V <- V[good, , drop = FALSE]
    nrm2 <- nrm2[good]
    if (nrow(V) < 2L) {
      abof[i] <- 0
      next
    }
    G <- V %*% t(V)
    w <- 1 / outer(nrm2, nrm2)
    vals <- G * w
    iu <- which(upper.tri(w), arr.ind = TRUE)
    vv <- vals[iu]
    ww <- w[iu]
    wsum <- sum(ww)
    mu <- sum(ww * vv) / wsum
    abof[i] <- sum(ww * (vv - mu)^2) / wsum
  }
  score <- -abof
  rank <- integer(n)
  rank[order(-score)] <- seq_len(n) - 1L
  list(abof = abof, score = score, rank = rank, k = k,
       n = n, d = ncol(X),
       mode = if (is.null(k)) "exact" else "approximate",
       method = "abod")
}


#' ECOD -- empirical-CDF outlier detection
#'
#' Per dimension, the left and right empirical tail probabilities of
#' each observation; the score is the summed negative log tail. A third
#' "automatic" variant picks the tail per dimension by the sign of the
#' skewness. The reported score is the largest of the three.
#'
#' ECOD has NO hyperparameters, which is its selling point, but it
#' shares HBOS's blind spot exactly: it looks one dimension at a time,
#' so a point that is unremarkable in every margin and impossible in the
#' joint distribution scores as normal.
#'
#' @param X numeric matrix, one row per observation.
#' @return list with \code{score}, \code{rank}, \code{tail_left},
#'   \code{tail_right} and \code{skewness}.
#' @references Li, Z. et al. (2022). ECOD: unsupervised outlier
#'   detection using empirical cumulative distribution functions.
#'   \emph{IEEE TKDE}, 35(12), 12181-12193.
#' @examples
#' X <- cbind(rnorm(50), rnorm(50))
#' str(morie_ecod(X)$score)
#' @export
morie_ecod <- function(X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) == 1L && ncol(X) > 1L) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (n < 2L) stop("need at least 2 observations", call. = FALSE)
  left <- matrix(0, n, d)
  right <- matrix(0, n, d)
  skew <- numeric(d)
  for (j in seq_len(d)) {
    col <- X[, j]
    srt <- sort(col)
    # findInterval(x, srt) counts values <= x, matching numpy's
    # searchsorted(..., side = "right").
    left[, j] <- findInterval(col, srt) / n
    right[, j] <- (n - findInterval(col, srt, left.open = TRUE)) / n
    sdev <- sqrt(mean((col - mean(col))^2))
    skew[j] <- if (sdev == 0) 0 else mean(((col - mean(col)) / sdev)^3)
  }
  flr <- 1 / n
  lo <- rowSums(-log(pmax(left, flr)))
  hi <- rowSums(-log(pmax(right, flr)))
  auto_tail <- matrix(0, n, d)
  for (j in seq_len(d)) {
    auto_tail[, j] <- if (skew[j] < 0) left[, j] else right[, j]
  }
  auto <- rowSums(-log(pmax(auto_tail, flr)))
  score <- pmax(pmax(lo, hi), auto)
  rank <- integer(n)
  rank[order(-score)] <- seq_len(n) - 1L
  list(score = score, rank = rank, tail_left = left, tail_right = right,
       skewness = skew, n = n, d = d, method = "ecod")
}


#' HBOS -- histogram-based outlier score
#'
#' One histogram per feature; the score is the summed negative log
#' density of the bin each observation falls in. Because the features
#' are scored independently and added, HBOS is fast and completely blind
#' to dependence: a point at the centre of both margins but off the
#' correlation ridge is invisible to it.
#'
#' @param X numeric matrix, one row per observation.
#' @param bins number of bins per feature.
#' @param mode \code{"static"} for equal-width bins, \code{"dynamic"}
#'   for equal-count (quantile) bins.
#' @return list with \code{score}, \code{rank}, \code{densities} and
#'   \code{bin_edges}.
#' @references Goldstein, M. and Dengel, A. (2012). Histogram-based
#'   outlier score (HBOS). \emph{KI-2012 Poster and Demo Track}.
#' @examples
#' X <- cbind(rnorm(100), rnorm(100))
#' str(morie_hbos(X)$score)
#' @export
morie_hbos <- function(X, bins = 10, mode = c("static", "dynamic")) {
  mode <- match.arg(mode)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  bins <- as.integer(bins)
  if (bins < 1L) stop("bins must be at least 1", call. = FALSE)
  dens <- matrix(0, n, d)
  edges_all <- vector("list", d)
  for (j in seq_len(d)) {
    col <- X[, j]
    if (identical(mode, "dynamic")) {
      edges <- unique(stats::quantile(col, seq(0, 1, length.out = bins + 1L),
                                      type = 7L, names = FALSE))
      if (length(edges) < 2L) edges <- c(min(col) - 0.5, max(col) + 0.5)
    } else {
      lo <- min(col)
      hi <- max(col)
      if (lo == hi) {
        lo <- lo - 0.5
        hi <- hi + 0.5
      }
      edges <- seq(lo, hi, length.out = bins + 1L)
    }
    nb <- length(edges) - 1L
    # numpy's histogram is right-open except in the last bin, which is
    # closed; findInterval + a clamp reproduces that exactly.
    idx <- pmin(pmax(findInterval(col, edges), 1L), nb)
    counts <- tabulate(idx, nbins = nb)
    width <- diff(edges)
    p <- counts / (sum(counts) * ifelse(width > 0, width, 1))
    dens[, j] <- pmax(p[idx], 1e-12)
    edges_all[[j]] <- edges
  }
  score <- rowSums(-log(dens))
  rank <- integer(n)
  rank[order(-score)] <- seq_len(n) - 1L
  list(score = score, rank = rank, densities = dens, bin_edges = edges_all,
       mode = mode, bins = bins, n = n, d = d, method = "hbos")
}


#' Isolation forest
#'
#' Random axis-parallel splits; the depth at which a point becomes
#' isolated is the statistic. Anomalies are isolated in few splits, so
#' short average path length means high score. The normalisation
#' \code{c(psi)} is the average path length of an unsuccessful search in
#' a binary search tree, which is what makes scores comparable across
#' subsample sizes.
#'
#' The splits are axis-parallel, so structure at an angle is invisible:
#' points inside a tight diagonal band score as anomalous even though
#' they sit exactly on the data's own manifold.
#'
#' @param X numeric matrix, one row per observation.
#' @param n_trees number of trees.
#' @param sample_size subsample size per tree (capped at \code{nrow(X)}).
#'   Subsampling is not only for speed -- it reduces swamping.
#' @param seed integer seed. R's generator is not numpy's, so scores
#'   will not match the Python module value-for-value; the rankings do.
#' @return list with \code{score} (in (0, 1], 0.5 is the neutral point),
#'   \code{rank}, \code{path_length}.
#' @references Liu, F. T., Ting, K. M. and Zhou, Z.-H. (2008).
#'   Isolation forest. \emph{ICDM}, 413-422.
#' @examples
#' X <- rbind(matrix(rnorm(200), ncol = 2), c(8, 8))
#' which.max(morie_isolation_forest(X, n_trees = 50, seed = 1)$score)
#' @export
morie_isolation_forest <- function(X, n_trees = 100, sample_size = 256,
                                   seed = 0) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  n_trees <- as.integer(n_trees)
  if (n_trees < 1L) stop("n_trees must be at least 1", call. = FALSE)
  psi <- as.integer(min(sample_size, n))
  if (psi < 2L) stop("sample_size must be at least 2", call. = FALSE)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()),
          add = TRUE)
  set.seed(as.integer(seed))
  limit <- ceiling(log2(psi))

  cfun <- function(m) {
    if (m <= 1) return(0)
    2 * (log(m - 1) + 0.5772156649) - 2 * (m - 1) / m
  }

  path <- function(x, idx, depth, sub) {
    repeat {
      if (depth >= limit || length(idx) <= 1L) return(depth + cfun(length(idx)))
      block <- sub[idx, , drop = FALSE]
      lo <- apply(block, 2L, min)
      hi <- apply(block, 2L, max)
      wide <- which(hi > lo)
      if (length(wide) == 0L) return(depth + cfun(length(idx)))
      j <- if (length(wide) == 1L) wide else sample(wide, 1L)
      thr <- stats::runif(1L, lo[j], hi[j])
      nxt <- if (x[j] < thr) idx[sub[idx, j] < thr] else idx[sub[idx, j] >= thr]
      if (length(nxt) == 0L) return(depth + 1)
      idx <- nxt
      depth <- depth + 1
    }
  }

  lengths <- numeric(n)
  for (b in seq_len(n_trees)) {
    sub <- X[sample.int(n, psi, replace = FALSE), , drop = FALSE]
    idx0 <- seq_len(psi)
    for (i in seq_len(n)) {
      lengths[i] <- lengths[i] + path(X[i, ], idx0, 0, sub)
    }
  }
  lengths <- lengths / n_trees
  score <- 2^(-lengths / max(cfun(psi), 1e-12))
  rank <- integer(n)
  rank[order(-score)] <- seq_len(n) - 1L
  list(score = score, rank = rank, path_length = lengths, threshold = 0.5,
       n_trees = n_trees, sample_size = psi, n = n,
       axis_parallel_caveat = paste("splits are axis-parallel, so structure",
                                    "at an angle is invisible: points inside",
                                    "a tight diagonal band score as anomalous"),
       method = "isolation_forest")
}


#' Local outlier factor
#'
#' Each point's local reachability density against the mean density of
#' its k neighbours. A LOF near 1 is normal; well above 1 means the
#' point sits in a sparser region than its neighbours do.
#'
#' The relativity is the whole point and also the limitation: a
#' genuinely sparse but self-consistent cluster is not flagged, because
#' every member is exactly as sparse as its neighbours.
#'
#' @param X numeric matrix, one row per observation.
#' @param k neighbourhood size.
#' @return list with \code{lof}, \code{score} (same vector),
#'   \code{rank}, \code{lrd}, \code{k_distance} and \code{neighbors}
#'   (0-based indices, to match the Python module).
#' @references Breunig, M. M., Kriegel, H.-P., Ng, R. T. and Sander, J.
#'   (2000). LOF: identifying density-based local outliers.
#'   \emph{SIGMOD}, 93-104.
#' @examples
#' X <- rbind(matrix(rnorm(100), ncol = 2), c(6, 6))
#' round(max(morie_local_outlier_factor(X, k = 5)$lof), 3)
#' @export
morie_local_outlier_factor <- function(X, k = 20) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  k <- as.integer(k)
  if (k < 1L || k > n - 1L) {
    stop(sprintf("k must be between 1 and %d", n - 1L), call. = FALSE)
  }
  D <- as.matrix(stats::dist(X))
  diag(D) <- Inf
  ord <- t(apply(D, 1L, order))
  nbrs <- ord[, seq_len(k), drop = FALSE]
  kdist <- D[cbind(seq_len(n), ord[, k])]
  reach <- pmax(matrix(kdist[nbrs], n, k),
                matrix(D[cbind(rep(seq_len(n), k), as.vector(nbrs))], n, k))
  lrd <- 1 / pmax(rowMeans(reach), 1e-12)
  lof <- rowMeans(matrix(lrd[nbrs], n, k)) / pmax(lrd, 1e-12)
  rank <- integer(n)
  rank[order(-lof)] <- seq_len(n) - 1L
  list(lof = lof, score = lof, rank = rank, lrd = lrd, k_distance = kdist,
       neighbors = nbrs - 1L, k = k, n = n, method = "local_outlier_factor")
}


#' MCD-based robust Mahalanobis outlier detection
#'
#' Minimum covariance determinant by C-steps from random starts, then
#' Mahalanobis distances against the robust location and scatter,
#' compared with a chi-squared cutoff.
#'
#' This exists because of MASKING. The classical covariance is inflated
#' by the very outliers one is hunting, so their classical Mahalanobis
#' distances come out small and they hide themselves. Fitting the
#' covariance on a clean majority subset removes that feedback loop --
#' the returned \code{classical_distance} is there to make the
#' difference visible.
#'
#' @param X numeric matrix, one row per observation.
#' @param support_fraction fraction of points in the support h, in
#'   (0.5, 1]. Default 0.75.
#' @param n_trials number of random starts.
#' @param alpha tail probability for the chi-squared cutoff.
#' @param seed integer seed. R's generator is not numpy's, so the
#'   subsets differ; the fitted location and scatter agree to within
#'   sampling noise, and the outlier flags agree.
#' @return list with \code{distance}, \code{classical_distance},
#'   \code{outlier}, \code{location}, \code{covariance}, \code{cutoff}.
#' @references Rousseeuw, P. J. and Van Driessen, K. (1999). A fast
#'   algorithm for the minimum covariance determinant estimator.
#'   \emph{Technometrics}, 41(3), 212-223.
#' @examples
#' X <- rbind(matrix(rnorm(200), ncol = 2), cbind(rnorm(5, 6), rnorm(5, 6)))
#' sum(morie_mcd_outlier(X, seed = 1)$outlier)
#' @export
morie_mcd_outlier <- function(X, support_fraction = NULL, n_trials = 50,
                              alpha = 0.025, seed = 0) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  if (n <= p) {
    stop(sprintf("need more observations than dimensions (n=%d, p=%d)", n, p),
         call. = FALSE)
  }
  frac <- if (is.null(support_fraction)) 0.75 else as.numeric(support_fraction)
  if (frac <= 0.5 || frac > 1) {
    stop("support_fraction must be in (0.5, 1]", call. = FALSE)
  }
  h <- max(ceiling(frac * n), p + 1L)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()),
          add = TRUE)
  set.seed(as.integer(seed))

  maha <- function(Z, mu, Si) {
    C <- sweep(Z, 2L, mu, "-")
    rowSums((C %*% Si) * C)
  }
  best_det <- Inf
  best_mu <- NULL
  best_S <- NULL
  for (trial in seq_len(as.integer(n_trials))) {
    idx <- sample.int(n, h, replace = FALSE)
    for (step in seq_len(20L)) {
      mu <- colMeans(X[idx, , drop = FALSE])
      S <- stats::cov(X[idx, , drop = FALSE]) + 1e-9 * diag(p)
      Si <- tryCatch(solve(S), error = function(e) NULL)
      if (is.null(Si)) break
      d <- maha(X, mu, Si)
      new <- order(d)[seq_len(h)]
      if (identical(sort(new), sort(idx))) break
      idx <- new
    }
    det_val <- det(stats::cov(X[idx, , drop = FALSE]))
    if (det_val >= 0 && det_val < best_det) {
      best_det <- det_val
      best_mu <- colMeans(X[idx, , drop = FALSE])
      best_S <- stats::cov(X[idx, , drop = FALSE]) + 1e-9 * diag(p)
    }
  }
  if (is.null(best_mu)) {
    best_mu <- colMeans(X)
    best_S <- stats::cov(X)
  }
  Si <- .morie_ginv(best_S)
  d2 <- maha(X, best_mu, Si)
  cS <- stats::cov(X)
  cd2 <- maha(X, colMeans(X), .morie_ginv(cS))
  cut <- stats::qchisq(1 - alpha, df = p)
  out <- d2 > cut
  list(distance = sqrt(pmax(d2, 0)),
       classical_distance = sqrt(pmax(cd2, 0)),
       outlier = out, location = best_mu, covariance = best_S,
       cutoff = sqrt(cut), n_outliers = sum(out), h = as.integer(h),
       support_fraction = frac, n = n,
       cutoff_caveat = paste("the chi-squared cutoff is exact only",
                             "asymptotically and under normality"),
       method = "mcd_outlier")
}


#' Rolling-median outlier detection for a time series
#'
#' A centred rolling median and MAD; the score is the robust z-score
#' \eqn{|y - med| / (1.4826\,MAD)}. Using the median rather than the
#' mean is what keeps trend and seasonality from firing the detector.
#'
#' The documented failure: a RUN of consecutive outliers longer than
#' half the window becomes the local median itself and is declared
#' normal. The window has to be wider than the longest run one expects
#' to catch.
#'
#' @param y numeric series.
#' @param W half-window; the full window is \code{2 * W + 1}.
#' @param threshold robust z-score above which a point is flagged.
#' @return list with \code{outlier}, \code{score}, \code{rolling_median},
#'   \code{rolling_mad}.
#' @references Iglewicz, B. and Hoaglin, D. C. (1993). \emph{How to
#'   Detect and Handle Outliers}. ASQC Quality Press. (the 1.4826 MAD
#'   scaling and the 3.5 default)
#' @examples
#' y <- sin(seq(0, 6, length.out = 100)); y[50] <- 12
#' which(morie_joseph_ts_outlier_detection(y)$outlier)
#' @export
morie_joseph_ts_outlier_detection <- function(y, W = 10, threshold = 3.5) {
  y <- as.numeric(y)
  n <- length(y)
  W <- as.integer(W)
  if (W < 1L) stop("W must be at least 1", call. = FALSE)
  if (n < 2L * W + 1L) {
    stop(sprintf("series of length %d is too short for W=%d", n, W),
         call. = FALSE)
  }
  med <- numeric(n)
  mad <- numeric(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - W)
    hi <- min(n, i + W)
    win <- y[lo:hi]
    med[i] <- stats::median(win)
    mad[i] <- stats::median(abs(win - med[i]))
  }
  scale <- 1.4826 * mad
  score <- ifelse(scale > 0, abs(y - med) / scale, 0)
  score[!is.finite(score)] <- 0
  out <- score > threshold
  list(outlier = out, score = score, rolling_median = med,
       rolling_mad = mad, n_outliers = sum(out), W = W,
       threshold = as.numeric(threshold), n = n,
       run_caveat = paste("a run of consecutive outliers longer than half",
                          "the window becomes the local median and is",
                          "declared normal"),
       method = "joseph_ts_outlier_detection")
}
