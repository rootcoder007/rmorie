# Reference: Cuesta-Albertos, J. A., Gordaliza, A., & Matrán, C. (1997)
# "Trimmed k-Means: An Attempt to Robustify Quantizers",
# The Annals of Statistics 25(2), 553-576.

.PENALTIES_RKMEANS <- c("square", "absolute", "huber")

#' .rkmeans_phi
#'
#' A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param t Numeric; combined arithmetically in the body.
#' @param penalty One of \code{"absolute"}, \code{"square"}.
#' @param c_val Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.rkmeans_phi <- function(t, penalty, c_val) {
  if (penalty == "square") {
    return(t * t)
  }
  if (penalty == "absolute") {
    return(t)
  }
  if (t <= c_val) {
    return(t * t)
  }
  c_val * (2.0 * t - c_val)
}

#' .rkmeans_dist
#'
#' A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}, \code{.rkmeans_huber_centre}, \code{.rkmeans_spatial_median}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param m Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.rkmeans_dist <- function(x, m) {
  d <- x - m
  sqrt(sum(d * d))
}

#' .rkmeans_mean
#'
#' A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}, \code{.rkmeans_huber_centre}, \code{.rkmeans_spatial_median}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param pts A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.rkmeans_mean <- function(pts) {
  p <- length(pts[[1]])
  out <- numeric(p)
  for (x in pts) {
    out <- out + x
  }
  out / length(pts)
}

#' .rkmeans_spatial_median
#'
#' A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param pts Passed to \code{.rkmeans_mean}.
#' @param tol Defaults to \code{1e-10}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @return The value of \code{m}, as built in the body.
#' @export
.rkmeans_spatial_median <- function(pts, tol = 1e-10, max_iter = 200) {
  m <- .rkmeans_mean(pts)
  for (iter in seq_len(max_iter)) {
    num <- numeric(length(m))
    den <- 0.0
    coincident <- FALSE
    for (x in pts) {
      d <- .rkmeans_dist(x, m)
      if (d < 1e-12) {
        coincident <- TRUE
        next
      }
      w <- 1.0 / d
      den <- den + w
      num <- num + w * x
    }
    if (den <= 0.0) {
      return(m)
    }
    new_m <- num / den
    shift <- .rkmeans_dist(new_m, m)
    m <- new_m
    if (shift < tol && !coincident) {
      break
    }
  }
  m
}

#' .rkmeans_huber_centre
#'
#' A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param pts Passed to \code{.rkmeans_mean}.
#' @param c_val Numeric; combined arithmetically in the body.
#' @param tol Defaults to \code{1e-10}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @return The value of \code{m}, as built in the body.
#' @export
.rkmeans_huber_centre <- function(pts, c_val, tol = 1e-10, max_iter = 200) {
  m <- .rkmeans_mean(pts)
  for (iter in seq_len(max_iter)) {
    num <- numeric(length(m))
    den <- 0.0
    for (x in pts) {
      d <- .rkmeans_dist(x, m)
      w <- if (d <= c_val) 1.0 else c_val / d
      den <- den + w
      num <- num + w * x
    }
    if (den <= 0.0) {
      return(m)
    }
    new_m <- num / den
    shift <- .rkmeans_dist(new_m, m)
    m <- new_m
    if (shift < tol) {
      break
    }
  }
  m
}

#' .rkmeans_concentrate
#'
#' A step of the rkmeans_native implementation. Called by \code{morie_rkmeans}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param rows A vector; its length is taken and its elements indexed.
#' @param cen A vector; indexed elementwise.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @param n_keep A count; the body uses it as \code{seq_len(...)}.
#' @param penalty One of \code{"absolute"}, \code{"square"}.
#' @param huber_c Passed to \code{.rkmeans_phi}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{list}.
#' @export
.rkmeans_concentrate <- function(rows, cen, k, n_keep, penalty, huber_c, max_iter) {
  n <- length(rows)
  prev <- NULL
  labels <- rep(-1L, n)
  kept <- integer(0)
  dists <- numeric(n)
  crit <- Inf
  for (iter in seq_len(max_iter)) {
    best_j <- integer(n)
    for (i in seq_len(n)) {
      bd <- Inf
      bj <- 1L
      for (j in seq_len(k)) {
        d <- .rkmeans_dist(rows[[i]], cen[[j]])
        if (d < bd) {
          bd <- d
          bj <- j
        }
      }
      dists[i] <- bd
      best_j[i] <- bj - 1L
    }
    phi_vals <- numeric(n)
    for (i in seq_len(n)) {
      phi_vals[i] <- .rkmeans_phi(dists[i], penalty, huber_c)
    }
    scores_order <- order(phi_vals, seq_len(n) - 1L)
    scores <- scores_order - 1L
    kept <- sort(scores[seq_len(n_keep)])
    labels <- rep(-1L, n)
    labels[kept + 1] <- best_j[kept + 1]
    phi_sum <- sum(phi_vals[kept + 1])
    crit <- phi_sum / n_keep
    if (!is.null(prev) && identical(labels, prev$labels) && identical(kept, prev$kept)) {
      break
    }
    prev <- list(labels = labels, kept = kept)
    for (j in seq_len(k)) {
      pts <- list()
      for (idx in kept) {
        if (labels[idx + 1] == j - 1L) {
          pts[[length(pts) + 1]] <- rows[[idx + 1]]
        }
      }
      if (length(pts) == 0) {
        worst <- kept[which.max(dists[kept + 1])]
        cen[[j]] <- rows[[worst + 1]]
        next
      }
      if (penalty == "square") {
        cen[[j]] <- .rkmeans_mean(pts)
      } else if (penalty == "absolute") {
        cen[[j]] <- .rkmeans_spatial_median(pts)
      } else {
        cen[[j]] <- .rkmeans_huber_centre(pts, huber_c)
      }
    }
  }
  list(crit, cen, labels, kept, dists)
}

#' morie_rkmeans
#'
#' A step of the rkmeans_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param k A count; the body uses it as \code{integer(...)}. Defaults to \code{2}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param penalty Compared against \code{"huber"}. Defaults to \code{"square"}.
#' @param n_start Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param huber_c Passed to \code{.rkmeans_concentrate}. Defaults to \code{1.345}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param centers Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @return A list with \code{estimate}, \code{centers}, \code{labels}, \code{kept}, \code{outliers}, \code{criterion}, \code{distances}, \code{sizes}, \code{n_trimmed}, \code{n_kept}, \code{alpha}, \code{k}, \code{penalty}, \code{method}.
#' @export
morie_rkmeans <- function(X, k = 2, alpha = 0.1, penalty = "square",
                          n_start = 20, max_iter = 100,
                          huber_c = 1.345, seed = 0, centers = NULL) {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("rkmeans: X must be a non-empty (n, p) matrix")
  }
  rows <- lapply(seq_len(nrow(X)), function(i) as.numeric(X[i, ]))
  p <- ncol(X)
  n <- nrow(X)
  k <- as.integer(k)
  if (k < 1) stop("rkmeans: k must be >= 1")
  if (k > n) stop(sprintf("rkmeans: k = %d exceeds n = %d", k, n))
  alpha <- as.numeric(alpha)
  if (alpha < 0.0 || alpha >= 1.0) {
    stop(sprintf("rkmeans: alpha must lie in [0, 1), got %g", alpha))
  }
  if (!(penalty %in% .PENALTIES_RKMEANS)) {
    stop(sprintf("rkmeans: penalty must be one of (%s), got %s",
                 paste(.PENALTIES_RKMEANS, collapse = ", "), penalty))
  }
  huber_c <- as.numeric(huber_c)
  if (penalty == "huber" && !(huber_c > 0.0)) {
    stop("rkmeans: huber_c must be > 0")
  }
  n_keep <- as.integer(ceiling(n * (1.0 - alpha)))
  if (n_keep < k) {
    stop(sprintf("rkmeans: alpha = %g keeps only %d points, fewer than k = %d",
                 alpha, n_keep, k))
  }
  e <- .ghc_rng(seed)
  starts <- list()
  if (!is.null(centers)) {
    if (is.data.frame(centers)) centers <- as.matrix(centers)
    if (!is.matrix(centers)) centers <- as.matrix(centers)
    storage.mode(centers) <- "double"
    if (nrow(centers) != k || ncol(centers) != p) {
      stop("rkmeans: centers must be (k, p)")
    }
    c0 <- lapply(seq_len(nrow(centers)), function(i) as.numeric(centers[i, ]))
    starts[[length(starts) + 1]] <- c0
  }
  n_start_int <- max(1L, as.integer(n_start))
  for (s in seq_len(n_start_int)) {
    idx <- integer(0)
    repeat {
      u <- .ghc_unif(e, 1L)
      cand <- as.integer(u * n)
      idx <- unique(c(idx, cand))
      if (length(idx) >= k) break
    }
    starts[[length(starts) + 1]] <- lapply(sort(idx), function(i) rows[[i + 1]])
  }
  best <- NULL
  for (init in starts) {
    got <- .rkmeans_concentrate(rows, init, k, n_keep, penalty, huber_c,
                                as.integer(max_iter))
    if (is.null(best) || got[[1]] < best[[1]]) {
      best <- got
    }
  }
  crit <- best[[1]]
  cen <- best[[2]]
  labels <- best[[3]]
  kept <- best[[4]]
  dists <- best[[5]]
  sizes <- integer(k)
  for (idx in kept) {
    cluster <- labels[idx + 1]
    sizes[cluster + 1] <- sizes[cluster + 1] + 1L
  }
  outliers <- which(labels < 0) - 1L
  list(
    estimate = cen,
    centers = cen,
    labels = as.integer(labels),
    kept = as.integer(kept),
    outliers = as.integer(outliers),
    criterion = as.numeric(crit),
    distances = as.numeric(dists),
    sizes = as.integer(sizes),
    n_trimmed = length(outliers),
    n_kept = length(kept),
    alpha = alpha,
    k = k,
    penalty = penalty,
    method = "trimmed k-means (Cuesta-Albertos et al. 1997)"
  )
}

#' .rkmeans_cheatsheet
#'
#' A step of the rkmeans_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.rkmeans_cheatsheet <- function() {
  paste0("rkmeans: impartially alpha-trimmed k-Phi-means ",
         "(Cuesta-Albertos, Gordaliza & Matran 1997). Minimises ",
         "V = (1/P(A)) int_A Phi(d(x, M)) dP over k-sets M AND ",
         "trimming sets A with P(A) >= 1-alpha -- the data choose ",
         "what to discard, not the analyst. Phi in {square (k-means), ",
         "absolute (k-medians), huber}. Corollary 3.2: hard trimming ",
         "is optimal. Trimmed points are returned as outliers.")
}

morie_trimmed_kmeans <- morie_rkmeans
morie_trimmedkmeans <- morie_rkmeans
