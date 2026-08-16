# morie.fn -- function file (rootcoder007/morie)
# UMAP: uniform manifold approximation and projection.
#
# McInnes, L., Healy, J., & Melville, J. (2018) "UMAP: Uniform
# Manifold Approximation and Projection for Dimension Reduction",
# arXiv:1802.03426.
#
# The paper's Algorithm 1 has four parts, and all four are here.
#
# Local fuzzy simplicial sets (Algorithm 2): for each point take its
# n_neighbors nearest neighbours, let rho_i be the distance to the
# NEAREST one, and give the edge to neighbour j the membership
#   mu_{i->j} = exp(-max(0, d(x_i, x_j) - rho_i) / sigma_i).
# Subtracting rho_i is what makes the construction local.
#
# The smooth k-NN distance (Algorithm 3): sigma_i is found by binary
# search so that sum_j exp(-(d_ij - rho_i)/sigma_i) = log2(n), fixing
# the fuzzy cardinality of each neighbourhood rather than its radius.
# If every neighbour is the same distance away the equation has no
# solution and the search returns the scale floor.
#
# Symmetrisation: B = A + A' - A o A' (probabilistic t-conorm).
#
# Layout: initialise with a spectral embedding of the graph
# (Algorithm 4), then minimise the fuzzy cross entropy by SGD with
# the paper's attractive and repulsive forces, eps = 0.001 as in the
# reference implementation, learning rate annealed to zero. a and b
# are fitted so that (1 + a d^(2b))^-1 matches the piecewise target.
#
# A misprint in the paper: Algorithm 4 writes the symmetric normalised
# Laplacian as L = D^(1/2)(D - A)D^(1/2), which scales UP by the
# degrees. The intended operator, used here, is
# L = D^(-1/2)(D - A)D^(-1/2); laplacian="as_printed" gives the
# literal formula so the difference can be measured.

.scumap_EPS <- 0.001

#' .scumap_matrix
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_fuzzy_simplicial_set}, \code{morie_scumap_umap_singlecell}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @return The value of \code{M}, as built in the body.
#' @export
.scumap_matrix <- function(X) {
  M <- as.matrix(X)
  storage.mode(M) <- "double"
  if (nrow(M) == 0L) {
    stop("scumap: X is empty")
  }
  if (ncol(M) == 0L) {
    stop("scumap: X has no columns")
  }
  if (any(!is.finite(M))) {
    stop("scumap: X contains a non-finite value")
  }
  M
}

#' .scumap_dist
#'
#' A step of the scumap_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.scumap_dist <- function(a, b) {
  sqrt(sum((a - b) ^ 2))
}

#' morie_scumap_smooth_knn_dist
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_fuzzy_simplicial_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param distances Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_neighbors Coerced to integer by the body, with \code{as.integer}.
#' @param rho Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param tol Passed to \code{<}. Defaults to \code{1e-05}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{64}.
#' @param min_scale Numeric; combined arithmetically in the body. Defaults to \code{0.001}.
#' @return A list with \code{sigma}, \code{rho}.
#' @export
morie_scumap_smooth_knn_dist <- function(distances, n_neighbors,
                                         rho=NULL, tol=1e-5, max_iter=64,
                                         min_scale=1e-3) {
  # Algorithm 3: the sigma that fixes the fuzzy cardinality. Binary
  # search for sigma with sum_j exp(-(d_j - rho)/sigma) = log2(n).
  # Returns list(sigma, rho).
  d <- sort(as.numeric(distances))
  if (length(d) == 0L) {
    stop("scumap: no distances to smooth over")
  }
  n <- as.integer(n_neighbors)
  if (n < 2L) {
    stop("scumap: n_neighbors must be at least 2")
  }
  target <- log2(n)
  if (is.null(rho)) {
    nz <- d[d > 0.0]
    rho <- if (length(nz) > 0L) nz[1L] else 0.0
  }
  lo <- 0.0
  hi <- Inf
  mid <- 1.0
  for (it in seq_len(as.integer(max_iter))) {
    total <- sum(exp(-pmax(0.0, d - rho) / mid))
    if (abs(total - target) < tol) {
      break
    }
    if (total > target) {
      hi <- mid
      mid <- (lo + hi) / 2.0
    } else {
      lo <- mid
      mid <- if (is.infinite(hi)) mid * 2.0 else (lo + hi) / 2.0
    }
  }
  # the reference implementation floors sigma relative to the local
  # scale so that a duplicated point cannot drive it to zero
  mean_d <- mean(d)
  if (rho > 0.0) {
    mid <- max(mid, min_scale * mean_d)
  } else if (mean_d > 0.0) {
    mid <- max(mid, min_scale * mean_d)
  }
  list(sigma=mid, rho=rho)
}

#' morie_scumap_fuzzy_simplicial_set
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.scumap_matrix}.
#' @param n_neighbors Coerced to integer by the body, with \code{as.integer}. Defaults to \code{15}.
#' @param symmetrize A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{A}, \code{B}, \code{rho}, \code{sigma}, \code{neighbours}, \code{n}.
#' @export
morie_scumap_fuzzy_simplicial_set <- function(X, n_neighbors=15,
                                              symmetrize=TRUE) {
  # Algorithms 2 and 3 plus the t-conorm: the weighted UMAP graph.
  # Returns a list with the directed memberships A, the symmetrised
  # B (A + A' - A o A'), and the per-point rho and sigma. Neighbour
  # indices are 1-based.
  M <- .scumap_matrix(X)
  n <- nrow(M)
  k <- as.integer(n_neighbors)
  if (k < 2L) {
    stop("scumap: n_neighbors must be at least 2")
  }
  if (k >= n) {
    stop(sprintf(paste0("scumap: n_neighbors (%d) must be smaller than ",
                        "the number of points (%d)"), k, n))
  }
  A <- matrix(0.0, n, n)
  rhos <- numeric(n)
  sigmas <- numeric(n)
  neighbours <- list()
  for (i in seq_len(n)) {
    dd <- sqrt(rowSums(sweep(M, 2L, M[i, ]) ^ 2))
    others <- setdiff(seq_len(n), i)
    ord <- others[order(dd[others])][seq_len(k)]
    dists <- dd[ord]
    sk <- morie_scumap_smooth_knn_dist(dists, k)
    rhos[i] <- sk$rho
    sigmas[i] <- sk$sigma
    neighbours[[i]] <- ord
    A[i, ord] <- exp(-pmax(0.0, dists - sk$rho) / sk$sigma)
  }
  if (!isTRUE(symmetrize)) {
    return(list(A=A, B=A, rho=rhos, sigma=sigmas,
                neighbours=neighbours, n=n))
  }
  B <- A + t(A) - A * t(A)
  list(A=A, B=B, rho=rhos, sigma=sigmas, neighbours=neighbours, n=n)
}

#' morie_scumap_spectral_layout
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param B A matrix; indexed by row and column.
#' @param n_components Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param laplacian One of \code{"as_printed"}, \code{"normalised"}. Defaults to \code{"normalised"}.
#' @return The value of \code{Y}, as built in the body.
#' @export
morie_scumap_spectral_layout <- function(B, n_components=2,
                                         laplacian="normalised") {
  # Algorithm 4: initialise from the graph Laplacian's eigenvectors.
  # laplacian="normalised" uses D^(-1/2)(D-A)D^(-1/2); "as_printed"
  # uses the paper's literal D^(1/2)(D-A)D^(1/2), a misprint.
  if (!(laplacian %in% c("normalised", "as_printed"))) {
    stop("scumap: laplacian must be 'normalised' or 'as_printed'")
  }
  B <- as.matrix(B)
  n <- nrow(B)
  d <- rowSums(B)
  L <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      lij <- (if (i == j) d[i] else 0.0) - B[i, j]
      if (laplacian == "normalised") {
        s <- sqrt(d[i]) * sqrt(d[j])
        L[i, j] <- if (s > 0) lij / s else 0.0
      } else {
        L[i, j] <- sqrt(d[i]) * lij * sqrt(d[j])
      }
    }
  }
  eg <- eigen(L, symmetric=TRUE)
  # eigen() returns eigenvalues in decreasing order; ascend them and
  # skip the trivial first eigenvector
  asc <- rev(seq_len(n))
  vecs <- eg$vectors[, asc, drop=FALSE]
  nc <- as.integer(n_components)
  Y <- matrix(0.0, n, nc)
  avail <- min(nc, n - 1L)
  if (avail > 0L) {
    Y[, seq_len(avail)] <- vecs[, 1L + seq_len(avail), drop=FALSE]
  }
  # scale to a sensible starting spread, as the reference does
  span <- 0.0
  for (cc in seq_len(nc)) {
    span <- max(span, max(Y[, cc]) - min(Y[, cc]))
  }
  if (span > 0) {
    Y <- 10.0 * Y / span
  }
  Y
}

#' morie_scumap_fit_ab
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param min_dist Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param spread Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param n_grid A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @return A list with \code{a}, \code{b}.
#' @export
morie_scumap_fit_ab <- function(min_dist=0.1, spread=1.0, n_grid=300,
                                iters=200) {
  # Fit a, b so that (1 + a d^(2b))^-1 matches the target curve
  # (1 inside min_dist, exponential decay outside).
  if (min_dist < 0) {
    stop("scumap: min_dist must be non-negative")
  }
  if (spread <= 0) {
    stop("scumap: spread must be positive")
  }
  n_grid <- as.integer(n_grid)
  xs <- 3.0 * spread * (seq_len(n_grid) - 1L) / (n_grid - 1)
  ys <- ifelse(xs < min_dist, 1.0, exp(-(xs - min_dist) / spread))
  pos <- xs > 0
  loss <- function(a, b) {
    sum((1.0 / (1.0 + a * xs[pos] ^ (2 * b)) - ys[pos]) ^ 2)
  }
  a <- 1.0
  b <- 1.0
  step <- 0.5
  moves <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1), c(1, 1), c(-1, -1))
  for (it in seq_len(as.integer(iters))) {
    best <- list(v=loss(a, b), a=a, b=b)
    for (mv in moves) {
      na <- a + mv[1L] * step
      nb <- b + mv[2L] * step
      if (na <= 0 || nb <= 0) {
        next
      }
      v <- loss(na, nb)
      if (v < best$v) {
        best <- list(v=v, a=na, b=nb)
      }
    }
    if (best$a == a && best$b == b) {
      step <- step / 2.0
      if (step < 1e-6) {
        break
      }
    } else {
      a <- best$a
      b <- best$b
    }
  }
  list(a=a, b=b)
}

#' Exact 31-bit LCG in doubles: split the state so no intermediate
#'
#' product exceeds 2^53.
#'
#' @param seed Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{function}.
#' @export
.scumap_rng <- function(seed) {
  # Exact 31-bit LCG in doubles: split the state so no intermediate
  # product exceeds 2^53.
  st <- as.numeric(seed) %% 2147483648
  if (st == 0) {
    st <- 1
  }
  function() {
    hi <- st %/% 65536
    lo <- st %% 65536
    st <<- ((((1103515245 * hi) %% 2147483648) * 65536) %% 2147483648 +
            1103515245 * lo + 12345) %% 2147483648
    st / 2147483648
  }
}

#' The reference implementation clips gradients to +/- 4
#'
#' A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{>}.
#' @param lim Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @return The value of \code{v}, as built in the body.
#' @export
.scumap_clip <- function(v, lim=4.0) {
  # The reference implementation clips gradients to +/- 4.
  if (v > lim) {
    return(lim)
  }
  if (v < -lim) {
    return(-lim)
  }
  v
}

#' morie_scumap_umap_singlecell
#'
#' A step of the scumap_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.scumap_matrix}.
#' @param n_neighbors Passed to \code{morie_scumap_fuzzy_simplicial_set}. Defaults to \code{15}.
#' @param min_dist Passed to \code{morie_scumap_fit_ab}. Defaults to \code{0.1}.
#' @param n_components Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param n_epochs A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @param learning_rate Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param spread Passed to \code{morie_scumap_fit_ab}. Defaults to \code{1}.
#' @param negative_sample_rate Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.
#' @param init One of \code{"random"}, \code{"spectral"}. Defaults to \code{"spectral"}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param laplacian Passed to \code{morie_scumap_spectral_layout}. Defaults to \code{"normalised"}.
#' @param a Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param b Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{estimate}, \code{embedding}, \code{graph}, \code{directed_graph}, \code{rho}, \code{sigma}, \code{neighbours}, \code{a}, \code{b}, \code{n_neighbors}, \code{min_dist}, \code{n_components}, \code{n_epochs}, \code{init}, \code{laplacian}, \code{n}, \code{method}, \code{note}.
#' @export
morie_scumap_umap_singlecell <- function(X, n_neighbors=15, min_dist=0.1,
                                         n_components=2, n_epochs=200,
                                         learning_rate=1.0, spread=1.0,
                                         negative_sample_rate=5,
                                         init="spectral", seed=0,
                                         laplacian="normalised",
                                         a=NULL, b=NULL) {
  # UMAP embedding of X (McInnes, Healy & Melville 2018). init is
  # "spectral" (Algorithm 4, the paper's recommendation) or "random".
  # a and b default to the fit against min_dist and spread.
  M <- .scumap_matrix(X)
  n <- nrow(M)
  if (!(init %in% c("spectral", "random"))) {
    stop("scumap: init must be 'spectral' or 'random'")
  }
  if (n_components < 1) {
    stop("scumap: n_components must be at least 1")
  }
  if (learning_rate <= 0) {
    stop("scumap: learning_rate must be positive")
  }
  if (n_epochs < 1) {
    stop("scumap: n_epochs must be at least 1")
  }
  graph <- morie_scumap_fuzzy_simplicial_set(M, n_neighbors)
  B <- graph$B
  if (is.null(a) || is.null(b)) {
    ab <- morie_scumap_fit_ab(min_dist, spread)
    a <- ab$a
    b <- ab$b
  }
  d <- as.integer(n_components)
  rnd <- .scumap_rng(as.numeric(seed) + 1)
  if (init == "spectral") {
    Y <- morie_scumap_spectral_layout(B, d, laplacian)
  } else {
    Y <- matrix(0.0, n, d)
    for (i in seq_len(n)) {
      for (cc in seq_len(d)) {
        Y[i, cc] <- 20.0 * (rnd() - 0.5)
      }
    }
  }
  ei <- integer(0)
  ej <- integer(0)
  ew <- numeric(0)
  for (i in seq_len(n)) {
    for (j in seq.int(i + 1L, length.out=max(0L, n - i))) {
      if (B[i, j] > 0.0) {
        ei <- c(ei, i)
        ej <- c(ej, j)
        ew <- c(ew, B[i, j])
      }
    }
  }
  if (length(ew) == 0L) {
    stop("scumap: the fuzzy graph has no edges")
  }
  w_max <- max(ew)
  n_epochs <- as.integer(n_epochs)
  for (epoch in seq_len(n_epochs) - 1L) {
    alpha <- learning_rate * (1.0 - epoch / n_epochs)
    for (e in seq_along(ew)) {
      if (rnd() > ew[e] / w_max) {  # sample edges by membership
        next
      }
      i <- ei[e]
      j <- ej[e]
      diff <- Y[i, ] - Y[j, ]
      dist2 <- sum(diff * diff)
      if (dist2 > 0.0) {
        coeff <- (-2.0 * a * b * dist2 ^ (b - 1.0)) /
          (1.0 + a * dist2 ^ b)
      } else {
        coeff <- 0.0
      }
      for (cc in seq_len(d)) {
        g <- .scumap_clip(coeff * diff[cc])
        Y[i, cc] <- Y[i, cc] + alpha * g
        Y[j, cc] <- Y[j, cc] - alpha * g
      }
      for (ns in seq_len(as.integer(negative_sample_rate))) {
        k0 <- as.integer(rnd() * n)
        if (k0 == i - 1L || k0 >= n) {
          next
        }
        kk <- k0 + 1L
        diff <- Y[i, ] - Y[kk, ]
        dist2 <- sum(diff * diff)
        if (dist2 > 0.0) {
          coeff <- (2.0 * b) /
            ((.scumap_EPS + dist2) * (1.0 + a * dist2 ^ b))
        } else if (i != kk) {
          coeff <- 0.0
        } else {
          next
        }
        for (cc in seq_len(d)) {
          g <- if (dist2 > 0.0) .scumap_clip(coeff * diff[cc]) else 4.0
          Y[i, cc] <- Y[i, cc] + alpha * g
        }
      }
    }
  }
  list(
    estimate=Y,
    embedding=Y,
    graph=B,
    directed_graph=graph$A,
    rho=graph$rho,
    sigma=graph$sigma,
    neighbours=graph$neighbours,
    a=a,
    b=b,
    n_neighbors=as.integer(n_neighbors),
    min_dist=as.numeric(min_dist),
    n_components=d,
    n_epochs=n_epochs,
    init=init,
    laplacian=laplacian,
    n=n,
    method=sprintf(paste0("UMAP (McInnes, Healy & Melville 2018): fuzzy ",
                          "simplicial sets, t-conorm symmetrisation, %s ",
                          "initialisation, cross-entropy SGD"), init),
    note=paste0("distances are Euclidean and neighbours are found ",
                "exactly, not approximately, so this is O(n^2) and ",
                "meant for the sample sizes an anchor can check; the ",
                "paper's Algorithm 4 misprints the normalised ",
                "Laplacian, see laplacian=")
  )
}

#' morie_scumap_cheatsheet
#'
#' A step of the scumap_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_scumap_cheatsheet <- function() {
  paste0(
    "scumap: UMAP (McInnes, Healy & Melville 2018). Membership ",
    "exp(-max(0, d - rho)/sigma) to each of the n_neighbors ",
    "nearest points, rho the nearest-neighbour distance and ",
    "sigma solved so the memberships sum to log2(n_neighbors); ",
    "symmetrise by the t-conorm B = A + A' - A.A'; initialise ",
    "from the normalised-Laplacian eigenvectors; then SGD on the ",
    "fuzzy cross entropy with the paper's attractive and ",
    "repulsive forces, a and b fitted to min_dist."
  )
}

# compact aliases per ledger/NAMING.md
morie_scumap_umapsinglecell <- morie_scumap_umap_singlecell

#' @export
morie_scumap <- morie_scumap_umap_singlecell
