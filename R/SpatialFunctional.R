# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial functional, topological, graph and multilevel methods.
# Identical to the Python arm morie.fn.spfunc.

#' Functional PCA, graph convolution and attention, persistent homology, possibilistic clustering, REML components
#'
#' \code{CurveFpca}: functional PCA on a common grid with trapezoid weights
#' (eigenfunctions of \code{W^(1/2) C W^(1/2)} rescaled to unit L2 norm,
#' largest value positive). \code{GcnLayer}: \code{sigma(D^(-1/2) (A + I) D^(-1/2) H W)}.
#' \code{SgcRidge}: ridge regression on \code{S^K X} (intercept unpenalised).
#' \code{GatLayer}: one-head graph attention with LeakyReLU scores and
#' softmax over neighbours. \code{RipsPersistence}: Vietoris-Rips persistence
#' in dimensions 0 and 1 by boundary-matrix reduction over Z/2 (rows are
#' dimension, birth, death; zero-persistence pairs dropped).
#' \code{PersistenceLandscape}: \code{lambda_k(t)}, the k-th largest tent
#' \code{max(0, min(t - b, d - t))}. \code{PossibilisticCmeans}: possibilistic
#' c-means with \code{omega} from fuzzy c-means (as ppclust::pcm).
#' \code{RemlComponents}: REML variance components by average-information
#' Newton steps with step halving and a boundary floor of \code{1e-10 var(y)}; \code{CrossedRandomEffects} and \code{NestedRandomEffects}
#' build the indicator designs.
#'
#' @param curves Matrix of curves (rows).
#' @param t Grid, or evaluation points for the landscape.
#' @param n_components Number of components.
#' @param A Adjacency matrix.
#' @param H Node features.
#' @param Wt Layer weights.
#' @param activation "relu", "tanh" or "linear".
#' @param self_loops Add self loops.
#' @param X Node features, data matrix, or fixed-effects design.
#' @param y Response.
#' @param k Number of propagation steps.
#' @param l2 Ridge penalty.
#' @param a Attention vector (length twice the output width), or factor codes.
#' @param negative_slope LeakyReLU slope.
#' @param points Point coordinates (rows) or a distance matrix.
#' @param max_dim Largest homology dimension (0 or 1).
#' @param max_scale Largest filtration value.
#' @param distance_matrix Treat \code{points} as distances.
#' @param diagram Persistence diagram rows.
#' @param k_max Number of landscape functions.
#' @param dimension Homology dimension to use, or NULL.
#' @param centers Initial prototypes.
#' @param m Fuzzifier.
#' @param omega Bandwidths, or NULL to derive them from fuzzy c-means.
#' @param K Omega multiplier.
#' @param max_iter Iteration limit.
#' @param con_val Convergence tolerance on the prototypes.
#' @param Zs List of random-effect design matrices.
#' @param tol Relative step tolerance.
#' @param b Second factor codes.
#' @return A list, matrix or vector.
#' @references Ramsay, J. O. and Silverman, B. W. (2005). Functional Data
#'   Analysis. Kipf, T. N. and Welling, M. (2017). ICLR 2017. Wu, F. et al.
#'   (2019). ICML 2019. Velickovic, P. et al. (2018). ICLR 2018. Edelsbrunner,
#'   H., Letscher, D. and Zomorodian, A. (2002). Discrete Comput. Geom. 28,
#'   511-533. Bubenik, P. (2015). JMLR 16, 77-102. Krishnapuram, R. and Keller,
#'   J. M. (1993). IEEE Trans. Fuzzy Systems 1, 98-110. Patterson, H. D. and
#'   Thompson, R. (1971). Biometrika 58, 545-554. Gilmour, A. R., Thompson, R.
#'   and Cullis, B. R. (1995). Biometrics 51, 1440-1450.
#' @examples
#' RipsPersistence(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$diagram
#' PersistenceLandscape(rbind(c(0, 2), c(1, 3)), c(0.5, 1, 1.5, 2), k_max = 2)
#' @export
CurveFpca <- function(curves, t, n_components = 2) {
  X <- unname(as.matrix(curves)) * 1
  N <- nrow(X)
  M <- length(t)
  w <- numeric(M)
  h <- diff(t)
  w[-M] <- w[-M] + h / 2
  w[-1] <- w[-1] + h / 2
  m <- colSums(X) / N
  Z <- sweep(X, 2, m)
  sw <- sqrt(w)
  C <- crossprod(Z) / (N - 1) * outer(sw, sw)
  e <- eigen(C, symmetric = TRUE)
  V <- e$vectors
  for (i in seq_len(ncol(V))) if (V[which.max(abs(V[, i])), i] < 0) V[, i] <- -V[, i]
  phis <- t(V[, seq_len(n_components), drop = FALSE] / sw)
  scores <- (Z * rep(w, each = N)) %*% t(phis)
  list(mean = m, eigenvalues = e$values[seq_len(n_components)], eigenfunctions = phis, scores = scores,
       explained = e$values[seq_len(n_components)] / sum(e$values[e$values > 0]))
}

.sf_norm_adj <- function(A, self_loops) {
  At <- unname(as.matrix(A)) * 1
  if (self_loops) At <- At + diag(nrow(At))
  d <- rowSums(At)
  At / sqrt(outer(d, d))
}

.sf_act <- function(v, activation) {
  if (activation == "relu") return(pmax(v, 0))
  if (activation == "tanh") return(tanh(v))
  v
}

#' @rdname CurveFpca
#' @export
GcnLayer <- function(A, H, Wt, activation = "relu", self_loops = TRUE) {
  .sf_act(.sf_norm_adj(A, self_loops) %*% as.matrix(H) %*% as.matrix(Wt), activation)
}

#' @rdname CurveFpca
#' @export
SgcRidge <- function(A, X, y, k = 2, l2 = 1, self_loops = TRUE) {
  S <- .sf_norm_adj(A, self_loops)
  F_ <- unname(as.matrix(X)) * 1
  for (r in seq_len(k)) F_ <- S %*% F_
  Fr <- cbind(1, F_)
  pen <- diag(c(0, rep(l2, ncol(F_))), ncol(Fr))
  beta <- as.numeric(solve(crossprod(Fr) + pen, crossprod(Fr, y)))
  list(coefficients = beta, features = F_, fitted = as.numeric(Fr %*% beta))
}

#' @rdname CurveFpca
#' @export
GatLayer <- function(A, H, Wt, a, negative_slope = 0.2, activation = "linear", self_loops = TRUE) {
  Z <- as.matrix(H) %*% as.matrix(Wt)
  n <- nrow(Z)
  f <- ncol(Z)
  att <- matrix(0, n, n)
  out <- matrix(0, n, f)
  for (i in seq_len(n)) {
    nb <- which((A[i, ] != 0 & seq_len(n) != i) | (self_loops & seq_len(n) == i))
    e <- sum(a[seq_len(f)] * Z[i, ]) + as.numeric(Z[nb, , drop = FALSE] %*% a[f + seq_len(f)])
    e <- ifelse(e > 0, e, negative_slope * e)
    ex <- exp(e - max(e))
    att[i, nb] <- ex / sum(ex)
    out[i, ] <- .sf_act(colSums(att[i, nb] * Z[nb, , drop = FALSE]), activation)
  }
  list(output = out, attention = att)
}

#' @rdname CurveFpca
#' @export
RipsPersistence <- function(points, max_dim = 1, max_scale = Inf, distance_matrix = FALSE) {
  D <- if (distance_matrix) unname(as.matrix(points)) * 1 else as.matrix(stats::dist(points))
  n <- nrow(D)
  val <- numeric(0)
  dim_ <- integer(0)
  verts <- list()
  for (i in seq_len(n)) {
    val <- c(val, 0)
    dim_ <- c(dim_, 0L)
    verts[[length(verts) + 1]] <- i
  }
  for (i in seq_len(n - 1)) for (j in (i + 1):n) if (D[i, j] <= max_scale) {
    val <- c(val, D[i, j])
    dim_ <- c(dim_, 1L)
    verts[[length(verts) + 1]] <- c(i, j)
  }
  if (max_dim >= 1 && n >= 3) for (i in 1:(n - 2)) for (j in (i + 1):(n - 1)) for (k in (j + 1):n) {
    v <- max(D[i, j], D[i, k], D[j, k])
    if (v <= max_scale) {
      val <- c(val, v)
      dim_ <- c(dim_, 2L)
      verts[[length(verts) + 1]] <- c(i, j, k)
    }
  }
  key <- vapply(verts, function(v) paste(sprintf("%06d", c(v, rep(0, 3 - length(v)))), collapse = ""), "")
  o <- order(val, dim_, key)
  val <- val[o]
  dim_ <- dim_[o]
  verts <- verts[o]
  key <- key[o]
  idx <- stats::setNames(seq_along(key), key)
  kf <- function(v) paste(sprintf("%06d", c(v, rep(0, 3 - length(v)))), collapse = "")
  cols <- lapply(seq_along(verts), function(q) {
    v <- verts[[q]]
    if (length(v) == 1) return(integer(0))
    sort(vapply(seq_along(v), function(r) idx[[kf(v[-r])]], 0L))
  })
  pivot_of <- integer(length(cols))
  pairs <- list()
  paired <- logical(length(cols))
  for (j in seq_along(cols)) {
    cc <- cols[[j]]
    while (length(cc) && pivot_of[cc[length(cc)]] > 0) {
      other <- cols[[pivot_of[cc[length(cc)]]]]
      cc <- sort(c(setdiff(cc, other), setdiff(other, cc)))
    }
    cols[[j]] <- cc
    if (length(cc)) {
      low <- cc[length(cc)]
      pivot_of[low] <- j
      paired[c(low, j)] <- TRUE
      if (val[j] > val[low]) pairs[[length(pairs) + 1]] <- c(dim_[low], val[low], val[j])
    }
  }
  for (q in seq_along(cols)) if (!paired[q] && !length(cols[[q]]) && dim_[q] <= max_dim) {
    pairs[[length(pairs) + 1]] <- c(dim_[q], val[q], Inf)
  }
  P <- if (length(pairs)) do.call(rbind, pairs) else matrix(0, 0, 3)
  P <- P[P[, 1] <= max_dim, , drop = FALSE]
  P <- P[order(P[, 1], P[, 2], P[, 3]), , drop = FALSE]
  colnames(P) <- c("dimension", "birth", "death")
  list(diagram = P)
}

#' @rdname CurveFpca
#' @export
PersistenceLandscape <- function(diagram, t, k_max = 3, dimension = NULL) {
  D <- as.matrix(diagram)
  if (ncol(D) == 3) {
    if (!is.null(dimension)) D <- D[D[, 1] == dimension, , drop = FALSE]
    D <- D[, 2:3, drop = FALSE]
  }
  D <- D[is.finite(D[, 2]), , drop = FALSE]
  out <- matrix(0, k_max, length(t))
  for (q in seq_along(t)) {
    v <- sort(pmax(0, pmin(t[q] - D[, 1], D[, 2] - t[q])), decreasing = TRUE)
    kk <- min(k_max, length(v))
    if (kk) out[seq_len(kk), q] <- v[seq_len(kk)]
  }
  out
}

#' @rdname CurveFpca
#' @export
PossibilisticCmeans <- function(X, centers, m = 2, omega = NULL, K = 1, max_iter = 1000, con_val = 1e-9) {
  X <- unname(as.matrix(X)) * 1
  v <- unname(as.matrix(centers)) * 1
  n <- nrow(X)
  k <- nrow(v)
  sqd <- function(v) outer(seq_len(n), seq_len(k), Vectorize(function(i, j) sum((X[i, ] - v[j, ])^2)))
  if (is.null(omega)) {
    prev <- NULL
    for (it in seq_len(max_iter)) {
      d <- sqd(v)
      u <- t(vapply(seq_len(n), function(i) {
        if (any(d[i, ] == 0)) return(as.numeric(d[i, ] == 0))
        vapply(seq_len(k), function(j) 1 / sum((d[i, j] / d[i, ])^(1 / (m - 1))), 0)
      }, numeric(k)))
      um <- u^m
      v <- crossprod(um, X) / colSums(um)
      if (!is.null(prev) && sum(abs(v - prev)) <= con_val) break
      prev <- v
    }
    d <- sqd(v)
    omega <- K * colSums(u^m * d) / colSums(u^m)
  }
  it <- 0
  change <- Inf
  tt <- matrix(0, n, k)
  while (change > con_val && it < max_iter) {
    it <- it + 1
    prev <- v
    d <- sqd(v)
    tt <- 1 / (1 + sweep(d, 2, omega, "/")^(1 / (m - 1)))
    tm <- tt^m
    v <- crossprod(tm, X) / colSums(tm)
    change <- sum(abs(v - prev))
  }
  list(centers = unname(v), typicality = tt, omega = as.numeric(omega), iterations = it)
}

#' @rdname CurveFpca
#' @export
RemlComponents <- function(y, X, Zs, max_iter = 200, tol = 1e-12) {
  y <- as.numeric(y)
  n <- length(y)
  X <- unname(as.matrix(X)) * 1
  p <- ncol(X)
  ZZ <- c(lapply(Zs, function(Z) tcrossprod(as.matrix(Z) * 1)), list(diag(n)))
  K <- length(ZZ)
  s <- rep(sum((y - mean(y))^2) / (n - 1) / K, K)
  pieces <- function(s) {
    V <- Reduce(`+`, Map(`*`, s, ZZ))
    Vi <- solve(V)
    VX <- Vi %*% X
    XVX <- crossprod(X, VX)
    XVXi <- solve(XVX)
    P <- Vi - VX %*% XVXi %*% t(VX)
    list(V = V, Vi = Vi, XVX = XVX, XVXi = XVXi, P = P, Py = as.numeric(P %*% y))
  }
  rll <- function(pc) {
    -0.5 * (as.numeric(determinant(pc$V)$modulus) + as.numeric(determinant(pc$XVX)$modulus) + sum(y * pc$Py)) -
      0.5 * (n - p) * log(2 * pi)
  }
  floor_ <- 1e-10 * sum((y - mean(y))^2) / (n - 1)
  pc <- pieces(s)
  ll <- rll(pc)
  it <- 0
  for (iter in seq_len(max_iter)) {
    it <- it + 1
    VkPy <- lapply(ZZ, function(M) as.numeric(M %*% pc$Py))
    PVkPy <- lapply(VkPy, function(v) as.numeric(pc$P %*% v))
    score <- vapply(seq_len(K), function(q) -0.5 * sum(pc$P * t(ZZ[[q]])) + 0.5 * sum(pc$Py * VkPy[[q]]), 0)
    AI <- outer(seq_len(K), seq_len(K), Vectorize(function(q, r) 0.5 * sum(VkPy[[q]] * PVkPy[[r]])))
    free <- which(s > floor_ * (1 + 1e-9) | score > 0)
    step <- numeric(K)
    if (length(free)) step[free] <- as.numeric(solve(AI[free, free, drop = FALSE], score[free]))
    f <- 1
    acc <- NULL
    for (h in seq_len(40)) {
      nw <- pmax(s + f * step, floor_)
      pcn <- pieces(nw)
      lln <- rll(pcn)
      if (lln >= ll - 1e-12 * abs(ll)) {
        acc <- list(nw = nw, pc = pcn, ll = lln)
        break
      }
      f <- f / 2
    }
    if (is.null(acc)) break
    delta <- max(abs(acc$nw - s) / pmax(acc$nw, floor_))
    s <- acc$nw
    pc <- acc$pc
    ll <- acc$ll
    if (delta <= tol) break
  }
  beta <- as.numeric(pc$XVXi %*% crossprod(X, pc$Vi %*% y))
  blups <- lapply(seq_len(K - 1), function(q) s[q] * as.numeric(crossprod(as.matrix(Zs[[q]]), pc$Py)))
  list(variances = s, beta = beta, blups = blups, reml_loglik = ll, iterations = it)
}

.sf_indicator <- function(codes) {
  lev <- sort(unique(codes))
  outer(codes, lev, `==`) * 1
}

#' @rdname CurveFpca
#' @export
CrossedRandomEffects <- function(y, a, b, X = NULL) {
  Xm <- if (is.null(X)) matrix(1, length(y), 1) else X
  RemlComponents(y, Xm, list(.sf_indicator(a), .sf_indicator(b)))
}

#' @rdname CurveFpca
#' @export
NestedRandomEffects <- function(y, a, b, X = NULL) {
  Xm <- if (is.null(X)) matrix(1, length(y), 1) else X
  u <- unique(cbind(a, b))
  u <- u[order(u[, 1], u[, 2]), , drop = FALSE]
  Zb <- vapply(seq_len(nrow(u)), function(q) as.numeric(a == u[q, 1] & b == u[q, 2]), numeric(length(y)))
  RemlComponents(y, Xm, list(.sf_indicator(a), matrix(Zb, length(y))))
}
