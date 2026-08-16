# SPDX-License-Identifier: AGPL-3.0-or-later
# Deterministic kernels shared by the optimal-transport modules.
#
# Written against Peyre, G. and Cuturi, M. (2019), Computational Optimal
# Transport, Foundations and Trends in Machine Learning 11(5-6):355-607
# (copy consulted: arXiv:1803.00567v4).  Nothing here draws a random
# number: sliced methods take their directions from the van der Corput /
# AS 241 stream in aaa_helpers_s03.R so this arm and the Python arm land
# on the same projections.

#' .ot_hist
#'
#' A step of the helpers_ot implementation. Called by \code{Otbar}, \code{Otbarfree}, \code{Otbreg} and 9 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Coerced to numeric by the body, with \code{as.numeric}.
#' @param normalise A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{v}, as built in the body.
#' @export
.ot_hist <- function(a, normalise = FALSE) {
  v <- as.numeric(a)
  if (any(v < 0)) stop("weights must be non-negative")
  if (normalise) {
    s <- sum(v)
    if (s <= 0) stop("weights must have positive total mass")
    v <- v / s
  }
  v
}

#' .ot_costmat
#'
#' A step of the helpers_ot implementation. Called by \code{Otbarfree}, \code{Otker}, \code{Otmcluster} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param Y A matrix; passed to \code{as.matrix}.
#' @param p Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @return The value of \code{out}, as built in the body.
#' @export
.ot_costmat <- function(X, Y, p = 2) {
  A <- as.matrix(X); B <- as.matrix(Y)
  if (ncol(A) != ncol(B)) stop("point clouds must share a dimension")
  n <- nrow(A); m <- nrow(B)
  out <- matrix(0, n, m)
  for (i in seq_len(n)) {
    dd <- sqrt(colSums((t(B) - A[i, ])^2))
    out[i, ] <- dd^p
  }
  out
}

#' .ot_frob
#'
#' A step of the helpers_ot implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param C Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ot_frob <- function(T, C) sum(T * C)

#' .ot_kl
#'
#' A step of the helpers_ot implementation. Called by \code{Otdiv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T A vector; indexed elementwise.
#' @param R A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.ot_kl <- function(T, R) {
  pos <- T > 0
  sum(T[pos] * (log(T[pos]) - log(R[pos]))) + sum(R) - sum(T)
}

#' .ot_lse
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_sinkhorn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.ot_lse <- function(v) {
  mx <- max(v)
  if (!is.finite(mx)) return(mx)
  mx + log(sum(exp(v - mx)))
}

#' .ot_sinkhorn
#'
#' A step of the helpers_ot implementation. Called by \code{Otdiv}, \code{Otgws}, \code{Otker} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @param C A matrix; indexed by row and column.
#' @param eps Numeric; combined arithmetically in the body.
#' @param n_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @return A list with \code{T}, \code{f}, \code{g}.
#' @export
.ot_sinkhorn <- function(a, b, C, eps, n_iter = 200L) {
  n <- length(a); m <- length(b)
  if (eps <= 0) stop("epsilon must be positive")
  la <- log(a); lb <- log(b)
  f <- numeric(n); g <- numeric(m)
  for (it in seq_len(as.integer(n_iter))) {
    for (i in seq_len(n)) {
      if (!is.finite(la[i])) { f[i] <- -Inf; next }
      f[i] <- eps * (la[i] - .ot_lse((g - C[i, ]) / eps))
    }
    for (j in seq_len(m)) {
      if (!is.finite(lb[j])) { g[j] <- -Inf; next }
      g[j] <- eps * (lb[j] - .ot_lse((f - C[, j]) / eps))
    }
  }
  Z <- outer(f, g, "+") - C
  T <- ifelse(Z > -Inf, exp(Z / eps), 0)
  T[!is.finite(T)] <- 0
  list(T = matrix(T, n, m), f = f, g = g)
}

#' .ot_sinkhorn_unbalanced
#'
#' A step of the helpers_ot implementation. Called by \code{Otunbal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; its length is taken and its elements indexed.
#' @param C Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body.
#' @param n_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @return A numeric value.
#' @export
.ot_sinkhorn_unbalanced <- function(a, b, C, eps, lam, n_iter = 200L) {
  n <- length(a); m <- length(b)
  if (eps <= 0 || lam <= 0) stop("epsilon and lambda must be positive")
  pw <- lam / (lam + eps)
  K <- exp(-C / eps)
  u <- rep(1, n); v <- rep(1, m)
  for (it in seq_len(as.integer(n_iter))) {
    for (i in seq_len(n)) {
      s <- sum(K[i, ] * v)
      u[i] <- if (s > 0) (a[i] / s)^pw else 0
    }
    for (j in seq_len(m)) {
      s <- sum(K[, j] * u)
      v[j] <- if (s > 0) (b[j] / s)^pw else 0
    }
  }
  matrix(u, n, m) * K * matrix(v, n, m, byrow = TRUE)
}

# ------------------------------------------------------- exact transport

#' .ot_nwcorner
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @return A list with \code{T}, \code{basis}.
#' @export
.ot_nwcorner <- function(a, b) {
  n <- length(a); m <- length(b)
  ra <- a; rb <- b
  T <- matrix(0, n, m)
  basis <- list()
  i <- 1L; j <- 1L
  repeat {
    t <- min(ra[i], rb[j])
    T[i, j] <- t
    basis[[length(basis) + 1L]] <- c(i, j)
    ra[i] <- ra[i] - t; rb[j] <- rb[j] - t
    if (i == n && j == m) break
    if (ra[i] <= 1e-15 && i < n) i <- i + 1L
    else if (j < m) j <- j + 1L
    else i <- i + 1L
  }
  list(T = T, basis = basis)
}

#' .ot_complete_tree
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param basis See Usage.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{.ot_sortbasis}.
#' @export
.ot_complete_tree <- function(basis, n, m) {
  parent <- seq_len(n + m)
  fnd <- function(x) { while (parent[x] != x) { parent[x] <<- parent[parent[x]]; x <- parent[x] }; x }
  edges <- list()
  have <- character(0)
  for (e in basis) {
    ri <- fnd(e[1]); rj <- fnd(n + e[2])
    if (ri != rj) {
      parent[ri] <- rj
      edges[[length(edges) + 1L]] <- e
      have <- c(have, paste(e[1], e[2]))
    }
  }
  for (i in seq_len(n)) for (j in seq_len(m)) {
    if (length(edges) >= n + m - 1L) break
    if (paste(i, j) %in% have) next
    ri <- fnd(i); rj <- fnd(n + j)
    if (ri != rj) {
      parent[ri] <- rj
      edges[[length(edges) + 1L]] <- c(i, j)
      have <- c(have, paste(i, j))
    }
  }
  .ot_sortbasis(edges)
}

#' .ot_sortbasis
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_complete_tree}, \code{.ot_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A vector; its length is taken and its elements indexed.
#' @return The value of \code{[}.
#' @export
.ot_sortbasis <- function(edges) {
  if (!length(edges)) return(edges)
  key <- vapply(edges, function(e) e[1] * 1e6 + e[2], 0)
  edges[order(key)]
}

#' .ot_adj
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_potentials}, \code{.ot_tree_path}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param basis Iterated over elementwise, with \code{vapply}.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{adj}, as built in the body.
#' @export
.ot_adj <- function(basis, n) {
  adj <- vector("list", n + max(vapply(basis, function(e) e[2], 0L)))
  for (e in basis) {
    i <- e[1]; j <- e[2]
    adj[[i]] <- c(adj[[i]], list(c(n + j, i, j)))
    adj[[n + j]] <- c(adj[[n + j]], list(c(i, i, j)))
  }
  adj
}

#' .ot_potentials
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param basis Passed to \code{.ot_adj}.
#' @param C A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{numeric(...)}.
#' @param m A count; the body uses it as \code{numeric(...)}.
#' @return A list with \code{u}, \code{v}.
#' @export
.ot_potentials <- function(basis, C, n, m) {
  adj <- .ot_adj(basis, n)
  u <- numeric(n); v <- numeric(m)
  seen <- rep(FALSE, n + m); seen[1] <- TRUE
  stack <- c(1L)
  while (length(stack)) {
    node <- stack[length(stack)]; stack <- stack[-length(stack)]
    for (e in adj[[node]]) {
      nb <- e[1]; i <- e[2]; j <- e[3]
      if (seen[nb]) next
      seen[nb] <- TRUE
      if (nb > n) v[nb - n] <- C[i, j] - u[i] else u[nb] <- C[i, j] - v[j]
      stack <- c(stack, nb)
    }
  }
  list(u = u, v = v)
}

#' .ot_tree_path
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param basis Passed to \code{.ot_adj}.
#' @param n Numeric; combined arithmetically in the body.
#' @param si See Usage.
#' @param sj Numeric; combined arithmetically in the body.
#' @return Nothing; the function is called for its effect.
#' @export
.ot_tree_path <- function(basis, n, si, sj) {
  adj <- .ot_adj(basis, n)
  goal <- n + sj
  stack <- list(list(node = si, path = list(), seen = si))
  while (length(stack)) {
    st <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
    if (st$node == goal) return(st$path)
    for (e in adj[[st$node]]) {
      nb <- e[1]
      if (nb %in% st$seen) next
      stack[[length(stack) + 1L]] <- list(node = nb,
                                          path = c(st$path, list(c(e[2], e[3]))),
                                          seen = c(st$seen, nb))
    }
  }
  NULL
}

#' .ot_emd
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_partial_plan}, \code{Otbarfree}, \code{Otemd} and 6 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @param C A matrix; passed to \code{nrow}.
#' @param max_pivots Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20000L}.
#' @return A list with \code{T}, \code{cost}.
#' @export
.ot_emd <- function(a, b, C, max_pivots = 20000L) {
  n <- length(a); m <- length(b)
  if (n == 0L || m == 0L) stop("emd: empty marginal")
  C <- matrix(as.numeric(C), n, m)
  if (nrow(C) != n || ncol(C) != m) stop("emd: cost matrix does not match the marginals")
  sa <- sum(a); sb <- sum(b)
  if (abs(sa - sb) > 1e-9 * max(1, abs(sa)))
    stop("emd: marginals must have equal total mass")
  nw <- .ot_nwcorner(a, b)
  T <- nw$T
  basis <- .ot_complete_tree(nw$basis, n, m)
  done <- FALSE
  for (piv in seq_len(as.integer(max_pivots))) {
    bkey <- vapply(basis, function(e) e[1] * 1e6 + e[2], 0)
    pot <- .ot_potentials(basis, C, n, m)
    D <- C - outer(pot$u, pot$v, "+")
    D[cbind(vapply(basis, function(e) e[1], 0L), vapply(basis, function(e) e[2], 0L))] <- Inf
    best <- min(D)
    if (!(best < -1e-11)) { done <- TRUE; break }
    # Row-major scan, so the tie-break matches the Python arm exactly;
    # degenerate transport problems have several optimal vertices and
    # column-major order picks a different one.
    Dt <- t(D)
    idx <- which(Dt <= best + 1e-15)[1]
    sj <- ((idx - 1L) %% m) + 1L
    si <- ((idx - 1L) %/% m) + 1L
    path <- .ot_tree_path(basis, n, si, sj)
    minus <- path[seq(1, length(path), by = 2)]
    flows <- vapply(minus, function(e) T[e[1], e[2]], 0)
    theta <- min(flows)
    leave <- minus[[which(flows <= theta + 1e-15)[1]]]
    T[si, sj] <- T[si, sj] + theta
    sgn <- -1
    for (e in path) {
      T[e[1], e[2]] <- T[e[1], e[2]] + sgn * theta
      sgn <- -sgn
    }
    lk <- leave[1] * 1e6 + leave[2]
    basis <- .ot_sortbasis(c(basis[bkey != lk], list(c(si, sj))))
  }
  if (!done) stop("emd: pivot cap reached")
  list(T = T, cost = sum(T * C))
}

# Partial transport of exactly m units, via a zero-price dummy row and
# column (Caffarelli and McCann 2010): the inequality-constrained problem
# becomes an ordinary balanced one and is solved exactly.
#' Partial transport of exactly m units, via a zero-price dummy row and
#'
#' column (Caffarelli and McCann 2010): the inequality-constrained
#' problem becomes an ordinary balanced one and is solved exactly.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @param C Numeric; passed to \code{max}.
#' @param m Numeric; combined arithmetically in the body.
#' @return A list with \code{T}, \code{cost}.
#' @export
.ot_partial_plan <- function(a, b, C, m) {
  n <- length(a); k <- length(b)
  sa <- sum(a); sb <- sum(b); m <- as.numeric(m)
  if (m < 0 || m > min(sa, sb) + 1e-12)
    stop("the transported mass must lie in [0, min(|a|,|b|)]")
  big <- 2 * max(C) + 1
  sup <- c(a, sb - m); dem <- c(b, sa - m)
  Ce <- rbind(cbind(C, 0), c(rep(0, k), big))
  r <- .ot_emd(sup, dem, Ce)
  P <- r$T[seq_len(n), seq_len(k), drop = FALSE]
  list(T = P, cost = sum(P * C))
}

# --------------------------------------------------------------- Gaussian

#' .ot_sqrtm
#'
#' A step of the helpers_ot implementation. Called by \code{.ot_w2gauss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param S A matrix; passed to \code{as.matrix}.
#' @return The value of \code{%*%}.
#' @export
.ot_sqrtm <- function(S) {
  e <- .s03jacobi(as.matrix(S))
  r <- ifelse(e$values > 0, sqrt(e$values), 0)
  e$vectors %*% diag(r, nrow = length(r)) %*% t(e$vectors)
}

#' .ot_w2gauss
#'
#' A step of the helpers_ot implementation. Called by \code{Otmxh}, \code{Otwsg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param S1 A matrix; passed to \code{as.matrix}.
#' @param m2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param S2 A matrix; passed to \code{as.matrix}.
#' @return A numeric value.
#' @export
.ot_w2gauss <- function(m1, S1, m2, S2) {
  a <- as.numeric(m1); b <- as.numeric(m2)
  A <- as.matrix(S1); B <- as.matrix(S2)
  d <- length(a)
  if (length(b) != d || nrow(A) != d || nrow(B) != d)
    stop("w2gauss: dimension mismatch")
  R <- .ot_sqrtm(A)
  Msq <- .ot_sqrtm(R %*% B %*% R)
  bures <- sum(diag(A)) + sum(diag(B)) - 2 * sum(diag(Msq))
  if (bures < 0) bures <- 0
  sum((a - b)^2) + bures
}

# ------------------------------------------------------------ 1-D, slices

#' .ot_wp1d
#'
#' A step of the helpers_ot implementation. Called by \code{Otmsw}, \code{Otsw}, \code{Otws2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param p Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @return A numeric value.
#' @export
.ot_wp1d <- function(x, y, p = 2) {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y))
  if (length(xs) != length(ys)) stop("wp1d: samples must have equal length")
  if (!length(xs)) stop("wp1d: empty sample")
  if (p <= 0) stop("wp1d: p must be positive")
  (sum(abs(xs - ys)^p) / length(xs))^(1 / p)
}

#' .ot_quantiles
#'
#' A step of the helpers_ot implementation. Called by \code{Otsd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param grid Iterated over elementwise, with \code{vapply}.
#' @return A vector, from \code{vapply}.
#' @export
.ot_quantiles <- function(x, grid) {
  v <- sort(as.numeric(x))
  vapply(grid, function(q) .s03quantile7(v, q), 0)
}

#' .ot_directions
#'
#' A step of the helpers_ot implementation. Called by \code{Otmsw}, \code{Otsd}, \code{Otsw}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d A count; the body uses it as \code{matrix(...)}.
#' @param n_proj A count; the body uses it as \code{matrix(...)}.
#' @return A numeric value.
#' @export
.ot_directions <- function(d, n_proj) {
  if (d < 1 || n_proj < 1) stop("directions: d and n_proj must be positive")
  n_proj <- as.integer(n_proj)
  if (d == 1) return(matrix(1, n_proj, 1))
  z <- .s03normdraws(d * n_proj)
  M <- matrix(z, nrow = n_proj, ncol = d, byrow = TRUE)
  nrm <- sqrt(rowSums(M^2))
  nrm[nrm <= 0] <- 1
  M / nrm
}

#' .ot_project
#'
#' A step of the helpers_ot implementation. Called by \code{Otmsw}, \code{Otsd}, \code{Otsw}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
.ot_project <- function(X, theta) as.numeric(as.matrix(X) %*% as.numeric(theta))
