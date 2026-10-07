#' Density and prototype clustering: DENCLUE, FLAME, PFCM, growing neural gas
#'
#' \code{Denclue}: Gaussian-kernel hill climbing to density attractors,
#' noise below \code{xi}. \code{FlameClustering}: fuzzy clustering by local
#' approximation of memberships, as the reference C code.
#' \code{PossibilisticFcm}: possibilistic fuzzy c-means started from fuzzy
#' c-means. \code{GrowingNeuralGas}: Fritzke's growing neural gas, clusters
#' are graph components. Identical to the Python arm
#' \code{morie.fn.clusterdensity}.
#'
#' @param X Data matrix (rows are points).
#' @param h Kernel bandwidth.
#' @param xi Density threshold for noise.
#' @param eps Attractor merge distance (default h / 2).
#' @param tol Convergence tolerance.
#' @param maxit Maximum iterations.
#' @param knn Number of neighbours.
#' @param outlier_threshold Outlier density threshold in standard deviations.
#' @param steps,epsilon FLAME iteration budget and tolerance.
#' @param centers Initial centres (matrix).
#' @param a,b Weights of memberships and typicalities.
#' @param m,eta Fuzzifier and typicality exponent.
#' @param K Scale of the typicality penalties.
#' @param n_signals,max_nodes Signals presented and node cap.
#' @param eps_b,eps_n Winner and neighbour learning rates.
#' @param lam Insertion interval.
#' @param alpha Error reduction on insertion.
#' @param a_max Maximum edge age.
#' @param d Error decay.
#' @param seed Philox seed.
#' @return List with \code{cluster} (0 = noise or outlier where applicable).
#' @references Hinneburg, A. and Gabriel, H.-H. (2007). DENCLUE 2.0: fast
#'   clustering based on kernel density estimation. Proceedings of IDA, 70-80.
#'
#'   Fu, L. and Medico, E. (2007). FLAME, a novel fuzzy clustering method for
#'   the analysis of DNA microarray data. BMC Bioinformatics 8, 3.
#'
#'   Pal, N. R., Pal, K., Keller, J. M. and Bezdek, J. C. (2005). A
#'   possibilistic fuzzy c-means clustering algorithm. IEEE Transactions on
#'   Fuzzy Systems 13, 517-530.
#'
#'   Fritzke, B. (1995). A growing neural gas network learns topologies.
#'   Advances in Neural Information Processing Systems 7, 625-632.
#' @examples
#' Denclue(matrix(c(0, 0.3, 0.1, 9, 9.2, 30)), 0.5, 0.2)$cluster
#' PossibilisticFcm(matrix(c(0, 1, 9, 10, 4)), matrix(c(0, 10)))$cluster
#' @export
Denclue <- function(X, h, xi, eps = NULL, tol = 1e-10, maxit = 1000L) {
  X <- as.matrix(X)
  n <- nrow(X)
  d <- ncol(X)
  cc <- (2 * pi * h * h)^(-d / 2) / n
  if (is.null(eps)) eps <- h / 2
  dens <- function(y) cc * .cd_ss(exp(-colSums((t(X) - y)^2) / (2 * h * h)))
  att <- matrix(0, n, d)
  fatt <- numeric(n)
  for (i in seq_len(n)) {
    y <- X[i, ]
    fy <- dens(y)
    for (it in seq_len(maxit)) {
      w <- exp(-colSums((t(X) - y)^2) / (2 * h * h))
      sw <- .cd_ss(w)
      y <- vapply(seq_len(d), function(t) .cd_ss(w * X[, t]) / sw, 0)
      fn <- dens(y)
      gain <- fn - fy
      fy <- fn
      if (gain <= tol * fn) break
    }
    att[i, ] <- y
    fatt[i] <- fy
  }
  par <- seq_len(n)
  find <- function(a) {
    while (par[a] != a) a <- par[a]
    a
  }
  keep <- fatt >= xi
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (keep[i] && keep[j] && sum((att[i, ] - att[j, ])^2) < eps * eps) {
        ra <- find(i)
        rb <- find(j)
        par[max(ra, rb)] <- min(ra, rb)
      }
    }
  }
  lab <- ifelse(keep, vapply(seq_len(n), find, 1L), 0L)
  list(cluster = .cd_first(lab), attractors = att, density = fatt)
}

.cd_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.cd_d2 <- function(a, b) {
  s <- 0
  for (t in seq_along(a)) s <- s + (a[t] - b[t]) * (a[t] - b[t])
  s
}

.cd_first <- function(lab) {
  u <- unique(lab[lab != 0])
  out <- match(lab, u)
  out[lab == 0] <- 0L
  as.integer(out)
}

#' @rdname Denclue
#' @export
FlameClustering <- function(X, knn = 10L, outlier_threshold = -2, steps = 500L, epsilon = 1e-6) {
  .morie_arg(X, "m")
  X <- as.matrix(X)
  n <- nrow(X)
  kmax <- min(floor(sqrt(n)) + 10, n - 1)
  knn <- min(knn, kmax)
  D2 <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) D2[i, j] <- .cd_d2(X[i, ], X[j, ])
  graph <- vector("list", n)
  dists <- vector("list", n)
  for (i in seq_len(n)) {
    oth <- setdiff(seq_len(n), i)
    o <- oth[order(D2[i, oth], oth)][seq_len(kmax)]
    graph[[i]] <- o
    dists[[i]] <- sqrt(D2[i, o])
  }
  counts <- integer(n)
  weights <- vector("list", n)
  density <- numeric(n)
  for (i in seq_len(n)) {
    k <- knn
    dk <- dists[[i]][knn]
    if (knn < kmax) {
      for (j in (knn + 1):kmax) {
        if (dists[[i]][j] == dk) k <- k + 1 else break
      }
    }
    counts[i] <- k
    s <- 0.5 * k * (k + 1)
    weights[[i]] <- (k - (seq_len(k) - 1)) / s
    density[i] <- 1 / (.cd_ss(dists[[i]][seq_len(k)]) + 1e-9)
  }
  mu <- .cd_ss(density) / n
  thd <- mu + outlier_threshold * sqrt(max(.cd_ss(density * density) / n - mu * mu, 0))
  typ <- integer(n)
  for (i in seq_len(n)) {
    k <- counts[i]
    fmax <- 0
    fmin <- density[i] / density[graph[[i]][1]]
    if (k > 1) {
      for (j in 2:k) {
        r <- density[i] / density[graph[[i]][j]]
        fmax <- max(fmax, r)
        fmin <- min(fmin, r)
        if (typ[graph[[i]][j]] != 0) fmin <- 0
      }
    }
    if (fmin >= 1) {
      typ[i] <- 1L
    } else if (fmax <= 1 && density[i] < thd) {
      typ[i] <- 2L
    }
  }
  csos <- which(typ == 1L)
  m <- length(csos)
  A <- matrix(1 / (m + 1), n, m + 1)
  for (i in seq_len(n)) {
    if (typ[i] == 1L) A[i, ] <- replace(numeric(m + 1), match(i, csos), 1)
    if (typ[i] == 2L) A[i, ] <- replace(numeric(m + 1), m + 1, 1)
  }
  B <- A
  even <- FALSE
  it <- 0L
  for (it in seq_len(steps)) {
    dev <- 0
    prev <- if (even) A else B
    cur <- if (even) B else A
    for (i in seq_len(n)) {
      if (typ[i] != 0L) next
      ids <- graph[[i]][seq_len(counts[i])]
      wt <- weights[[i]]
      tot <- 0
      for (j in seq_len(m + 1)) {
        v <- 0
        for (q in seq_along(ids)) v <- v + wt[q] * prev[ids[q], j]
        cur[i, j] <- v
        dev <- dev + (v - prev[i, j])^2
        tot <- tot + v
      }
      cur[i, ] <- cur[i, ] / tot
    }
    if (even) B <- cur else A <- cur
    even <- !even
    if (dev < epsilon) break
  }
  for (i in seq_len(n)) {
    ids <- graph[[i]][seq_len(counts[i])]
    wt <- weights[[i]]
    for (j in seq_len(m + 1)) {
      v <- 0
      for (q in seq_along(ids)) v <- v + wt[q] * B[ids[q], j]
      A[i, j] <- v
    }
  }
  jmax <- max.col(A, ties.method = "first")
  lab <- ifelse(jmax == m + 1, 0L, jmax)
  list(cluster = .cd_first(lab), membership = A, supports = csos, outliers = which(typ == 2L), iterations = it)
}

#' @rdname Denclue
#' @export
PossibilisticFcm <- function(X, centers, a = 1, b = 1, m = 2, eta = 2, K = 1, tol = 1e-9, maxit = 1000L) {
  X <- as.matrix(X)
  n <- nrow(X)
  f <- FuzzyCmeans(X, centers, m = m)
  V <- as.matrix(f$centers)
  k <- nrow(V)
  U0 <- f$membership
  d2m <- function(V) matrix(vapply(seq_len(k), function(c) colSums((t(X) - V[c, ])^2), numeric(n)), n)
  D2 <- d2m(V)
  gam <- vapply(seq_len(k), function(c) K * .cd_ss(U0[, c]^m * D2[, c]) / .cd_ss(U0[, c]^m), 0)
  e <- 2 / (m - 1)
  U <- U0
  Tt <- NULL
  it <- 0L
  for (it in seq_len(maxit)) {
    D2 <- d2m(V)
    U <- t(apply(D2, 1, function(r) {
      if (any(r == 0)) return((r == 0) / sum(r == 0))
      vapply(seq_len(k), function(c) 1 / .cd_ss((r[c] / r)^(e / 2)), 0)
    }))
    U <- matrix(U, n)
    Tt <- 1 / (1 + (b * sweep(D2, 2, gam, "/"))^(1 / (eta - 1)))
    newV <- t(vapply(seq_len(k), function(c) {
      w <- a * U[, c]^m + b * Tt[, c]^eta
      sw <- .cd_ss(w)
      vapply(seq_len(ncol(X)), function(t) .cd_ss(w * X[, t]) / sw, 0)
    }, numeric(ncol(X))))
    newV <- matrix(newV, k)
    shift <- max(sqrt(rowSums((V - newV)^2)))
    V <- newV
    if (shift < tol) break
  }
  list(centers = V, membership = U, typicality = Tt, gamma = gam,
       cluster = max.col(U, ties.method = "first"), iter = it)
}

#' @rdname Denclue
#' @export
GrowingNeuralGas <- function(X, n_signals = 5000L, max_nodes = 30L, eps_b = 0.2, eps_n = 0.006, lam = 100L,
                             alpha = 0.5, a_max = 50L, d = 0.995, seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  u <- .morie_random_uniform(n_signals + 2, seed = seed)
  pick <- function(v) min(floor(v * n), n - 1) + 1
  W <- rbind(X[pick(u[1]), ], X[pick(u[2]), ])
  err <- c(0, 0)
  E <- matrix(integer(0), 0, 3)
  for (s in seq_len(n_signals)) {
    x <- X[pick(u[s + 2]), ]
    dd <- colSums((t(W) - x)^2)
    o <- order(dd, seq_along(dd))
    s1 <- o[1]
    s2 <- o[2]
    inc <- E[, 1] == s1 | E[, 2] == s1
    E[inc, 3] <- E[inc, 3] + 1L
    err[s1] <- err[s1] + dd[s1]
    W[s1, ] <- W[s1, ] + eps_b * (x - W[s1, ])
    for (r in which(inc)) {
      oth <- if (E[r, 2] == s1) E[r, 1] else E[r, 2]
      W[oth, ] <- W[oth, ] + eps_n * (x - W[oth, ])
    }
    a1 <- min(s1, s2)
    b1 <- max(s1, s2)
    hit <- which(E[, 1] == a1 & E[, 2] == b1)
    if (length(hit)) E[hit, 3] <- 0L else E <- rbind(E, c(a1, b1, 0L))
    E <- E[E[, 3] <= a_max, , drop = FALSE]
    alive <- sort(unique(c(E[, 1], E[, 2])))
    if (length(alive) < nrow(W)) {
      W <- W[alive, , drop = FALSE]
      err <- err[alive]
      E[, 1] <- match(E[, 1], alive)
      E[, 2] <- match(E[, 2], alive)
    }
    if (s %% lam == 0 && nrow(W) < max_nodes) {
      q <- which.max(err)
      nbr <- c(E[E[, 1] == q, 2], E[E[, 2] == q, 1])
      nbr <- nbr[order(-err[nbr], nbr)]
      f <- nbr[1]
      r <- nrow(W) + 1
      W <- rbind(W, 0.5 * (W[q, ] + W[f, ]))
      E <- E[!(E[, 1] == min(q, f) & E[, 2] == max(q, f)), , drop = FALSE]
      E <- rbind(E, c(min(q, r), max(q, r), 0L), c(min(f, r), max(f, r), 0L))
      err[q] <- err[q] * alpha
      err[f] <- err[f] * alpha
      err <- c(err, err[q])
    }
    err <- err * d
  }
  m <- nrow(W)
  par <- seq_len(m)
  find <- function(a) {
    while (par[a] != a) a <- par[a]
    a
  }
  E <- E[order(E[, 1], E[, 2]), , drop = FALSE]
  for (r in seq_len(nrow(E))) {
    ra <- find(E[r, 1])
    rb <- find(E[r, 2])
    if (ra != rb) par[max(ra, rb)] <- min(ra, rb)
  }
  near <- vapply(seq_len(n), function(i) which.min(colSums((t(W) - X[i, ])^2)), 1L)
  lab <- vapply(near, function(i) as.integer(find(i)), 1L)
  list(cluster = .cd_first(lab), nodes = W, edges = E[, 1:2, drop = FALSE])
}
