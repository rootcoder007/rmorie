# ROBPCA: robust principal components for data with outliers.
#
# Hubert, M., Rousseeuw, P. J., & Vanden Branden, K. (2005) "ROBPCA: A New
# Approach to Robust Principal Component Analysis", Technometrics 47(1),
# 64-79.

.Z975 <- 1.959963984540054

.robpca_matrix <- function(X, name = "X") {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.numeric(X)) {
    X <- tryCatch(as.matrix(X), error = function(e) NULL)
    if (is.null(X) || !is.numeric(X))
      stop(sprintf("robpca: %s must be numeric", name))
  }
  if (is.null(dim(X)) || any(dim(X) == 0L))
    stop(sprintf("robpca: %s is empty", name))
  if (any(!is.finite(X)))
    stop(sprintf("robpca: %s contains a non-finite value", name))
  storage.mode(X) <- "double"
  X
}

.robpca_eigh_desc <- function(C) {
  e <- eigen(C, symmetric = TRUE)
  ord <- order(e$values, decreasing = TRUE)
  Vm <- t(e$vectors[, ord, drop = FALSE])
  # sign convention of the Python jacobi mirror: the largest-magnitude
  # entry of each eigenvector is positive
  for (j in seq_len(nrow(Vm))) {
    m <- which.max(abs(Vm[j, ]))
    if (Vm[j, m] < 0) Vm[j, ] <- -Vm[j, ]
  }
  list(values = as.numeric(e$values[ord]), vectors = Vm)
}

.robpca_det_from_chol <- function(C) {
  L <- tryCatch(chol(C), error = function(e) NULL)
  if (is.null(L)) return(0.0)
  prod(diag(L) ^ 2)
}

.robpca_mahalanobis <- function(X, mu, C) {
  invC <- tryCatch(solve(C), error = function(e) NULL)
  if (is.null(invC)) stop("singular")
  Xc <- sweep(X, 2, mu, "-")
  d2 <- rowSums((Xc %*% invC) * Xc)
  d2[d2 < 0] <- 0
  sqrt(d2)
}

univariate_mcd <- function(values, h = NULL, consistent = TRUE) {
  v <- sort(as.numeric(values))
  n <- length(v)
  if (n < 2L) stop("robpca: the univariate MCD needs two values")
  if (is.null(h)) h <- (n + 2L) %/% 2L
  h <- as.integer(h)
  if (h < 2L || h > n)
    stop("robpca: h must lie in [2, n] for the univariate MCD")
  csum <- cumsum(c(0, v))
  csq <- cumsum(c(0, v * v))
  best_ss <- Inf
  best_mean <- 0
  for (i in 1:(n - h + 1L)) {
    s <- csum[i + h] - csum[i]
    q <- csq[i + h] - csq[i]
    ss <- q - s * s / h
    if (ss < best_ss) {
      best_ss <- ss
      best_mean <- s / h
    }
  }
  scale <- sqrt(max(best_ss, 0.0) / (h - 1.0))
  if (consistent && scale > 0.0) {
    a <- h / n
    denom <- pchisq(qchisq(min(a, 0.999999), 1), 3)
    if (denom > 0) scale <- scale * sqrt(a / denom)
  }
  c(loc = unname(best_mean), scale = unname(scale))
}

.robpca_directions <- function(rows, n_dirs, seed) {
  n <- nrow(rows)
  p <- ncol(rows)
  pairs <- list()
  total <- n * (n - 1L) %/% 2L
  if (total <= n_dirs) {
    for (i in 1:(n - 1L))
      for (j in (i + 1L):n)
        pairs[[length(pairs) + 1L]] <- c(i, j)
  } else {
    state <- .ghc_rng(seed)
    seen <- new.env(hash = TRUE)
    guard <- 0L
    while (length(pairs) < n_dirs && guard < 50L * n_dirs) {
      guard <- guard + 1L
      i <- .ghc_int(state, 1L, n) + 1L
      j <- .ghc_int(state, 1L, n) + 1L
      if (i == j) next
      key <- paste(min(i, j), max(i, j), sep = ",")
      if (exists(key, envir = seen, inherits = FALSE)) next
      assign(key, TRUE, envir = seen)
      pairs[[length(pairs) + 1L]] <- c(i, j)
    }
  }
  dirs <- list()
  for (pair in pairs) {
    v <- rows[pair[1L], ] - rows[pair[2L], ]
    norm <- sqrt(sum(v * v))
    if (norm > 1e-12)
      dirs[[length(dirs) + 1L]] <- v / norm
  }
  if (length(dirs) == 0L) return(matrix(0, nrow = 0L, ncol = p))
  do.call(rbind, dirs)
}

outlyingness <- function(X, h = NULL, n_dirs = 250L, seed = 17L) {
  rows <- .robpca_matrix(X)
  n <- nrow(rows)
  if (is.null(h)) h <- (n + 2L) %/% 2L
  out <- rep(0.0, n)
  dirs <- .robpca_directions(rows, n_dirs, seed)
  for (k in seq_len(nrow(dirs))) {
    v <- dirs[k, ]
    proj <- as.numeric(rows %*% v)
    res <- univariate_mcd(proj, h = h)
    loc <- res["loc"]
    scale <- res["scale"]
    if (scale <= 1e-12)
      return(list(outl = NULL, exact_fit_direction = v))
    r <- abs(proj - loc) / scale
    out <- pmax(out, r)
  }
  list(outl = out, exact_fit_direction = NULL)
}

.robpca_c_steps <- function(rows, idx, max_iter = 100L) {
  h <- length(idx)
  cur <- idx
  X_sub <- rows[cur, , drop = FALSE]
  mu <- colMeans(X_sub)
  C <- cov(X_sub)
  detval <- .robpca_det_from_chol(C)
  for (iter in seq_len(max_iter)) {
    if (detval <= 0.0) break
    d <- tryCatch(.robpca_mahalanobis(rows, mu, C), error = function(e) NULL)
    if (is.null(d)) break
    nxt <- order(d)[1L:h]
    X_sub2 <- rows[nxt, , drop = FALSE]
    mu2 <- colMeans(X_sub2)
    C2 <- cov(X_sub2)
    det2 <- .robpca_det_from_chol(C2)
    if (det2 >= detval - 1e-15 * max(detval, 1.0)) {
      if (det2 < detval) {
        cur <- nxt; mu <- mu2; C <- C2; detval <- det2
      }
      break
    }
    cur <- nxt; mu <- mu2; C <- C2; detval <- det2
  }
  list(idx = cur, mu = mu, C = C, det = detval)
}

.robpca_fast_mcd <- function(rows, h, n_start = 250L, seed = 17L) {
  n <- nrow(rows)
  p <- ncol(rows)
  state <- .ghc_rng(seed + 1L)
  cands <- list()
  for (s in seq_len(as.integer(n_start))) {
    pick <- integer(0)
    guard <- 0L
    target <- min(p + 1L, n)
    while (length(pick) < target && guard < 100L) {
      guard <- guard + 1L
      i <- .ghc_int(state, 1L, n) + 1L
      pick <- c(pick, i)
      pick <- unique(pick)
    }
    if (length(pick) < 2L) next
    sub <- sort(pick)
    res <- tryCatch({
      X_sub <- rows[sub, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      detval <- .robpca_det_from_chol(C)
      list(mu = mu, C = C, det = detval)
    }, error = function(e) NULL)
    if (is.null(res)) next
    if (res$det <= 0.0) {
      base <- colMeans(rows[sub, , drop = FALSE])
      d <- sqrt(rowSums((rows -
        matrix(base, nrow = n, ncol = p, byrow = TRUE)) ^ 2))
      sub <- order(d)[1L:h]
      X_sub <- rows[sub, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      detval <- .robpca_det_from_chol(C)
      if (detval <= 0.0) next
    }
    d <- tryCatch(.robpca_mahalanobis(rows, mu, C), error = function(e) NULL)
    if (is.null(d)) next
    cur <- order(d)[1L:h]
    for (step in 1:2) {
      X_sub <- rows[cur, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      detval <- .robpca_det_from_chol(C)
      if (detval <= 0.0) break
      d <- tryCatch(.robpca_mahalanobis(rows, mu, C), error = function(e) NULL)
      if (is.null(d)) break
      cur <- order(d)[1L:h]
    }
    X_sub <- rows[cur, , drop = FALSE]
    mu <- colMeans(X_sub)
    C <- cov(X_sub)
    detval <- .robpca_det_from_chol(C)
    cands[[length(cands) + 1L]] <- list(det = detval, idx = cur)
  }
  if (length(cands) == 0L) return(NULL)
  dets <- vapply(cands, function(x) x$det, numeric(1L))
  ord <- order(dets)
  best <- NULL
  topn <- min(10L, length(cands))
  for (j in seq_len(topn)) {
    cand <- cands[[ord[j]]]
    got <- .robpca_c_steps(rows, cand$idx)
    if (is.null(best) || got$det < best$det) best <- got
  }
  best
}

classify_outliers <- function(sd, od, sd_cut, od_cut) {
  n <- length(sd)
  out <- character(n)
  for (i in seq_len(n)) {
    far_in <- sd[i] > sd_cut
    far_off <- od[i] > od_cut
    if (far_in && far_off) {
      out[i] <- "bad leverage"
    } else if (far_in) {
      out[i] <- "good leverage"
    } else if (far_off) {
      out[i] <- "orthogonal outlier"
    } else {
      out[i] <- "regular"
    }
  }
  out
}

.robpca_choose_k <- function(l0, k, kmax, r1) {
  pos <- l0[l0 > 1e-12]
  r <- length(pos)
  if (is.numeric(k) && length(k) == 1L && !is.logical(k)) {
    k <- as.integer(k)
    if (k < 1L || k > r)
      stop(sprintf("robpca: k must lie in [1, %d], the rank of the preliminary scatter", r))
    return(k)
  }
  rule <- if (is.null(k)) "cumulative" else as.character(k)
  if (rule == "cumulative") {
    tot <- sum(pos)
    run <- 0.0
    for (j in seq_along(pos)) {
      run <- run + pos[j]
      if (run / tot >= 0.90) return(min(j, kmax, r1))
    }
    return(min(r, kmax, r1))
  }
  if (rule == "ratio") {
    kk <- sum(pos / pos[1L] >= 1e-3)
    return(max(1L, min(kk, kmax, r1)))
  }
  stop("robpca: k must be an integer, 'cumulative' (eq. 5) or 'ratio' (eq. 6)")
}

.robpca_drop_direction <- function(rows, v) {
  p <- ncol(rows)
  norm <- sqrt(sum(v * v))
  u <- v / norm
  basis <- matrix(0, nrow = p, ncol = p - 1L)
  nbasis <- 0L
  for (j in seq_len(p)) {
    e <- numeric(p)
    e[j] <- 1
    w <- e - u * u[j]
    if (nbasis > 0L) {
      for (b in seq_len(nbasis)) {
        dot <- sum(w * basis[, b])
        w <- w - dot * basis[, b]
      }
    }
    nw <- sqrt(sum(w * w))
    if (nw > 1e-9) {
      nbasis <- nbasis + 1L
      basis[, nbasis] <- w / nw
    }
    if (nbasis == p - 1L) break
  }
  if (nbasis == 0L) return(matrix(0, nrow = nrow(rows), ncol = 0L))
  rows %*% basis[, seq_len(nbasis), drop = FALSE]
}

.robpca_reweight_factor <- function(q, k) {
  denom <- pchisq(qchisq(q, k), k + 2)
  if (denom > 0) q / denom else 1.0
}

.robpca_od_cutoff <- function(od, h) {
  v <- od ^ (2.0 / 3.0)
  if (length(v) < 2L) return(Inf)
  hh <- min(max(h, 2L), length(v))
  res <- univariate_mcd(v, h = hh)
  cut <- res["loc"] + res["scale"] * .Z975
  if (cut <= 0) return(0.0)
  cut ^ 1.5
}

morie_robpca <- function(X, k = NULL, alpha = 0.75, kmax = 10L,
                         n_dirs = 250L, n_start = 250L,
                         seed = 17L, reweight = TRUE) {
  rows <- .robpca_matrix(X)
  n <- nrow(rows)
  p <- ncol(rows)
  if (alpha < 0.5 || alpha > 1.0)
    stop("robpca: alpha must lie in [0.5, 1]")
  if (n < 3L)
    stop("robpca: need at least three observations")
  kmax <- as.integer(kmax)
  if (kmax < 1L)
    stop("robpca: kmax must be positive")

  # Stage 1: affine subspace via SVD
  mu0 <- colMeans(rows)
  Xc <- sweep(rows, 2, mu0, "-")
  s_full <- svd(Xc, nu = 0L, nv = 0L)$d
  tol <- max(n, p) * (if (length(s_full) > 0L) max(s_full) else 0) * 2.22e-16
  r0 <- sum(s_full > tol)
  if (r0 == 0L)
    stop("robpca: every observation is identical")
  sv <- svd(Xc, nu = 0L, nv = r0)
  V <- sv$v
  Z <- Xc %*% V

  h <- max(as.integer(alpha * n), (n + kmax + 1L) %/% 2L)
  h <- min(h, n)
  if (h < 2L)
    stop("robpca: h came out below 2; n is too small")

  # Stage 2: outlyingness with exact-fit reduction
  work <- Z
  outl <- NULL
  for (iter in seq_len(r0)) {
    res <- outlyingness(work, h = h, n_dirs = n_dirs, seed = seed)
    if (is.null(res$exact_fit_direction)) {
      outl <- res$outl
      break
    }
    work <- .robpca_drop_direction(work, res$exact_fit_direction)
    if (ncol(work) == 0L)
      stop("robpca: the data collapsed to a point under repeated exact fits")
  }
  if (is.null(outl))
    stop("robpca: exact fit reduction did not terminate")
  r1 <- ncol(work)
  H0 <- order(outl)[1L:h]

  mu1 <- colMeans(work[H0, , drop = FALSE])
  S0 <- cov(work[H0, , drop = FALSE])
  e0 <- .robpca_eigh_desc(S0)
  l0 <- e0$values
  P0 <- e0$vectors
  pos <- l0[l0 > 1e-12]
  if (length(pos) == 0L)
    stop("robpca: the h least outlying points are identical")
  k0 <- .robpca_choose_k(l0, k, kmax, r1)

  work_c <- sweep(work, 2, mu1, "-")
  Xs <- work_c %*% t(P0[1L:k0, , drop = FALSE])

  # Stage 3: MCD on the subspace
  h3 <- max(h, k0 + 1L)
  h3 <- min(h3, n)
  H0_use <- if (length(H0) >= h3) H0[1L:h3] else order(outl)[1L:h3]
  from_h0 <- .robpca_c_steps(Xs, H0_use)
  rnd <- .robpca_fast_mcd(Xs, h3, n_start = n_start, seed = seed)
  best <- from_h0
  if (!is.null(rnd) && rnd$det < best$det) best <- rnd
  mu4 <- best$mu
  S3 <- best$C

  k1 <- k0
  scale_factor <- 1.0
  d <- tryCatch(.robpca_mahalanobis(Xs, mu4, S3),
                error = function(e) stop("robpca: the scatter on the k-dimensional subspace is singular; ask for fewer components"))
  if (reweight) {
    d2 <- sort(d ^ 2)
    q <- qchisq(min(h3 / n, 0.999999), k1)
    c1 <- if (q > 0) d2[h3] / q else 1.0
    if (c1 <= 0) c1 <- 1.0
    scale_factor <- c1
    d_scaled <- d / sqrt(c1)
    cut <- sqrt(qchisq(0.975, k1))
    keep <- which(d_scaled <= cut)
    if (length(keep) > k1 + 1L) {
      Xk <- Xs[keep, , drop = FALSE]
      mu5 <- colMeans(Xk)
      S4 <- cov(Xk)
      crew <- .robpca_reweight_factor(0.975, k1)
      S4 <- crew * S4
    } else {
      mu5 <- mu4
      S4 <- S3
      keep <- seq_len(n)
    }
  } else {
    keep <- seq_len(n)
    mu5 <- mu4
    S4 <- S3
  }

  e1 <- .robpca_eigh_desc(S4)
  lam <- e1$values
  P2 <- e1$vectors
  kk <- min(k0, sum(lam > 1e-12))
  if (kk == 0L)
    stop("robpca: the robust scatter has no positive eigenvalues")
  lam <- lam[1L:kk]
  P2 <- P2[1L:kk, , drop = FALSE]

  Xs_c <- sweep(Xs, 2, mu5, "-")
  T <- Xs_c %*% t(P2)

  P0_sub <- P0[1L:k0, 1L:r1, drop = FALSE]
  load_r1 <- P2 %*% P0_sub
  loadings <- load_r1 %*% t(V[, 1L:r1, drop = FALSE])

  center_r1 <- mu1 + as.numeric(t(P0_sub) %*% mu5[1L:k0])
  center <- mu0 + as.numeric(V[, 1L:r1, drop = FALSE] %*% center_r1)

  sd <- sqrt(rowSums(sweep(T ^ 2, 2, lam, "/")))
  fitted <- matrix(center, nrow = n, ncol = p, byrow = TRUE) + T %*% loadings
  od <- sqrt(rowSums((rows - fitted) ^ 2))
  sd_cut <- sqrt(qchisq(0.975, kk))
  od_cut <- .robpca_od_cutoff(od, h3)
  cls <- classify_outliers(sd, od, sd_cut, od_cut)

  list(
    estimate = unname(lam[1L]),
    loadings = loadings,
    eigenvalues = lam,
    center = center,
    scores = T,
    k = kk,
    k0 = k0,
    h = h3,
    alpha = alpha,
    rank = r0,
    subspace_rank = r1,
    outlyingness = outl,
    h_subset = sort(best$idx) - 1L,   # 0-based, matching the Python arm
    reweighted_kept = keep - 1L,
    consistency_factor = scale_factor,
    score_distance = sd,
    orthogonal_distance = od,
    sd_cutoff = sd_cut,
    od_cutoff = od_cut,
    classification = cls,
    n_outliers = sum(cls != "regular"),
    n = n,
    p = p,
    reweighted = isTRUE(reweight),
    method = "ROBPCA (Hubert, Rousseeuw & Vanden Branden 2005): projection-pursuit outlyingness, then MCD on the resulting subspace",
    note = "loadings are orthonormal columns in the original p variables; the diagnostic plot is score distance against orthogonal distance, with the four regions of the paper's Figure 1 given in classification"
  )
}
