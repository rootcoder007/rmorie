.mds_pava <- function(y, w) {
  v <- numeric(0)
  wt <- numeric(0)
  sz <- integer(0)
  for (i in seq_along(y)) {
    v <- c(v, y[i])
    wt <- c(wt, w[i])
    sz <- c(sz, 1L)
    k <- length(v)
    while (k > 1 && v[k - 1] > v[k]) {
      tw <- wt[k - 1] + wt[k]
      v[k - 1] <- if (tw > 0) (v[k - 1] * wt[k - 1] + v[k] * wt[k]) / tw else (v[k - 1] + v[k]) / 2
      wt[k - 1] <- tw
      sz[k - 1] <- sz[k - 1] + sz[k]
      v <- v[-k]
      wt <- wt[-k]
      sz <- sz[-k]
      k <- k - 1
    }
  }
  rep(v, sz)
}

#' Symmetric SMACOF multidimensional scaling
#'
#' Guttman-transform majorisation of raw stress from the Torgerson start
#' with ratio, interval or ordinal (primary ties) optimal scaling and
#' disparities normalised to \eqn{\sum w\hat d^2 = n(n-1)/2}, stopping when
#' the stress decrease falls below \code{eps}, exactly as
#' \code{smacof::smacofSym} (De Leeuw and Mair 2009). NA dissimilarities or
#' zero weights are missing. Identical to the Python arm
#' \code{morie.fn.mdsops.smacof}.
#'
#' @param delta Symmetric dissimilarity matrix.
#' @param ndim Dimensions.
#' @param type \code{"ratio"}, \code{"interval"} or \code{"ordinal"}.
#' @param weights Weight matrix.
#' @param init Starting configuration (default Torgerson).
#' @param itmax Maximum iterations.
#' @param eps Convergence tolerance.
#' @return List with conf, stress, dhat, confdist, spp, niter.
#' @references De Leeuw, J. and Mair, P. (2009). Multidimensional scaling
#'   using majorization: SMACOF in R. Journal of Statistical Software 31(3),
#'   1-30.
#' @examples
#' D <- rbind(c(0, 3, 4, 6), c(3, 0, 5, 4), c(4, 5, 0, 3), c(6, 4, 3, 0))
#' SmacofMds(D)$stress
#' @export
SmacofMds <- function(delta, ndim = 2, type = "ratio", weights = NULL, init = NULL, itmax = 1000L, eps = 1e-6) {
  if (!type %in% c("ratio", "interval", "ordinal")) stop("type must be ratio, interval or ordinal")
  D <- as.matrix(delta)
  n <- nrow(D)
  if (ndim > n - 1) stop("ndim must be at most n - 1")
  W <- if (is.null(weights)) matrix(1, n, n) else as.matrix(weights)
  lt <- lower.tri(D)
  dl <- D[lt]
  w <- ifelse(is.na(dl), 0, W[lt])
  nn <- length(dl)
  obs <- which(!is.na(dl))
  Df <- D
  Df[is.na(Df)] <- mean(dl, na.rm = TRUE)
  X <- if (is.null(init)) {
    D2 <- Df^2
    B <- -0.5 * (D2 - outer(rowMeans(D2), colMeans(D2), `+`) + mean(D2))
    z <- eigen(B, symmetric = TRUE)
    z$vectors[, 1:ndim, drop = FALSE] %*% diag(sqrt(pmax(z$values[1:ndim], 0)), ndim)
  } else {
    as.matrix(init)
  }
  dhat <- ifelse(is.na(dl), 1, dl * sqrt(nn / sum(w[obs] * dl[obs]^2)))
  Wm <- matrix(0, n, n)
  Wm[lt] <- w
  Wm <- Wm + t(Wm)
  V <- diag(rowSums(Wm)) - Wm
  Vp <- solve(V + 1 / n) - 1 / n
  iord <- obs[order(dl[obs])]
  blk <- split(iord, cumsum(c(TRUE, diff(dl[iord]) != 0)))
  trans <- function(e) {
    R <- numeric(nn)
    if (type == "ratio") {
      R[obs] <- dl[obs]
    } else if (type == "interval") {
      wb <- vapply(blk, function(b) sum(w[b]), 0)
      yb <- vapply(blk, function(b) if (sum(w[b]) > 0) sum(w[b] * e[b]) / sum(w[b]) else 0, 0)
      xb <- vapply(blk, function(b) dl[b[1]], 0) - dl[blk[[1]][1]]
      A <- cbind(1, xb)
      cf <- solve(crossprod(A * sqrt(wb)), crossprod(A * sqrt(wb), sqrt(wb) * yb))
      for (t in seq_along(blk)) R[blk[[t]]] <- cf[1] + cf[2] * xb[t]
    } else {
      o <- obs[order(dl[obs], e[obs])]
      R[o] <- .mds_pava(e[o], w[o])
    }
    R * sqrt(nn / sum(w * R^2))
  }
  dd <- function(Y) as.matrix(stats::dist(Y))[lt]
  d <- dd(X)
  lb <- sum(w * d * dhat) / sum(w * d^2)
  X <- lb * X
  d <- lb * d
  sold <- sum(w * (dhat - d)^2) / nn
  itel <- 1
  repeat {
    b <- matrix(0, n, n)
    b[lt] <- ifelse(d < 1e-12, 0, w * dhat / d)
    b <- b + t(b)
    Bm <- diag(rowSums(b)) - b
    Y <- Vp %*% (Bm %*% X)
    e <- dd(Y)
    dhat <- trans(e)
    snon <- sum(w * (dhat - e)^2) / nn
    if (sold - snon < eps || itel == itmax) break
    X <- Y
    d <- e
    sold <- snon
    itel <- itel + 1
  }
  res <- matrix(0, n, n)
  res[lt] <- w * (dhat - e)^2
  res <- res + t(res)
  cm <- colSums(res) / (n - 1)
  list(conf = unname(Y), stress = sqrt(snon), dhat = ifelse(is.na(dl), NA, dhat), confdist = e,
       spp = 100 * cm / sum(cm), niter = itel, weights = w)
}

.mds_proc <- function(A, B, sc) {
  s <- svd(crossprod(A, B))
  R <- s$v %*% t(s$u)
  c <- if (sc) sum(s$d) / sum(B^2) else 1
  list(R = R, c = c, Yr = c * B %*% R, ss = sum(A^2) + c^2 * sum(B^2) - 2 * c * sum(s$d))
}

#' Procrustes analysis (vegan procrustes and protest conventions)
#'
#' \code{ProcrustesFit}: centred (optionally unit-scaled) configurations,
#' rotation \eqn{A = VU'} from \eqn{X'Y = USV'}, dilation
#' \eqn{\sum S/tr(Y'Y)}, residuals and Procrustes correlation (Gower 1971;
#' Peres-Neto and Jackson 2001). \code{GeneralizedProcrustes}: orthogonal
#' generalised Procrustes analysis (Gower 1975).
#'
#' @param X Target configuration.
#' @param Y Configuration to rotate.
#' @param scale Fit a dilation.
#' @param symmetric Scale both to unit sum of squares.
#' @param configs List of configurations.
#' @param tol Convergence tolerance.
#' @param maxit Maximum iterations.
#' @return List.
#' @references Gower, J. C. (1975). Generalized Procrustes analysis.
#'   Psychometrika 40, 33-51.
#'
#'   Peres-Neto, P. R. and Jackson, D. A. (2001). How well do multivariate
#'   data sets match? Oecologia 129, 169-178.
#' @examples
#' ProcrustesFit(rbind(c(0, 0), c(1, 0), c(0, 1)), rbind(c(0, 0), c(0, 2), c(-2, 0)))$scale
#' @export
ProcrustesFit <- function(X, Y, scale = TRUE, symmetric = FALSE) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  if (nrow(X) != nrow(Y)) stop("X and Y must have the same number of rows")
  k <- max(ncol(X), ncol(Y))
  if (ncol(X) < k) X <- cbind(X, matrix(0, nrow(X), k - ncol(X)))
  if (ncol(Y) < k) Y <- cbind(Y, matrix(0, nrow(Y), k - ncol(Y)))
  mx <- colMeans(X)
  my <- colMeans(Y)
  Xc <- sweep(X, 2, mx)
  Yc <- sweep(Y, 2, my)
  if (symmetric) {
    Xc <- Xc / sqrt(sum(Xc^2))
    Yc <- Yc / sqrt(sum(Yc^2))
  }
  f <- .mds_proc(Xc, Yc, scale)
  fs <- .mds_proc(Xc / sqrt(sum(Xc^2)), Yc / sqrt(sum(Yc^2)), TRUE)
  list(Yrot = unname(f$Yr), rotation = f$R, scale = f$c, translation = as.vector(mx - f$c * my %*% f$R), ss = f$ss,
       residuals = sqrt(rowSums((Xc - f$Yr)^2)), correlation = sqrt(max(0, 1 - fs$ss)))
}

#' @rdname ProcrustesFit
#' @export
GeneralizedProcrustes <- function(configs, tol = 1e-12, maxit = 1000L) {
  m <- length(configs)
  if (m < 2) stop("need at least two configurations of equal shape")
  cur <- lapply(configs, function(M) sweep(as.matrix(M), 2, colMeans(as.matrix(M))))
  orig <- cur
  G <- Reduce(`+`, cur) / m
  rss <- function(F, G) sum(vapply(F, function(M) sum((M - G)^2), 0))
  old <- rss(cur, G)
  it <- 0
  for (it in seq_len(maxit)) {
    for (t in seq_len(m)) {
      others <- (G * m - cur[[t]]) / (m - 1)
      cur[[t]] <- ProcrustesFit(others, orig[[t]], scale = FALSE)$Yrot
      G <- Reduce(`+`, cur) / m
    }
    new <- rss(cur, G)
    if (old - new < tol) {
      old <- new
      break
    }
    old <- new
  }
  list(fitted = cur, consensus = G, rss = old, iterations = it)
}

#' Embedding quality, alienation coefficient and dissimilarity checks
#'
#' \code{EmbeddingQuality}: trustworthiness and continuity (Venna and Kaski
#' 2001), \eqn{Q_{NX}} (Lee and Verleysen 2009) and LCMC (Chen and Buja 2009)
#' at neighbourhood size \code{k}. \code{AlienationCoefficient}: Guttman's
#' \eqn{K = \sqrt{1 - \mu^2}} with \eqn{\mu} the congruence of distances and
#' disparities (Borg and Groenen 2005). \code{DissimilarityCheck}: symmetry,
#' triangle inequality violations and Euclidean embeddability (Gower 1966).
#'
#' @param D_high,D_low Distance matrices.
#' @param k Neighbourhood size.
#' @param dhat,d Disparities and distances.
#' @param delta Dissimilarity matrix.
#' @param tol Tolerance.
#' @return List or number.
#' @references Venna, J. and Kaski, S. (2001). Neighborhood preservation in
#'   nonlinear projection methods: an experimental study. ICANN 2001, LNCS
#'   2130, 485-491.
#'
#'   Borg, I. and Groenen, P. J. F. (2005). Modern Multidimensional Scaling,
#'   2nd edn. Springer.
#' @examples
#' D <- as.matrix(dist(1:4))
#' EmbeddingQuality(D, D, 1)$trustworthiness
#' AlienationCoefficient(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' DissimilarityCheck(rbind(c(0, 1, 5), c(1, 0, 1), c(5, 1, 0)))$triangle_violations
#' @export
EmbeddingQuality <- function(D_high, D_low, k) {
  Dh <- as.matrix(D_high)
  Dl <- as.matrix(D_low)
  n <- nrow(Dh)
  if (k < 1 || k >= n / 2) stop("k must satisfy 1 <= k < n/2")
  rk <- function(D) {
    R <- matrix(0, n, n)
    for (i in seq_len(n)) {
      j <- setdiff(seq_len(n), i)
      R[i, j[order(D[i, j], j)]] <- seq_along(j)
    }
    R
  }
  Rh <- rk(Dh)
  Rl <- rk(Dl)
  off <- row(Rh) != col(Rh)
  inh <- Rh <= k & off
  inl <- Rl <= k & off
  norm <- 2 / (n * k * (2 * n - 3 * k - 1))
  qnx <- sum(inh & inl) / (n * k)
  list(trustworthiness = 1 - norm * sum((Rh - k)[inl & !inh]), continuity = 1 - norm * sum((Rl - k)[inh & !inl]),
       qnx = qnx, lcmc = qnx - k / (n - 1))
}

#' @rdname EmbeddingQuality
#' @export
AlienationCoefficient <- function(dhat, d) sqrt(max(0, 1 - (sum(d * dhat) / sqrt(sum(d^2) * sum(dhat^2)))^2))

#' @rdname EmbeddingQuality
#' @export
DissimilarityCheck <- function(delta, tol = 1e-12) {
  D <- as.matrix(delta)
  n <- nrow(D)
  viol <- 0
  mx <- 0
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      t <- setdiff(seq_len(n), c(i, j))
      ex <- if (length(t)) max(D[i, j] - D[i, t] - D[t, j]) else 0
      if (ex > tol) {
        viol <- viol + 1
        mx <- max(mx, ex)
      }
    }
  }
  D2 <- D^2
  B <- -0.5 * (D2 - outer(rowMeans(D2), colMeans(D2), `+`) + mean(D2))
  ev <- sort(eigen(B, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  list(symmetric = isTRUE(all(D == t(D))), zero_diagonal = all(diag(D) == 0), nonnegative = all(D >= 0),
       triangle_violations = viol, max_violation = mx, euclidean = min(ev) >= -1e-10 * (if (max(abs(ev)) > 0) max(abs(ev)) else 1),
       eigenvalues = ev)
}

#' Classical (Torgerson) multidimensional scaling
#'
#' Eigendecomposition of \eqn{B = -J D^2 J / 2}; coordinates are the leading
#' eigenvectors scaled by the square roots of their (non-negative)
#' eigenvalues, as \code{stats::cmdscale} (Torgerson 1952); stress-1 of the
#' fitted distances as the Python arm \code{morie.fn.mds.mds}.
#'
#' @param D Distance matrix.
#' @param n_dims Dimensions.
#' @return List with \code{coordinates}, \code{eigenvalues}, \code{stress}.
#' @references Torgerson, W. S. (1952). Multidimensional scaling: I. Theory
#'   and method. Psychometrika 17, 401-419.
#' @examples
#' ClassicalMds(rbind(c(0, 3, 4, 5), c(3, 0, 5, 4), c(4, 5, 0, 3), c(5, 4, 3, 0)))$eigenvalues
#' @export
ClassicalMds <- function(D, n_dims = 2) {
  D <- as.matrix(D)
  D2 <- D^2
  B <- -0.5 * (D2 - outer(rowMeans(D2), colMeans(D2), `+`) + mean(D2))
  z <- eigen(B, symmetric = TRUE)
  k <- min(n_dims, nrow(D))
  X <- z$vectors[, seq_len(k), drop = FALSE] %*% diag(sqrt(pmax(z$values[seq_len(k)], 0)), k)
  Dh <- as.matrix(stats::dist(X))
  list(coordinates = X, eigenvalues = z$values, stress = if (sum(D^2) > 0) sqrt(sum((D - Dh)^2) / sum(D^2)) else 0)
}
