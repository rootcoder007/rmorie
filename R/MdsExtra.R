.mdsx_torg <- function(D, p) {
  D2 <- D^2
  B <- -0.5 * (D2 - outer(rowMeans(D2), colMeans(D2), `+`) + mean(D2))
  z <- eigen(B, symmetric = TRUE)
  z$vectors[, 1:p, drop = FALSE] %*% diag(sqrt(pmax(z$values[1:p], 0)), p)
}

.mdsx_geninv <- function(V) {
  n <- nrow(V)
  solve(V + 1 / n) - 1 / n
}

.mdsx_procrustus <- function(M) {
  s <- svd(M)
  tcrossprod(s$u, s$v)
}

.mdsx_norm2 <- function(M, how) if (how == "smacof") norm(M)^2 else sum(M^2)

#' Three-way SMACOF (INDSCAL, IDIOSCAL, replicated MDS)
#'
#' Ratio three-way SMACOF as \code{smacof::smacofIndDiff}: each source gets
#' \eqn{X_j = Z C_j} with diagonal \eqn{C_j} (\code{"indscal"}, subject
#' weights), general \eqn{C_j} (\code{"idioscal"}) or \eqn{C_j = I}
#' (\code{"identity"}, replicated MDS). Unconstrained Guttman transforms are
#' projected on the constraint each iteration; dissimilarities are
#' normalised per source, missing ones get weight 0; the start is the
#' Torgerson solution of the summed dissimilarities (Carroll and Chang 1970;
#' De Leeuw and Mair 2009). Identical to the Python arm
#' \code{morie.fn.mdsext.smacof_indiff}.
#'
#' @param deltas List of dissimilarity matrices.
#' @param ndim Dimensions.
#' @param constraint \code{"indscal"}, \code{"idioscal"} or \code{"identity"}.
#' @param itmax Maximum iterations.
#' @param eps Convergence tolerance on raw stress.
#' @return List with conf, gspace, cweights, stress, sps, spp, confdist, niter.
#' @references Carroll, J. D. and Chang, J. J. (1970). Analysis of individual
#'   differences in multidimensional scaling via an N-way generalization of
#'   Eckart-Young decomposition. Psychometrika 35, 283-319.
#'
#'   De Leeuw, J. and Mair, P. (2009). Multidimensional scaling using
#'   majorization: SMACOF in R. Journal of Statistical Software 31(3), 1-30.
#' @examples
#' D1 <- as.matrix(dist(1:4))
#' SmacofIndDiff(list(D1, D1), 1, constraint = "identity")$stress
#' @export
SmacofIndDiff <- function(deltas, ndim = 2, constraint = "indscal", itmax = 1000L, eps = 1e-6) {
  if (!constraint %in% c("indscal", "idioscal", "identity")) {
    stop("constraint must be indscal, idioscal or identity", call. = FALSE)
  }
  Ds <- lapply(deltas, as.matrix)
  m <- length(Ds)
  n <- nrow(Ds[[1]])
  if (any(vapply(Ds, nrow, 0L) != n)) stop("all dissimilarity matrices must have the same size", call. = FALSE)
  if (ndim > n - 1) stop("ndim must be at most n - 1", call. = FALSE)
  p <- ndim
  lt <- lower.tri(Ds[[1]])
  nn <- sum(lt)
  w <- lapply(Ds, function(D) ifelse(is.na(D[lt]), 0, 1))
  dl <- lapply(Ds, function(D) ifelse(is.na(D[lt]), 0, D[lt]))
  dh <- lapply(seq_len(m), function(j) dl[[j]] * sqrt(nn / sum(w[[j]] * dl[[j]]^2)))
  full <- function(v) {
    M <- matrix(0, n, n)
    M[lt] <- v
    M + t(M)
  }
  V <- lapply(w, function(wj) {
    W <- full(wj)
    diag(rowSums(W)) - W
  })
  Vp <- lapply(V, .mdsx_geninv)
  Z <- .mdsx_torg(full(Reduce(`+`, dl)), p)
  C <- rep(list(diag(p)), m)
  dd <- function(Y) as.matrix(stats::dist(Y))[lt]
  d0 <- dd(Z)
  lb <- sum(vapply(seq_len(m), function(j) sum(w[[j]] * d0 * dh[[j]]), 0)) /
    sum(vapply(seq_len(m), function(j) sum(w[[j]] * d0^2), 0))
  Z <- lb * Z
  X <- rep(list(Z), m)
  d <- rep(list(lb * d0), m)
  sold <- sum(vapply(seq_len(m), function(j) sum(w[[j]] * (dh[[j]] - d[[j]])^2), 0))
  itel <- 1L
  repeat {
    Y <- lapply(seq_len(m), function(j) {
      b <- ifelse(d[[j]] >= 1e-12, w[[j]] * dh[[j]] / d[[j]], 0)
      Bm <- -full(b)
      diag(Bm) <- -rowSums(Bm)
      Vp[[j]] %*% (Bm %*% X[[j]])
    })
    if (constraint == "identity") {
      Z <- .mdsx_geninv(Reduce(`+`, V)) %*% Reduce(`+`, lapply(seq_len(m), function(j) V[[j]] %*% Y[[j]]))
      Y <- rep(list(Z), m)
    } else if (constraint == "indscal") {
      aux0 <- matrix(0, n, p)
      for (j in seq_len(m)) {
        VY <- V[[j]] %*% Y[[j]]
        cj <- colSums(Z * VY) / colSums(Z * (V[[j]] %*% Z))
        C[[j]] <- diag(cj, p)
        aux0 <- aux0 + sweep(VY, 2, cj, `*`)
      }
      for (s in seq_len(p)) {
        M <- Reduce(`+`, lapply(seq_len(m), function(j) C[[j]][s, s]^2 * V[[j]]))
        Z[, s] <- .mdsx_geninv(M) %*% aux0[, s]
      }
      Y <- lapply(C, function(Cj) Z %*% Cj)
    } else {
      aux0 <- matrix(0, n, p)
      K <- matrix(0, n * p, n * p)
      for (j in seq_len(m)) {
        VY <- V[[j]] %*% Y[[j]]
        C[[j]] <- solve(crossprod(Z, V[[j]] %*% Z), crossprod(Z, VY))
        aux0 <- aux0 + VY %*% t(C[[j]])
        K <- K + kronecker(tcrossprod(C[[j]]), V[[j]])
      }
      Z <- matrix(solve(K + kronecker(diag(p), matrix(1 / n, n, n)), as.vector(aux0)), n, p)
      Y <- lapply(C, function(Cj) Z %*% Cj)
    }
    e <- lapply(Y, dd)
    snon <- sum(vapply(seq_len(m), function(j) sum(w[[j]] * (dh[[j]] - e[[j]])^2), 0))
    if (sold - snon < eps || itel == itmax) break
    X <- Y
    d <- e
    sold <- snon
    itel <- itel + 1L
  }
  confdist <- lapply(seq_len(m), function(j) e[[j]] * sqrt(nn / sum(w[[j]] * e[[j]]^2)))
  spps <- t(vapply(seq_len(m), function(j) {
    R <- full(w[[j]] * (dh[[j]] - confdist[[j]])^2)
    cm <- colSums(R) / (n - 1)
    100 * cm / sum(cm)
  }, numeric(n)))
  sps <- vapply(seq_len(m), function(j) sum(w[[j]] * (dh[[j]] - e[[j]])^2), 0)
  list(conf = lapply(Y, unname), gspace = unname(Z), cweights = lapply(C, unname), stress = sqrt(snon / m / nn),
       sps = 100 * sps / sum(sps), spp = colMeans(spps), confdist = confdist, niter = itel,
       constraint = constraint)
}

#' Jackknife and bootstrap stability of a SMACOF solution
#'
#' \code{MdsJackknife}: leave-one-out refits rotated to a common comparison
#' configuration, with stability, cross-validity and dispersion (De Leeuw and
#' Meulman 1986), as \code{smacof::jackmds}. \code{MdsBootstrap}: row
#' resampling of an \eqn{N \times n} data matrix whose columns are the MDS
#' objects, dissimilarities \eqn{\sqrt{1 - r}} (Pearson or Spearman) or
#' Euclidean, each refit Procrustes-fitted onto the original; per-object
#' covariance matrices, stress percentile interval and stability (Jacoby and
#' Armstrong 2014), as \code{smacof::bootmds}. \code{method = "standard"}
#' uses the full sum in the jackknife Procrustes update and Frobenius norms;
#' \code{method = "smacof"} reproduces smacof 2.1 (update over later objects
#' only, \code{base::norm}'s default one-norm). Identical to the Python arm
#' \code{morie.fn.mdsext}; \code{resamples} are 1-based here.
#'
#' @param delta Dissimilarity matrix.
#' @param data Data matrix, columns are objects.
#' @param ndim Dimensions.
#' @param type SMACOF type (see \code{SmacofMds}).
#' @param method \code{"standard"} or \code{"smacof"}.
#' @param eps,itmax Jackknife convergence tolerance and iteration limit.
#' @param method_dat \code{"pearson"}, \code{"spearman"} or \code{"euclidean"}.
#' @param nrep Bootstrap replicates.
#' @param alpha Stress interval level.
#' @param seed Philox seed.
#' @param resamples Optional list of row-index vectors.
#' @return List.
#' @references De Leeuw, J. and Meulman, J. (1986). A special jackknife for
#'   multidimensional scaling. Journal of Classification 3, 97-112.
#'
#'   Jacoby, W. G. and Armstrong, D. A. (2014). Bootstrap confidence regions
#'   for multidimensional scaling solutions. American Journal of Political
#'   Science 58, 264-278.
#' @examples
#' D <- rbind(c(0, 1, 2, 1), c(1, 0, 1, 2), c(2, 1, 0, 1), c(1, 2, 1, 0))
#' MdsJackknife(D)$stab
#' X <- cbind(1:6, c(2, 1, 5, 3, 6, 4), c(3, 4, 2, 6, 5, 8), c(1, 0, 2, 1, 3, 2))
#' MdsBootstrap(X, 2, method_dat = "euclidean", nrep = 5)$bootci
#' @export
MdsJackknife <- function(delta, ndim = 2, type = "ratio", method = "standard", eps = 1e-6, itmax = 100L) {
  if (!method %in% c("standard", "smacof")) stop("method must be standard or smacof", call. = FALSE)
  D <- as.matrix(delta)
  n <- nrow(D)
  x0 <- SmacofMds(D, ndim, type = type)$conf
  xx <- lapply(seq_len(n), function(i) {
    X <- matrix(0, n, ndim)
    X[-i, ] <- SmacofMds(D[-i, -i], ndim, type = type)$conf
    X
  })
  K <- rep(list(diag(ndim)), n)
  oloss <- Inf
  itel <- 1L
  repeat {
    y0 <- Reduce(`+`, lapply(seq_len(n), function(i) xx[[i]] %*% K[[i]])) * (n - 1) / (n * (n - 2))
    for (i in seq_len(n)) {
      js <- if (method == "smacof") i:n else seq_len(n)
      zz <- Reduce(`+`, lapply(js, function(j) xx[[j]] %*% K[[j]]))
      K[[i]] <- .mdsx_procrustus(crossprod(xx[[i]], zz))
    }
    yy <- lapply(seq_len(n), function(i) {
      Yi <- xx[[i]] %*% K[[i]]
      Yi[i, ] <- n * y0[i, ] / (n - 1)
      sweep(Yi, 2, y0[i, ] / (n - 1))
    })
    nloss <- sum(vapply(yy, function(Yi) sum((y0 - Yi)^2), 0))
    if (oloss - nloss < eps || itel == itmax) break
    itel <- itel + 1L
    oloss <- nloss
  }
  x0 <- x0 %*% .mdsx_procrustus(crossprod(x0, y0))
  den <- sum(vapply(yy, .mdsx_norm2, 0, how = method))
  stab <- 1 - sum(vapply(yy, function(Yi) .mdsx_norm2(Yi - y0, method), 0)) / den
  cross <- 1 - n * .mdsx_norm2(x0 - y0, method) / den
  list(smacof_conf = x0, jackknife_conf = yy, comparison_conf = y0, stab = stab, cross = cross,
       disp = 2 - (stab + cross), niter = itel, loss = nloss)
}

.mdsx_diss <- function(X, how) {
  if (how == "euclidean") return(as.matrix(stats::dist(t(X))))
  if (!how %in% c("pearson", "spearman")) stop("method_dat must be pearson, spearman or euclidean", call. = FALSE)
  if (how == "spearman") X <- apply(X, 2, rank)
  r <- stats::cor(X)
  D <- sqrt(pmax(1 - r, 0))
  diag(D) <- 0
  D
}

#' @rdname MdsJackknife
#' @export
MdsBootstrap <- function(data, ndim = 2, method_dat = "pearson", nrep = 100L, alpha = 0.05, type = "ratio",
                         method = "standard", seed = 1, resamples = NULL) {
  if (!method %in% c("standard", "smacof")) stop("method must be standard or smacof", call. = FALSE)
  X <- as.matrix(data)
  N <- nrow(X)
  n <- ncol(X)
  fit0 <- SmacofMds(.mdsx_diss(X, method_dat), ndim, type = type)
  X0 <- fit0$conf
  mx <- colMeans(X0)
  Xc <- sweep(X0, 2, mx)
  if (is.null(resamples)) {
    resamples <- lapply(seq_len(nrep) - 1, function(r) pmin(N - 1, floor(.morie_random_uniform(N, seed, r) * N)) + 1)
  }
  stressvec <- numeric(0)
  coord <- lapply(resamples, function(idx) {
    o <- SmacofMds(.mdsx_diss(X[idx, , drop = FALSE], method_dat), ndim, type = type)
    stressvec[length(stressvec) + 1] <<- o$stress
    Yc <- sweep(o$conf, 2, colMeans(o$conf))
    Tm <- t(.mdsx_procrustus(crossprod(Xc, Yc)))
    YT <- Yc %*% Tm
    sweep(sum(Xc * YT) / sum(Yc^2) * YT, 2, mx, `+`)
  })
  R <- length(coord)
  covs <- lapply(seq_len(n), function(k) stats::cov(t(vapply(coord, function(cd) cd[k, ], numeric(ndim)))))
  y0 <- Reduce(`+`, coord) / R
  stab <- 1 - sum(vapply(coord, function(cd) .mdsx_norm2(cd - y0, method), 0)) /
    sum(vapply(coord, .mdsx_norm2, 0, how = method))
  list(conf = X0, bootconf = coord, cov = covs, stressvec = stressvec,
       bootci = unname(stats::quantile(stressvec, c(alpha / 2, 1 - alpha / 2))), stab = stab, nrep = R,
       stress = fit0$stress)
}

#' Oblique Procrustes rotation and configuration orientation checks
#'
#' \code{ProcrustesOblique}: Browne's oblique target rotation
#' \eqn{L = A (T')^{-1}} minimising \eqn{\|L - B\|^2} with unit-length columns
#' of \eqn{T}, by Jennrich's gradient projection (as
#' \code{GPArotation::targetQ}: Barzilai-Borwein steps with a non-monotone
#' Armijo search over the last \code{fwindow} iterations); \code{NA} targets are
#' unspecified. \code{MdsReflect}: axis signs agreeing with a target, or
#' making each axis's largest-magnitude coordinate positive.
#' \code{MdsFlip}: orthogonal Procrustes of \code{Y} onto \code{X}; a
#' negative determinant flags a reflection, with per-axis correlations.
#' \code{MdsPolarity}: orient each dimension so its anchor object is
#' non-negative (W-NOMINATE polarity), with the poles. \code{MdsAnisotropy}:
#' covariance eigenvalues, ratio \eqn{\lambda_1 / \lambda_p}, principal-axis
#' angle in degrees and eccentricity. Identical to the Python arm
#' \code{morie.fn.mdsext}; anchors and poles are 1-based here.
#'
#' @param A Loading or configuration matrix.
#' @param target Target matrix.
#' @param eps,maxit Convergence tolerance and iteration limit.
#' @param fwindow Non-monotone line-search window.
#' @param X,Y Configurations.
#' @param anchors Row index per dimension.
#' @return List.
#' @references Browne, M. W. (2001). An overview of analytic rotation in
#'   exploratory factor analysis. Multivariate Behavioral Research 36, 111-150.
#'
#'   Jennrich, R. I. (2002). A simple general method for oblique rotation.
#'   Psychometrika 67, 7-19.
#'
#'   Borg, I. and Groenen, P. J. F. (2005). Modern Multidimensional Scaling,
#'   2nd ed. Springer.
#' @examples
#' A <- rbind(c(0.8, 0.1), c(0.7, 0.2), c(0.2, 0.9), c(0.1, 0.7))
#' ProcrustesOblique(A, rbind(c(1, 0), c(1, 0), c(0, 1), c(0, 1)))$Phi
#' MdsFlip(rbind(c(0, 0), c(1, 0), c(0, 2)), rbind(c(0, 0), c(-1, 0), c(0, 2)))$reflected
#' MdsAnisotropy(rbind(c(-2, 0), c(2, 0), c(0, -1), c(0, 1)))$ratio
#' @export
ProcrustesOblique <- function(A, target, eps = 1e-8, maxit = 5000L, fwindow = 10L) {
  A <- as.matrix(A)
  B <- as.matrix(target)
  k <- ncol(A)
  crit <- function(Tm) {
    Ti <- solve(Tm)
    L <- A %*% t(Ti)
    Gq <- 2 * (L - B)
    Gq[is.na(Gq)] <- 0
    list(L = L, f = sum((L - B)^2, na.rm = TRUE), G = -t(t(L) %*% Gq %*% Ti))
  }
  Tm <- diag(k)
  cur <- crit(Tm)
  alpha <- 1
  fs <- numeric(0)
  T_prev <- NULL
  it <- 0L
  repeat {
    Gp <- cur$G - Tm %*% diag(colSums(Tm * cur$G), k)
    s <- sqrt(sum(Gp^2))
    fs <- c(fs, cur$f)
    if (s < eps || it == maxit + 1L) break
    if (!is.null(T_prev)) {
      dT <- Tm - T_prev
      dG <- Gp - Gp_prev
      if (sum(dG^2) > 0) alpha <- max(1e-10, min(sum(dT^2) / abs(sum(dT * dG)), 20))
    } else {
      alpha <- 2 * alpha
    }
    target_f <- max(utils::tail(fs, fwindow))
    for (i in 0:10) {
      Xm <- Tm - alpha * Gp
      Tt <- Xm %*% diag(1 / sqrt(colSums(Xm^2)), k)
      nxt <- crit(Tt)
      if (target_f - nxt$f > 0.5 * s^2 * alpha) break
      alpha <- alpha / 2
    }
    T_prev <- Tm
    Gp_prev <- Gp
    Tm <- Tt
    cur <- nxt
    it <- it + 1L
  }
  list(loadings = unname(cur$L), Phi = unname(crossprod(Tm)), T = unname(Tm), f = cur$f, iterations = it,
       converged = s < eps)
}

#' @rdname ProcrustesOblique
#' @export
MdsReflect <- function(X, target = NULL) {
  X <- as.matrix(X)
  signs <- vapply(seq_len(ncol(X)), function(c) {
    if (!is.null(target)) return(if (sum(X[, c] * as.matrix(target)[, c]) < 0) -1 else 1)
    big <- which.max(abs(X[, c]))
    if (X[big, c] < 0) -1 else 1
  }, 0)
  list(conf = unname(sweep(X, 2, signs, `*`)), signs = signs)
}

#' @rdname ProcrustesOblique
#' @export
MdsFlip <- function(X, Y) {
  Xc <- scale(as.matrix(X), scale = FALSE)
  Yc <- scale(as.matrix(Y), scale = FALSE)
  s <- svd(crossprod(Xc, Yc))
  R <- s$v %*% t(s$u)
  cr <- vapply(seq_len(ncol(Xc)), function(c) {
    sx <- sqrt(sum(Xc[, c]^2))
    sy <- sqrt(sum(Yc[, c]^2))
    if (sx > 0 && sy > 0) sum(Xc[, c] * Yc[, c]) / (sx * sy) else 0
  }, 0)
  dt <- det(R)
  list(rotation = R, determinant = dt, reflected = dt < 0, axis_correlations = cr, flipped_axes = which(cr < 0),
       ss_before = sum((Xc - Yc)^2), ss_after = sum((Xc - Yc %*% R)^2))
}

#' @rdname ProcrustesOblique
#' @export
MdsPolarity <- function(X, anchors) {
  X <- as.matrix(X)
  if (length(anchors) != ncol(X)) stop("one anchor per dimension", call. = FALSE)
  signs <- vapply(seq_len(ncol(X)), function(c) if (X[anchors[c], c] < 0) -1 else 1, 0)
  C <- sweep(X, 2, signs, `*`)
  list(conf = unname(C), signs = signs, poles = lapply(seq_len(ncol(C)), function(c) c(which.min(C[, c]), which.max(C[, c]))))
}

#' @rdname ProcrustesOblique
#' @export
MdsAnisotropy <- function(X) {
  X <- as.matrix(X)
  k <- ncol(X)
  e <- eigen(stats::cov(X), symmetric = TRUE)
  w <- e$values
  v1 <- e$vectors[, 1]
  ang <- if (k > 1) (atan2(v1[2], v1[1]) * 180 / pi) %% 180 else 0
  if (ang >= 180 - 1e-12) ang <- 0
  list(eigenvalues = w, ratio = if (w[k] > 0) w[1] / w[k] else Inf, angle = ang,
       eccentricity = if (w[1] > 0) sqrt(max(0, 1 - w[k] / w[1])) else 0)
}
