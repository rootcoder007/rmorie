#' morie_pace
#' Internal: kernel weight for the PACE smoothers
#'
#' Epanechnikov on \eqn{|u| \le 1}, or the Gaussian kernel without its
#' normalising constant -- the constant cancels in a local linear fit,
#' and leaving it out is what the Python arm does.
#'
#' @param u Scaled distance.
#' @param kernel \code{"epan"} or \code{"gauss"}.
#' @return A numeric weight.
#' @keywords internal
#' @noRd
.pace_kweight <- function(u, kernel) {
  if (kernel == "gauss") return(exp(-0.5 * u * u))
  a <- 1 - u * u
  ifelse(a > 0, 0.75 * a, 0)
}

#' Local linear smoother on a one-dimensional grid
#'
#' Weighted least squares of \code{y} on \code{t - t0} at each point of
#' \code{at}, taking the intercept. Where the design is degenerate --
#' one distinct point inside the bandwidth -- the fit falls back to the
#' weighted mean, which is the local CONSTANT fit and the only thing
#' identified there.
#'
#' @param t,y The observation times and values, pooled over subjects.
#' @param at Points to evaluate at.
#' @param bw Bandwidth.
#' @param kernel \code{"epan"} or \code{"gauss"}.
#' @return A numeric vector, one fitted value per point of \code{at}.
#' @export
morie_pace_local_linear <- function(t, y, at, bw, kernel = "epan") {
  if (!kernel %in% c("epan", "gauss")) {
    stop("pace: kernel must be epan or gauss, got '", kernel, "'.",
         call. = FALSE)
  }
  if (bw <= 0) stop("pace: the bandwidth must be positive.", call. = FALSE)
  t <- as.numeric(t)
  y <- as.numeric(y)
  vapply(as.numeric(at), function(t0) {
    d <- t - t0
    w <- .pace_kweight(d / bw, kernel)
    keep <- w != 0
    if (!any(keep)) {
      stop(sprintf("pace: bandwidth %g leaves the point %g with no data.",
                   bw, t0), call. = FALSE)
    }
    w <- w[keep]
    dd <- d[keep]
    yy <- y[keep]
    s0 <- sum(w)
    s1 <- sum(w * dd)
    s2 <- sum(w * dd * dd)
    b0 <- sum(w * yy)
    b1 <- sum(w * dd * yy)
    det <- s0 * s2 - s1 * s1
    if (abs(det) < 1e-12) b0 / s0 else (s2 * b0 - s1 * b1) / det
  }, numeric(1))
}

#' Local linear smoother on a two-dimensional grid
#'
#' The covariance surface: a plane fitted in \code{(s, t)} around each
#' grid point, taking the intercept. Points are bucketed by bandwidth
#' so each fit touches only the neighbouring cells -- the Epanechnikov
#' kernel vanishes past one bandwidth, the Gaussian is truncated at
#' four, which is the reach the Python arm uses.
#'
#' @param s,t,z The two coordinates and the value at each pair.
#' @param at_s,at_t Grids to evaluate on.
#' @param bw Bandwidth, shared by both coordinates.
#' @param kernel \code{"epan"} or \code{"gauss"}.
#' @return A matrix with one row per \code{at_s} and column per
#'   \code{at_t}.
#' @export
morie_pace_local_linear_2d <- function(s, t, z, at_s, at_t, bw,
                                       kernel = "epan") {
  if (!kernel %in% c("epan", "gauss")) {
    stop("pace: kernel must be epan or gauss, got '", kernel, "'.",
         call. = FALSE)
  }
  if (bw <= 0) stop("pace: the bandwidth must be positive.", call. = FALSE)
  s <- as.numeric(s)
  t <- as.numeric(t)
  z <- as.numeric(z)
  reach <- if (kernel == "epan") 1L else 4L
  s0m <- min(s)
  t0m <- min(t)
  bi <- as.integer((s - s0m) / bw)
  bj <- as.integer((t - t0m) / bw)
  key <- paste(bi, bj, sep = ",")
  buckets <- split(seq_along(z), key)
  out <- matrix(0, length(at_s), length(at_t))
  for (a in seq_along(at_s)) {
    sv <- at_s[a]
    ci <- as.integer((sv - s0m) / bw)
    for (b in seq_along(at_t)) {
      tv <- at_t[b]
      cj <- as.integer((tv - t0m) / bw)
      keys <- paste(rep(seq(ci - reach, ci + reach), each = 2 * reach + 1),
                    rep(seq(cj - reach, cj + reach), times = 2 * reach + 1),
                    sep = ",")
      idx <- unlist(buckets[intersect(keys, names(buckets))],
                    use.names = FALSE)
      if (!length(idx)) {
        stop(sprintf("pace: bandwidth %g leaves (%g, %g) with no data.",
                     bw, sv, tv), call. = FALSE)
      }
      ds <- s[idx] - sv
      dt <- t[idx] - tv
      w <- .pace_kweight(ds / bw, kernel) * .pace_kweight(dt / bw, kernel)
      keep <- w != 0
      if (!any(keep)) {
        stop(sprintf("pace: bandwidth %g leaves (%g, %g) with no data.",
                     bw, sv, tv), call. = FALSE)
      }
      w <- w[keep]
      ds <- ds[keep]
      dt <- dt[keep]
      yv <- z[idx][keep]
      X <- cbind(1, ds, dt)
      Xw <- X * w
      XtX <- crossprod(Xw, X)
      Xty <- as.numeric(crossprod(Xw, yv))
      fit <- tryCatch(.s03ridgesolve(XtX, Xty, 1e-10)[1],
                      error = function(e) sum(w * yv) / sum(w))
      out[a, b] <- fit
    }
  }
  out
}

#' Sparse functional PCA by conditional expectation (PACE)
#'
#' Principal Analysis by Conditional Expectation: the method for
#' functional data observed at few, irregular times per subject, where
#' a curve cannot be estimated one subject at a time. The mean and the
#' covariance surface are smoothed across ALL subjects' observations
#' pooled, the eigenfunctions come from that surface, and each
#' subject's scores are the conditional expectation given their handful
#' of points -- which is what shrinks a sparsely observed subject
#' towards the mean rather than letting noise dominate.
#'
#' The measurement-error variance is the gap between the smoothed
#' diagonal of the raw second moments and the smoothed off-diagonal
#' surface extended to the diagonal: the diagonal carries error, the
#' off-diagonal does not, so the difference identifies it. Pairs with
#' \code{a == b} are excluded from the covariance fit for exactly that
#' reason.
#'
#' @param Y A list of numeric vectors, one per subject: the observed
#'   values.
#' @param argvals A list of numeric vectors, the observation times, one
#'   vector per subject and the same length as that subject's values.
#' @param K Number of components to keep.
#' @param n_grid Size of the shared grid the mean, covariance and
#'   eigenfunctions are evaluated on.
#' @param bw_mu,bw_cov Bandwidths for the mean and covariance
#'   smoothers. \code{NULL} takes a rule of thumb for the mean and one
#'   and a half times it for the covariance, the surface being harder
#'   to estimate.
#' @param kernel \code{"epan"} or \code{"gauss"}.
#' @param shrink If \code{TRUE}, scores are the conditional expectation
#'   (the PACE estimator, which shrinks); if \code{FALSE} they are the
#'   trapezoid integral of the centred curve against the eigenfunction,
#'   which is unbiased but noisy when a subject has few points.
#' @return A list with \code{scores} (one vector per subject),
#'   \code{eigenvalues}, \code{eigenfunctions} (on the grid),
#'   \code{mean}, \code{grid}, \code{sigma2}, \code{fve},
#'   \code{fitted}, \code{n}, \code{K}, \code{n_grid}, \code{bw_mu},
#'   \code{bw_cov}, \code{kernel}, \code{shrink}, \code{n_obs} and
#'   \code{method}.
#' @references
#'   Yao, F., Muller, H.-G. and Wang, J.-L. (2005) "Functional data
#'     analysis for sparse longitudinal data." Journal of the American
#'     Statistical Association 100(470), 577-590.
#'     doi:10.1198/016214504000001745.
#' @export
morie_pace <- function(Y, argvals, K = 2L, n_grid = 21L, bw_mu = NULL,
                       bw_cov = NULL, kernel = "epan", shrink = TRUE) {
  if (!kernel %in% c("epan", "gauss")) {
    stop("pace: kernel must be epan or gauss, got '", kernel, "'.",
         call. = FALSE)
  }
  ys <- lapply(Y, as.numeric)
  ts <- lapply(argvals, as.numeric)
  n <- length(ys)
  if (n == 0L) stop("pace: no subjects.", call. = FALSE)
  if (length(ts) != n) {
    stop("pace: ", n, " subjects but ", length(ts), " time vectors.",
         call. = FALSE)
  }
  for (i in seq_len(n)) {
    if (length(ys[[i]]) != length(ts[[i]])) {
      stop("pace: subject ", i - 1L, " has ", length(ys[[i]]),
           " values and ", length(ts[[i]]), " times.", call. = FALSE)
    }
  }
  pooled_t <- unlist(ts, use.names = FALSE)
  pooled_y <- unlist(ys, use.names = FALSE)
  if (length(pooled_t) < 3L) {
    stop("pace: need at least three observations in total.",
         call. = FALSE)
  }
  K <- as.integer(K)
  if (K < 1L) stop("pace: K must be at least 1.", call. = FALSE)
  lo <- min(pooled_t)
  hi <- max(pooled_t)
  if (hi <= lo) {
    stop("pace: all observation times are identical.", call. = FALSE)
  }
  ng <- as.integer(n_grid)
  if (ng < 3L) stop("pace: n_grid must be at least 3.", call. = FALSE)
  gr <- lo + (hi - lo) * (seq_len(ng) - 1L) / (ng - 1)

  rot <- function(tt) {
    max((max(tt) - min(tt)) * max(length(tt), 2L)^(-0.2) / 2,
        (max(tt) - min(tt)) * 1e-3)
  }
  hmu <- if (!is.null(bw_mu) && bw_mu) as.numeric(bw_mu) else rot(pooled_t)
  hcov <- if (!is.null(bw_cov) && bw_cov) as.numeric(bw_cov) else 1.5 * hmu

  mu_g <- morie_pace_local_linear(pooled_t, pooled_y, gr, hmu, kernel)
  dt <- (hi - lo) / (ng - 1)

  interp <- function(vals, x) {
    p <- (x - lo) / dt
    i0 <- floor(p)
    i0 <- min(max(i0, 0), ng - 2)
    w <- p - i0
    vals[i0 + 1] * (1 - w) + vals[i0 + 2] * w
  }

  cs <- ct <- cz <- numeric(0)
  diag_s <- diag_z <- numeric(0)
  for (i in seq_len(n)) {
    m <- length(ts[[i]])
    cen <- ys[[i]] - vapply(ts[[i]], function(x) interp(mu_g, x), numeric(1))
    diag_s <- c(diag_s, ts[[i]])
    diag_z <- c(diag_z, cen * cen)
    if (m > 1L) {
      a <- rep(seq_len(m), times = m)
      b <- rep(seq_len(m), each = m)
      off <- a != b
      cs <- c(cs, ts[[i]][a[off]])
      ct <- c(ct, ts[[i]][b[off]])
      cz <- c(cz, cen[a[off]] * cen[b[off]])
    }
  }
  if (!length(cz)) {
    stop("pace: no off-diagonal pairs -- every subject has a single ",
         "observation, so the covariance is not identified.",
         call. = FALSE)
  }
  G <- morie_pace_local_linear_2d(cs, ct, cz, gr, gr, hcov, kernel)
  G <- 0.5 * (G + t(G))

  dsm <- morie_pace_local_linear(diag_s, diag_z, gr, hmu, kernel)
  sigma2 <- sum(dsm - diag(G)) / ng
  if (sigma2 < 0) sigma2 <- 0

  eig <- .s03jacobi(G)
  ord <- order(eig$values, decreasing = TRUE)
  kk <- min(K, ng)
  lam <- numeric(0)
  phi <- vector("list", 0L)
  for (idx in ord[seq_len(kk)]) {
    ev <- max(eig$values[idx], 0) * dt
    f <- eig$vectors[, idx]
    nrm <- sqrt(max(sum(f * f) * dt, 1e-300))
    lam <- c(lam, ev)
    phi[[length(phi) + 1L]] <- f / nrm
  }
  total <- sum(pmax(eig$values, 0)) * dt
  fve <- vapply(seq_along(lam), function(j)
    if (total > 0) sum(lam[seq_len(j)]) / total else NA_real_, numeric(1))

  phi_at <- function(j, x) {
    p <- (x - lo) / dt
    i0 <- min(max(floor(p), 0), ng - 2)
    w <- p - i0
    phi[[j]][i0 + 1] * (1 - w) + phi[[j]][i0 + 2] * w
  }

  scores <- vector("list", n)
  fitted <- vector("list", n)
  nk <- length(lam)
  for (i in seq_len(n)) {
    m <- length(ts[[i]])
    cen <- ys[[i]] - vapply(ts[[i]], function(x) interp(mu_g, x), numeric(1))
    P <- matrix(0, m, nk)
    for (a in seq_len(m)) for (j in seq_len(nk)) P[a, j] <- phi_at(j, ts[[i]][a])
    if (isTRUE(shrink)) {
      S <- matrix(0, m, m)
      for (a in seq_len(m)) for (b in seq_len(m)) {
        S[a, b] <- sum(lam * P[a, ] * P[b, ]) + if (a == b) sigma2 else 0
      }
      z <- tryCatch(as.numeric(.s03ridgesolve(S, cen, 1e-10)),
                    error = function(e) rep(0, m))
      xi <- vapply(seq_len(nk), function(j) lam[j] * sum(P[, j] * z),
                   numeric(1))
    } else {
      xi <- vapply(seq_len(nk), function(j) {
        if (m < 2L) return(0)
        h <- diff(ts[[i]])
        sum(0.5 * h * (cen[-m] * P[-m, j] + cen[-1] * P[-1, j]))
      }, numeric(1))
    }
    scores[[i]] <- xi
    fitted[[i]] <- vapply(seq_len(ng), function(g)
      mu_g[g] + sum(vapply(seq_len(nk), function(j) xi[j] * phi[[j]][g],
                           numeric(1))), numeric(1))
  }

  list(
    estimate = scores, scores = scores,
    eigenvalues = lam, eigenfunctions = phi,
    mean = mu_g, grid = gr, sigma2 = sigma2,
    fve = fve, fitted = fitted,
    n = n, K = nk, n_grid = ng,
    bw_mu = hmu, bw_cov = hcov, kernel = kernel,
    shrink = shrink,
    n_obs = length(pooled_t),
    method = paste("PACE sparse FPCA with conditional-expectation",
                   "scores (Yao, M\u00fcller & Wang 2005)")
  )
}
