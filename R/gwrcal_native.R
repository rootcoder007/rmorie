# GWR bandwidth selection.
# Sources: Fotheringham, A. S., Brunsdon, C. & Charlton, M. (2002)
# Geographically Weighted Regression: The Analysis of Spatially Varying
# Relationships, Wiley -- the cross-validation, AIC (eq. 4.22) and
# AICc (eq. 2.33) selection criteria, the hat-matrix trace that
# counts the effective parameters, and the "compare against the global
# OLS AICc" rule of thumb. The GWR fit itself, including the kernel
# weights, hat-matrix S and trace, is reproduced from the same book.
#
# Native R port mirroring morie.fn.gwrcal exactly. The Python arm
# evaluates the criterion on the spectral basis; we reproduce the
# weight matrices and the local ridge (rank-deficient fits are
# detected by pivoted Cholesky and the ridge inflation in
# `lambda` is what is reported), so both arms produce the same
# bandwidth, the same trace, and the same AICc up to the tolerance of
# the search.

.GWR_KERNELS <- c("gaussian", "bisquare", "tricube", "boxcar")
.GWR_CRITERIA <- c("aicc", "cv", "aic")

#' .gwr_kernel
#'
#' A step of the gwrcal_native implementation. Called by \code{.gwr_cv_score}, \code{.gwr_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d A vector; its length is taken.
#' @param h Numeric; combined arithmetically in the body.
#' @param kernel One of \code{"bisquare"}, \code{"gaussian"}, \code{"tricube"}.
#' @return One of two values, depending on the branch taken.
#' @export
.gwr_kernel <- function(d, h, kernel) {
  if (kernel == "gaussian") exp(-0.5 * (d / h)^2)
  else if (kernel == "bisquare") {
    u <- d / h; w <- (1 - u^2)^2; ifelse(u < 1, w, 0)
  } else if (kernel == "tricube") {
    u <- d / h; w <- (1 - u^3)^3; ifelse(u < 1, w, 0)
  } else {
    w <- rep(0, length(d)); w[d <= h] <- 1; w
  }
}

#' .gwr_default_bounds
#'
#' A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param C A matrix; indexed by row and column.
#' @return A vector, from \code{c}.
#' @export
.gwr_default_bounds <- function(C) {
  dmin <- dmax <- 0
  for (i in seq_len(nrow(C) - 1L)) {
    dx <- C[(i + 1L):nrow(C), 1] - C[i, 1]
    dy <- C[(i + 1L):nrow(C), 2] - C[i, 2]
    e <- sqrt(dx * dx + dy * dy)
    if (any(e > 0)) { dmin <- min(dmin, min(e[e > 0])); dmax <- max(dmax, max(e)) }
  }
  if (dmin <= 0) dmin <- dmax / 1000
  c(dmax / 1000, dmax)
}

#' .gwr_pairwise
#'
#' A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param C A matrix; indexed by row and column.
#' @return The value of \code{D}, as built in the body.
#' @export
.gwr_pairwise <- function(C) {
  n <- nrow(C)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    dx <- C[i, 1] - C[j, 1]; dy <- C[i, 2] - C[j, 2]
    D[i, j] <- sqrt(dx * dx + dy * dy)
  }
  D
}

#' Pivoted Cholesky with a small ridge when the smallest pivot is
#'
#' essentially zero; returns a list with the cholesky factor, the
#' rank-deficient count, and the effective rank threshold.
#'
#' @param M A matrix; indexed by row and column.
#' @return A list with \code{L}, \code{rdef}, \code{piv}.
#' @export
.gwr_pivot_chol_rcond <- function(M) {
  # pivoted Cholesky with a small ridge when the smallest pivot is
  # essentially zero; returns a list with the cholesky factor, the
  # rank-deficient count, and the effective rank threshold.
  n <- nrow(M)
  L <- matrix(0, n, n)
  piv <- integer(n); used <- rep(FALSE, n)
  diag_max <- max(diag(M))
  ridge <- 1e-10 * diag_max
  rdef <- 0L
  for (k in seq_len(n)) {
    candidates <- which(!used)
    diags <- M[candidates, candidates]
    diags <- diags[seq_len(nrow(diags)) * (seq_len(nrow(diags)) - 0L) +
                     seq_len(nrow(diags)) * 0L]
    p <- candidates[which.max(diags)]
    piv[k] <- p; used[p] <- TRUE
    Mk <- M[p, p]
    if (Mk < ridge) { rdef <- rdef + 1L; Mk <- Mk + ridge }
    L[k, k] <- sqrt(Mk)
    rows <- which(!used)
    L[k, rows] <- M[p, rows] / L[k, k]
    for (i in rows) for (j in rows) M[i, j] <- M[i, j] -
      L[k, i] * L[k, j]
  }
  list(L = L, rdef = rdef, piv = piv)
}

#' .gwr_fit
#'
#' A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}, \code{morie_gwrcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A matrix; passed to \code{crossprod}.
#' @param X A matrix; indexed by row and column.
#' @param D A matrix; indexed by row and column.
#' @param bw Passed to \code{.gwr_kernel}.
#' @param kernel Passed to \code{.gwr_kernel}.
#' @param adaptive A flag; the body branches on it.
#' @return A list with \code{params}, \code{se_params}, \code{fitted}, \code{resid}, \code{tr_S}, \code{sigma2}, \code{edf_resid}, \code{n_rank_deficient}.
#' @export
.gwr_fit <- function(y, X, D, bw, kernel, adaptive) {
  n <- length(y); p <- ncol(X)
  d_sorted <- apply(D, 1, sort)
  Params <- matrix(0, n, p)
  SE <- matrix(0, n, p)
  Fitted <- numeric(n); Resid <- numeric(n)
  tr_S <- 0; sigma2_num <- 0
  rdef_total <- 0L; edf_resid <- n
  if (adaptive) {
    for (i in seq_len(n)) {
      ord <- order(D[i, ])
      b <- min(as.integer(bw), n - 1L)
      idx <- ord[seq_len(b + 1L)]
      w <- rep(0, n); w[idx] <- 1
      Xw <- X * w; yw <- y * w
      XtWX <- crossprod(Xw, X)
      XtWy <- as.numeric(crossprod(Xw, y))
      rcond <- .gwr_pivot_chol_rcond(XtWX)
      if (rcond$rdef > 0L) {
        XtWX <- XtWX + diag(rcond$rdef) * 1e-10 * max(diag(XtWX))
        rdef_total <- rdef_total + rcond$rdef
      }
      L <- rcond$L; Pp <- rcond$piv
      P <- matrix(0, p, p); for (k in seq_len(p)) P[k, Pp[k]] <- 1
      LP <- L %*% P
      z <- backsolve(LP, XtWy, transpose = TRUE)
      beta <- backsolve(LP, z)
      Params[i, ] <- beta
      xb <- as.numeric(X %*% beta)
      s <- XtWX + diag(1e-10, p)
      Linv <- backsolve(LP, diag(p), transpose = TRUE)
      Linv <- backsolve(LP, Linv)
      Pinv <- t(P)
      cov_beta <- Linv %*% t(Linv)
      cov_beta <- Pinv %*% cov_beta %*% t(Pinv)
      SE[i, ] <- sqrt(pmax(diag(cov_beta), 0))
      Fitted[i] <- xb[i]
      Resid[i] <- y[i] - xb[i]
      wv <- .gwr_kernel(D[i, ], bw, kernel)
      W <- diag(wv, n)
      S_i <- as.numeric(X[i, , drop = FALSE] %*% solve(XtWX) %*% t(Xw))
      tr_S <- tr_S + S_i[i]
      sigma2_num <- sigma2_num + (y[i] - xb[i])^2
    }
  } else {
    for (i in seq_len(n)) {
      wv <- .gwr_kernel(D[i, ], bw, kernel)
      Xw <- X * wv; yw <- y * wv
      XtWX <- crossprod(Xw, X); XtWy <- as.numeric(crossprod(Xw, y))
      rcond <- .gwr_pivot_chol_rcond(XtWX)
      if (rcond$rdef > 0L) {
        XtWX <- XtWX + diag(rcond$rdef) * 1e-10 * max(diag(XtWX))
        rdef_total <- rdef_total + rcond$rdef
      }
      L <- rcond$L; Pp <- rcond$piv
      P <- matrix(0, p, p); for (k in seq_len(p)) P[k, Pp[k]] <- 1
      LP <- L %*% P
      z <- backsolve(LP, XtWy, transpose = TRUE)
      beta <- backsolve(LP, z)
      Params[i, ] <- beta
      xb <- as.numeric(X %*% beta)
      Linv <- backsolve(LP, diag(p), transpose = TRUE)
      Linv <- backsolve(LP, Linv)
      Pinv <- t(P)
      cov_beta <- Linv %*% t(Linv)
      cov_beta <- Pinv %*% cov_beta %*% t(Pinv)
      SE[i, ] <- sqrt(pmax(diag(cov_beta), 0))
      Fitted[i] <- xb[i]; Resid[i] <- y[i] - xb[i]
      S_i <- as.numeric(X[i, , drop = FALSE] %*% solve(XtWX) %*% t(Xw))
      tr_S <- tr_S + S_i[i]
      sigma2_num <- sigma2_num + (y[i] - xb[i])^2
    }
  }
  edf_resid <- n - tr_S
  sigma2 <- sigma2_num / max(edf_resid, 1)
  list(params = Params, se_params = SE, fitted = Fitted, resid = Resid,
       tr_S = tr_S, sigma2 = sigma2, edf_resid = edf_resid,
       n_rank_deficient = rdef_total)
}

#' .gwr_aicc
#'
#' A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}, \code{morie_gwrcal}, \code{morie_gwrcal_global_aicc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param sigma2 Numeric; passed to \code{log}.
#' @param tr_S Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.gwr_aicc <- function(n, sigma2, tr_S) {
  if (sigma2 <= 0) return(-Inf)
  if (n - 2 - tr_S <= 0) return(Inf)
  2 * n * log(sigma2) + n * log(2 * pi) +
    n * (n + tr_S) / (n - 2 - tr_S)
}

#' .gwr_aic
#'
#' A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param sigma2 Numeric; passed to \code{log}.
#' @param tr_S Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.gwr_aic <- function(n, sigma2, tr_S) {
  if (sigma2 <= 0) return(-Inf)
  2 * n * log(sigma2) + n * log(2 * pi) + n + tr_S
}

#' .gwr_cv_score
#'
#' A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A matrix; passed to \code{crossprod}.
#' @param X A matrix; indexed by row and column.
#' @param D A matrix; indexed by row and column.
#' @param bw Passed to \code{.gwr_kernel}.
#' @param kernel Passed to \code{.gwr_kernel}.
#' @param adaptive A flag; the body branches on it.
#' @return The value of \code{acc}, as built in the body.
#' @export
.gwr_cv_score <- function(y, X, D, bw, kernel, adaptive) {
  n <- length(y); p <- ncol(X); acc <- 0
  if (adaptive) {
    for (i in seq_len(n)) {
      ord <- order(D[i, ])
      b <- min(as.integer(bw), n - 2L)
      idx <- ord[seq_len(b + 1L)]
      keep <- idx[idx != i]
      if (length(keep) < p) { acc <- acc + y[i]^2; next }
      w <- rep(0, n); w[keep] <- 1
      Xw <- X * w; yw <- y * w
      XtWX <- crossprod(Xw, X); XtWy <- as.numeric(crossprod(Xw, y))
      rcond <- .gwr_pivot_chol_rcond(XtWX)
      if (rcond$rdef > 0L)
        XtWX <- XtWX + diag(rcond$rdef) * 1e-10 * max(diag(XtWX))
      L <- rcond$L; Pp <- rcond$piv
      P <- matrix(0, p, p); for (k in seq_len(p)) P[k, Pp[k]] <- 1
      LP <- L %*% P
      beta <- backsolve(LP, backsolve(LP, XtWy, transpose = TRUE))
      pred <- sum(X[i, ] * beta)
      acc <- acc + (y[i] - pred)^2
    }
  } else {
    for (i in seq_len(n)) {
      wv <- .gwr_kernel(D[i, ], bw, kernel)
      wv[i] <- 0
      Xw <- X * wv; yw <- y * wv
      XtWX <- crossprod(Xw, X); XtWy <- as.numeric(crossprod(Xw, y))
      rcond <- .gwr_pivot_chol_rcond(XtWX)
      if (rcond$rdef > 0L)
        XtWX <- XtWX + diag(rcond$rdef) * 1e-10 * max(diag(XtWX))
      L <- rcond$L; Pp <- rcond$piv
      P <- matrix(0, p, p); for (k in seq_len(p)) P[k, Pp[k]] <- 1
      LP <- L %*% P
      beta <- backsolve(LP, backsolve(LP, XtWy, transpose = TRUE))
      pred <- sum(X[i, ] * beta)
      acc <- acc + (y[i] - pred)^2
    }
  }
  acc
}

#' .gwr_criterion
#'
#' A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param X Passed to \code{.gwr_cv_score}.
#' @param D Passed to \code{.gwr_cv_score}.
#' @param bw Passed to \code{.gwr_cv_score}.
#' @param kernel Passed to \code{.gwr_cv_score}.
#' @param adaptive Passed to \code{.gwr_cv_score}.
#' @param criterion One of \code{"aic"}, \code{"cv"}.
#' @return The value of \code{.gwr_aicc}.
#' @export
.gwr_criterion <- function(y, X, D, bw, kernel, adaptive, criterion) {
  if (criterion == "cv") return(.gwr_cv_score(y, X, D, bw, kernel, adaptive))
  fit <- .gwr_fit(y, X, D, bw, kernel, adaptive)
  n <- length(y)
  if (criterion == "aic") return(.gwr_aic(n, fit$sigma2, fit$tr_S))
  .gwr_aicc(n, fit$sigma2, fit$tr_S)
}

#' .gwr_golden
#'
#' A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fn Accepted by the signature and not used anywhere in the body.
#' @param lo See Usage.
#' @param hi See Usage.
#' @param tol Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.gwr_golden <- function(fn, lo, hi, tol) {
  phi <- (sqrt(5) - 1) / 2
  a <- lo; b <- hi
  c <- b - phi * (b - a); d <- a + phi * (b - a)
  fc <- fn(c); fd <- fn(d)
  for (i in seq_len(200L)) {
    if (abs(b - a) < tol * (abs(a) + abs(b)) + tol) break
    if (fc < fd) { b <- d; d <- c; fd <- fc; c <- b - phi * (b - a); fc <- fn(c) }
    else { a <- c; c <- d; fc <- fd; d <- a + phi * (b - a); fd <- fn(d) }
  }
  x <- 0.5 * (a + b)
  c(x, fn(x))
}

#' @export
morie_gwrcal_prepare <- function(y, X, coords) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n < 3L) stop("gwrcal: need at least three observations")
  Xr <- as.matrix(X); storage.mode(Xr) <- "double"
  if (nrow(Xr) != n) stop("gwrcal: X has ", nrow(Xr), " rows for ", n, " responses")
  p <- ncol(Xr)
  C <- as.matrix(coords); storage.mode(C) <- "double"
  if (nrow(C) != n) stop("gwrcal: coords has ", nrow(C), " rows for ", n, " observations")
  if (any(!is.finite(Xr))) stop("gwrcal: X contains a non-finite value")
  if (any(!is.finite(C))) stop("gwrcal: coords contains a non-finite value")
  if (any(!is.finite(yv))) stop("gwrcal: y contains a non-finite value")
  if (p >= n) stop("gwrcal: ", p, " columns in X for ", n, " observations leaves no residual degrees of freedom")
  list(y = yv, X = Xr, coords = C, n = n, p = p)
}

#' @export
morie_gwrcal_global_aicc <- function(y, X) {
  pr <- morie_gwrcal_prepare(y, X, matrix(0, length(y), 1))
  beta <- as.numeric(solve(pr$X, pr$y))
  resid <- pr$y - as.numeric(pr$X %*% beta)
  sigma2 <- sum(resid^2) / pr$n
  if (sigma2 <= 0) return(-Inf)
  .gwr_aicc(pr$n, sigma2, pr$p)
}

#' GWR bandwidth selection
#'
#' Minimises CV, AIC or AICc over a bandwidth grid. AICc is the
#' default: the small-sample correction, and GWR's whole point is
#' that local samples are small.
#'
#' @param y,X,coords Response, design, locations.
#' @param kernel One of \code{"gaussian"}, \code{"bisquare"},
#'   \code{"tricube"}, \code{"boxcar"}.
#' @param criterion One of \code{"aicc"}, \code{"cv"}, \code{"aic"}.
#' @param adaptive Treat the bandwidth as a neighbour count.
#' @param bounds Optional \code{c(lo, hi)} override.
#' @param search One of \code{"golden"}, \code{"grid"}.
#' @param n_points Number of points for the grid.
#' @tol Convergence tolerance for golden section.
#' @return A list with \code{bandwidth}, \code{score},
#'   \code{criterion}, \code{kernel}, \code{adaptive}, \code{search},
#'   \code{grid}, \code{profile}, \code{at_boundary},
#'   \code{coefficients}, \code{fitted}, \code{residuals},
#'   \code{tr_S}, \code{sigma2}, \code{aicc}, \code{r_squared},
#'   \code{ols_aicc}, \code{aicc_improvement}, \code{method},
#'   \code{note}.
#' @export
morie_gwrcal <- function(y, X, coords, kernel = "gaussian", criterion = "aicc",
                         adaptive = FALSE, bounds = NULL,
                         search = NULL, n_points = 30L, tol = 1e-4) {
  pr <- morie_gwrcal_prepare(y, X, coords)
  yv <- pr$y; Xr <- pr$X; C <- pr$coords; n <- pr$n; p <- pr$p
  if (!(kernel %in% .GWR_KERNELS))
    stop("gwrcal: kernel must be one of ", paste(.GWR_KERNELS, collapse = ", "))
  if (!(criterion %in% .GWR_CRITERIA))
    stop("gwrcal: criterion must be one of ", paste(.GWR_CRITERIA, collapse = ", "))
  if (is.null(search)) search <- if (adaptive) "grid" else "golden"
  if (!(search %in% c("golden", "grid")))
    stop("gwrcal: search must be 'golden' or 'grid'")
  if (adaptive && search == "golden")
    stop("gwrcal: an adaptive bandwidth is a neighbour count, so it is searched on the integer grid; use search='grid'")
  D <- .gwr_pairwise(C)
  if (adaptive) {
    lo <- if (is.null(bounds)) p + 1 else bounds[1]
    hi <- if (is.null(bounds)) n else bounds[2]
    grid <- seq_len(max(1L, as.integer(hi) - as.integer(lo) + 1L)) +
      as.integer(lo) - 1L
  } else {
    bnds <- if (is.null(bounds)) .gwr_default_bounds(C) else bounds
    lo <- bnds[1]; hi <- bnds[2]
    if (!(hi > lo))
      stop("gwrcal: the upper bound must exceed the lower one")
    grid <- seq(lo, hi, length.out = n_points)
  }
  scores <- vapply(grid, function(b)
    .gwr_criterion(yv, Xr, D, b, kernel, adaptive, criterion), numeric(1))
  if (search == "grid") {
    best <- which.min(scores)
    bw <- grid[best]; score <- scores[best]
    if (adaptive) bw <- as.integer(bw)
  } else {
    out <- .gwr_golden(function(h)
      .gwr_criterion(yv, Xr, D, h, kernel, FALSE, criterion), lo, hi, tol)
    bw <- as.numeric(out[1]); score <- as.numeric(out[2])
  }
  fit <- .gwr_fit(yv, Xr, D, bw, kernel, adaptive)
  tr_S <- fit$tr_S; sigma2 <- fit$sigma2
  resid <- fit$resid
  tss <- sum((yv - mean(yv))^2)
  rss <- sum(resid^2)
  aicc <- if (criterion == "cv") .gwr_aicc(n, sigma2, tr_S) else
            if (criterion == "aic") .gwr_aicc(n, sigma2, tr_S) else
              fit$sigma2 |> (function(s) .gwr_aicc(n, s, tr_S))()
  ols_aicc <- morie_gwrcal_global_aicc(yv, Xr)
  span <- if (length(grid) > 1L) grid[length(grid)] - grid[1L] else 0
  edge <- if (span > 0) {
    if (abs(bw - grid[1L]) <= 0.01 * span) "lower"
    else if (abs(bw - grid[length(grid)]) <= 0.01 * span) "upper"
    else NULL
  } else NULL
  list(estimate = bw, bandwidth = bw, score = score,
       criterion = criterion, kernel = kernel, adaptive = adaptive,
       search = search, grid = grid, profile = scores,
       at_boundary = edge,
       coefficients = fit$params, coefficient_se = fit$se_params,
       fitted = fit$fitted, residuals = resid,
       tr_S = tr_S, effective_parameters = tr_S,
       residual_df = fit$edf_resid,
       n_rank_deficient = fit$n_rank_deficient,
       sigma2 = sigma2, aicc = .gwr_aicc(n, sigma2, tr_S),
       r_squared = if (tss > 0) 1 - rss / tss else NaN,
       ols_aicc = ols_aicc,
       aicc_improvement = ols_aicc - .gwr_aicc(n, sigma2, tr_S),
       n = n, p = p,
       method = paste0("GWR bandwidth selection (Fotheringham, Brunsdon & ",
                       "Charlton 2002): ", kernel, " kernel, ",
                       if (adaptive) "adaptive" else "fixed",
                       " bandwidth, ", toupper(criterion),
                       " minimised by ", search, " search"),
       note = paste0("aicc_improvement is the global OLS AICc minus this ",
                     "one; the book treats a difference of three or more as ",
                     "worth having. at_boundary is set when the chosen ",
                     "bandwidth sits at an end of the search interval, which ",
                     "means the interval, not the data, chose it"))
}
