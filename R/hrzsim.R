# SPDX-License-Identifier: AGPL-3.0-or-later

#' Semiparametric WNLS estimator of a single-index model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.5.1, equations (2.22) to (2.25) and Theorem
#' 2.2, equation (2.26) (pages 20-21); the estimator is Ichimura's
#' (1993).  G is replaced by the leave-one-out, trimmed, weighted
#' kernel estimator (2.23)-(2.24) and betatilde solves (2.25), the
#' minimisation being over betatilde only so that beta_1 = 1.
#'
#' Minimisation uses the shelf's fixed-schedule coordinate search: a
#' set number of sweeps, a fixed step ladder, no tolerance-based early
#' exit and no random restart.
#'
#' @param x Numeric matrix of covariates, n by d, with no constant column.
#' @param y Numeric outcome vector.
#' @param h Numeric bandwidth for (2.23)-(2.24); default n^(-1/5).
#' @param weights Optional numeric W(X_i) in (2.22); default ones.
#' @param trim Numeric trimming constant eta defining A_x.
#' @param niter Integer sweeps of the coordinate search.
#' @param delta Numeric initial step of the coordinate search.
#' @param b0 Optional numeric starting value for betatilde.
#' @return Named list with estimate, se, objective, ghat, index,
#'   bandwidth, ntrim, n, method.
#' @keywords internal
#' @examples
#' n <- 150
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.9))
#' z <- as.numeric(x %*% c(1, 0.8))
#' Sindex(x, z / (1 + abs(z)), h = 0.35)$estimate
#' @export
Sindex <- function(x, y, h = NULL, weights = NULL, trim = 0.01, niter = 12L,
                   delta = 1, b0 = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("a single-index model needs at least two covariates.", call. = FALSE)
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  W <- if (is.null(weights)) rep(1, n) else as.numeric(weights)

  crit <- function(bt, want = FALSE) {
    b <- c(1, as.numeric(bt))
    z <- as.numeric(X %*% b)
    K <- .hrz2_gk(outer(z, z, "-") / hh) * rep(W, each = n)
    diag(K) <- 0
    den <- rowSums(K) / (n * hh)
    num <- as.numeric(K %*% yv) / (n * hh)
    safe <- ifelse(den > 1e-300, den, 1e-300)
    gh <- num / safe
    keep <- den > trim * mean(den)
    r <- yv - gh
    val <- sum(ifelse(keep, W * r * r, 0)) / n
    if (want) list(val = val, gh = gh, z = z, keep = keep, r = r) else val
  }

  start <- if (is.null(b0)) {
    ols <- as.numeric(qr.solve(X, yv))
    if (abs(ols[1L]) > 1e-12) ols[-1L] / ols[1L] else rep(0, d - 1L)
  } else as.numeric(b0)

  cm <- .hrz_coord_min(crit, start, niter = as.integer(niter),
                       delta = as.numeric(delta))
  bt <- cm$par
  cur <- crit(bt, want = TRUE)
  beta <- c(1, bt)

  eps <- 1e-5
  Jm <- matrix(0, n, d - 1L)
  for (j in seq_len(d - 1L)) {
    bp <- bt
    bp[j] <- bp[j] + eps
    bm <- bt
    bm[j] <- bm[j] - eps
    Jm[, j] <- -(crit(bp, want = TRUE)$gh - crit(bm, want = TRUE)$gh) / (2 * eps)
  }
  kf <- as.numeric(cur$keep)
  A <- crossprod(Jm * (kf * W), Jm) / n
  s2 <- sum(kf * W * cur$r * cur$r) / max(sum(kf), 1)
  se <- tryCatch({
    cov <- s2 * solve(A + diag(1e-12, d - 1L)) / n
    sqrt(pmax(diag(cov), 0))
  }, error = function(e) rep(NA_real_, d - 1L))
  list(estimate = beta, se = c(0, se), objective = cur$val, ghat = cur$gh,
       index = cur$z, bandwidth = hh, ntrim = as.integer(n - sum(kf)), n = n,
       method = "Horowitz (2009) eq. (2.25), Ichimura semiparametric WNLS")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Sindex
#' @keywords internal
#' @export
morie_horowitz_single_index_model <- Sindex
