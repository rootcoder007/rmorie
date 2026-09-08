# SPDX-License-Identifier: AGPL-3.0-or-later

#' Klein-Spady semiparametric MLE for a binary-response index model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.5.3, equation (2.33) and the semiparametric
#' analog on page 28; the estimator is Klein and Spady's (1993).  With
#' Y in \{0,1\}, G(x'beta) = P(Y = 1 | X = x); the known-G MLE maximises
#' (2.33), and in the semiparametric case G is replaced by a
#' leave-one-out kernel estimator.  Because Var(Y|X=x) depends on x
#' only through the index, the weight cancels and the UNWEIGHTED
#' estimator of G already attains the efficiency bound (page 28).
#'
#' Maximisation uses the shelf's fixed-schedule coordinate search: no
#' tolerance-based early exit, no random restarts.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric binary 0/1 outcome vector.
#' @param h Numeric bandwidth; default n^(-1/5).
#' @param trim Numeric density trimming constant defining A_x.
#' @param floor Numeric; Ghat is clipped into \[floor, 1-floor\].
#' @param niter Integer sweeps of the coordinate search.
#' @param delta Numeric initial step of the coordinate search.
#' @param b0 Optional numeric starting value for betatilde.
#' @return Named list with estimate, se, loglik, ghat, index,
#'   bandwidth, ntrim, n, method.
#' @keywords internal
#' @examples
#' n <- 200
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.8))
#' z <- as.numeric(x %*% c(1, 0.6))
#' Spmlebin(x, as.numeric(z >= 0), h = 0.4)$estimate
#' @export
Spmlebin <- function(x, y, h = NULL, trim = 0.01, floor = 1e-4, niter = 12L,
                     delta = 1, b0 = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("a single-index model needs at least two covariates.", call. = FALSE)
  }
  if (any(!(yv %in% c(0, 1)))) {
    stop("y must be binary 0/1 for a binary-response model.", call. = FALSE)
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  fl <- as.numeric(floor)

  negll <- function(bt, want = FALSE) {
    b <- c(1, as.numeric(bt))
    z <- as.numeric(X %*% b)
    K <- .hrz2_gk(outer(z, z, "-") / hh)
    diag(K) <- 0
    den <- rowSums(K) / (n * hh)
    num <- as.numeric(K %*% yv) / (n * hh)
    safe <- ifelse(den > 1e-300, den, 1e-300)
    gh <- pmin(pmax(num / safe, fl), 1 - fl)
    keep <- den > trim * mean(den)
    ll <- ifelse(keep, yv * log(gh) + (1 - yv) * log(1 - gh), 0)
    val <- -sum(ll) / n
    if (want) list(val = val, gh = gh, z = z, keep = keep) else val
  }

  start <- if (is.null(b0)) {
    ols <- as.numeric(qr.solve(X, yv))
    if (abs(ols[1L]) > 1e-12) ols[-1L] / ols[1L] else rep(0, d - 1L)
  } else as.numeric(b0)

  cm <- .hrz_coord_min(negll, start, niter = as.integer(niter),
                       delta = as.numeric(delta))
  bt <- cm$par
  cur <- negll(bt, want = TRUE)
  beta <- c(1, bt)

  eps <- 1e-5
  S <- matrix(0, n, d - 1L)
  kf <- as.numeric(cur$keep)
  gh <- cur$gh
  for (j in seq_len(d - 1L)) {
    bp <- bt
    bp[j] <- bp[j] + eps
    bm <- bt
    bm[j] <- bm[j] - eps
    dg <- (negll(bp, want = TRUE)$gh - negll(bm, want = TRUE)$gh) / (2 * eps)
    S[, j] <- kf * dg * (yv / gh - (1 - yv) / (1 - gh))
  }
  I <- crossprod(S) / n
  se <- tryCatch({
    cov <- solve(I + diag(1e-12, d - 1L)) / n
    sqrt(pmax(diag(cov), 0))
  }, error = function(e) rep(NA_real_, d - 1L))
  list(estimate = beta, se = c(0, se), loglik = -cur$val, ghat = gh,
       index = cur$z, bandwidth = hh, ntrim = as.integer(n - sum(kf)), n = n,
       method = "Horowitz (2009) eq. (2.33) and page 28, Klein-Spady")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Spmlebin
#' @keywords internal
#' @export
morie_horowitz_semipar_mle_binary <- Spmlebin
