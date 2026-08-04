# SPDX-License-Identifier: AGPL-3.0-or-later

#' Additive model with a known nonidentity link function
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.2, equation (3.19) (pages 70-71):
#' E(Y|X=x) = G[mu + m_1(x^1) + ... + m_d(x^d)] with G KNOWN.  The
#' book's recipe is a series approximation fitted by nonlinear least
#' squares -- which is what imposes additivity and avoids the curse of
#' dimensionality -- followed by ONE Newton step toward a local-linear
#' or local-constant estimate, after which each component behaves like
#' a one-dimensional smoother with the others known, hence oracle
#' efficient and asymptotically normal.
#'
#' The first stage uses a FIXED number of Gauss-Newton iterations with
#' no tolerance-based early exit; the second is the single Newton step.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param link One of "identity", "logistic", "exp", or a list of two
#'   functions (G, Gprime).
#' @param K Integer series length per coordinate (polynomial degree).
#' @param h Numeric bandwidth of the Newton step; default n^(-1/5).
#' @param niter Integer FIXED Gauss-Newton iterations.
#' @param ngrid Integer points per component grid.
#' @return Named list with mu, grids, components, fitted, eta, resid,
#'   rss, bandwidth, K, d, n, method.
#' @keywords internal
#' @examples
#' n <- 150
#' x1 <- seq(-2, 2, length.out = n)
#' x2 <- cos(seq_len(n) * 0.9)
#' y <- 1 / (1 + exp(-(0.5 * x1 + 0.8 * x2)))
#' Addlink(cbind(x1, x2), y, link = "logistic", h = 0.4)$rss
#' @export
Addlink <- function(x, y, link = "logistic", K = 4L, h = NULL, niter = 20L,
                    ngrid = 25L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("an additive model needs at least two covariates.", call. = FALSE)
  }
  lg <- function(v) 1 / (1 + exp(-pmin(pmax(v, -500), 500)))
  links <- list(
    identity = list(function(v) v, function(v) rep(1, length(v))),
    logistic = list(lg, function(v) lg(v) * (1 - lg(v))),
    exp = list(function(v) exp(pmin(pmax(v, -500), 500)),
               function(v) exp(pmin(pmax(v, -500), 500)))
  )
  if (is.character(link)) {
    if (!(link %in% names(links))) {
      stop(sprintf("link must be one of %s or a (G, Gprime) pair, got %s.",
                   paste(sort(names(links)), collapse = ", "), link),
           call. = FALSE)
    }
    G <- links[[link]][[1L]]
    Gp <- links[[link]][[2L]]
  } else {
    G <- link[[1L]]
    Gp <- link[[2L]]
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  Ki <- as.integer(K)

  cols <- list(rep(1, n))
  for (j in seq_len(d)) {
    scv <- X[, j]
    rng <- max(scv) - min(scv)
    scv <- (scv - min(scv)) / (if (rng > 0) rng else 1) * 2 - 1
    for (k in seq_len(Ki)) cols[[length(cols) + 1L]] <- scv^k
  }
  P <- do.call(cbind, cols)
  theta <- rep(0, ncol(P))
  theta[1L] <- mean(yv)
  for (it in seq_len(as.integer(niter))) {
    eta <- as.numeric(P %*% theta)
    w <- Gp(eta)
    r <- yv - G(eta)
    A <- crossprod(P, P * (w * w)) + diag(1e-8, ncol(P))
    b <- crossprod(P, w * r)
    theta <- theta + as.numeric(solve(A, b))
  }
  eta <- as.numeric(P %*% theta)

  w <- Gp(eta)
  work <- eta + ifelse(abs(w) > 1e-12, (yv - G(eta)) / ifelse(abs(w) > 1e-12, w, 1), 0)
  gs <- vector("list", d)
  comps <- vector("list", d)
  for (j in seq_len(d)) {
    g <- seq(min(X[, j]), max(X[, j]), length.out = as.integer(ngrid))
    gs[[j]] <- g
    Kj <- .hrz2_gk(outer(g, X[, j], "-") / hh)
    den <- rowSums(Kj)
    den <- ifelse(den > 1e-300, den, 1e-300)
    mj <- as.numeric(Kj %*% work) / den
    comps[[j]] <- mj - mean(mj)
  }

  mu <- mean(work)
  fit_eta <- rep(mu, n)
  for (j in seq_len(d)) {
    fit_eta <- fit_eta + stats::approx(gs[[j]], comps[[j]], X[, j], rule = 2)$y
  }
  fitted <- G(fit_eta)
  r <- yv - fitted
  list(mu = mu, grids = gs, components = comps, fitted = fitted,
       eta = fit_eta, resid = r, rss = sum(r * r), bandwidth = hh,
       K = Ki, d = as.integer(d), n = n,
       method = "Horowitz (2009) eq. (3.19), series then one Newton step")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Addlink
#' @keywords internal
#' @export
morie_horowitz_additive_nonid_link <- Addlink
