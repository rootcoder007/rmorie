.gwl_design <- function(X, n) {
  X <- unname(as.matrix(X)) * 1
  if (nrow(X) != n) X <- matrix(X, n)
  const <- any(apply(X, 2, function(v) length(unique(v)) == 1 && v[1] != 0))
  if (const) X else cbind(1, X)
}

.gwl_setup <- function(y, X, coords, bw, kernel, adaptive) {
  y <- as.numeric(y)
  n <- length(y)
  Xm <- .gwl_design(X, n)
  P <- unname(as.matrix(coords)) * 1
  D <- as.matrix(stats::dist(P))
  Wc <- t(vapply(seq_len(n), function(i) GWRKernelWeights(D[i, ], bw, kernel, adaptive), numeric(n)))
  list(y = y, X = Xm, P = P, D = unname(D), Wc = Wc, n = n)
}

.gwl_wls <- function(X, w, y) {
  xtw <- t(X * w)
  C <- solve(xtw %*% X, xtw)
  list(beta = as.vector(C %*% y), C = C)
}

.gwl_fit <- function(y, X, coords, bw, kernel, adaptive) {
  s <- .gwl_setup(y, X, coords, bw, kernel, adaptive)
  GWRBasic(s$y, s$X, s$P, bw, kernel = kernel, adaptive = adaptive)
}

.gwl_betavar <- function(y, X, P, bw, kernel, adaptive) {
  D <- unname(as.matrix(stats::dist(P)))
  B <- t(vapply(seq_along(y), function(i) .gwl_wls(X, GWRKernelWeights(D[i, ], bw, kernel, adaptive), y)$beta,
                numeric(ncol(X))))
  apply(matrix(B, length(y)), 2, stats::var)
}

.gwl_ggwr <- function(y, X, coords, bw, kernel, adaptive, tol, maxiter, family) {
  s <- .gwl_setup(y, X, coords, bw, kernel, adaptive)
  y <- s$y
  X <- s$X
  n <- s$n
  if (family == "poisson") {
    mu <- y + 0.1
    nu <- log(mu)
  } else {
    mu <- rep(0.5, n)
    nu <- rep(0, n)
  }
  wt2 <- rep(1, n)
  llik <- 0
  it <- 0
  repeat {
    yadj <- if (family == "poisson") nu + (y - mu) / mu else nu + (y - mu) / (mu * (1 - mu))
    fits <- lapply(seq_len(n), function(i) .gwl_wls(X, s$Wc[i, ] * wt2, yadj))
    betas <- t(vapply(fits, function(f) f$beta, numeric(ncol(X))))
    nu <- rowSums(X * betas)
    if (family == "poisson") {
      mu <- exp(nu)
      new <- sum(stats::dpois(y, mu, log = TRUE))
    } else {
      mu <- 1 / (1 + exp(-nu))
      new <- sum(y * log(mu) + (1 - y) * log(1 - mu))
    }
    old <- llik
    llik <- new
    if (abs((old - llik) / llik) < tol) break
    wt2 <- if (family == "poisson") mu else mu * (1 - mu)
    it <- it + 1
    if (it == maxiter) break
  }
  se <- t(vapply(fits, function(f) sqrt(rowSums(f$C^2 / rep(wt2, each = nrow(f$C)))), numeric(ncol(X))))
  dev <- if (family == "poisson") sum(ifelse(y != 0, 2 * (y * (log(y / mu) - 1) + mu), 2 * mu)) else -2 * llik
  list(betas = betas, se = se, fitted = mu, loglik = llik, deviance = dev, iterations = it)
}

#' Geographically weighted regression: coefficients, diagnostics and GLMs
#'
#' Front-ends and extensions of `GWRBasic` (an intercept is prepended when
#' `X` has no constant column): `gwrcoef` local coefficients, `gwrres`
#' residuals, `gwrstd` standard errors, `gwrhat` the hat-matrix diagonal,
#' `gwrcv` the leave-one-out CV score (`GWmodel::gwr.cv`), `gwrdlt` the
#' Leung-Mei-Zhang F tests (`GwrFTests`), `gwrtst` the Monte Carlo test of
#' coefficient variability with Philox permutations (`GWmodel::gwr.montecarlo`),
#' `gwrfwl` the local Frisch-Waugh-Lovell coefficients of `X2` net of
#' `X1`, `gwrsur` equation-by-equation GW-SUR with local residual
#' covariances, and `gwrpois` / `gwrlgt` the local-scoring GW Poisson and
#' logistic regressions (`GWmodel::ggwr.basic` with `tol = 1e-5,
#' maxiter = 20`).
#'
#' @param y Response.
#' @param ys List of response vectors (one per equation).
#' @param X Regressors.
#' @param X1 Regressors partialled out.
#' @param X2 Regressors of interest.
#' @param coords Two-column coordinates.
#' @param bw Bandwidth (distance, or neighbour count when adaptive).
#' @param kernel Kernel name (see `GWRKernelWeights`).
#' @param adaptive Adaptive bandwidth.
#' @param method "leung" or "gwmodel" for the F tests.
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @param tol Relative log-likelihood tolerance.
#' @param maxiter Maximum number of local-scoring passes.
#' @return `gwrcoef`, `gwrstd` matrices (one row per location); `gwrres`,
#'   `gwrhat` vectors; `gwrcv` a number; the others lists.
#' @references Brunsdon, C., Fotheringham, A. S. and Charlton, M. E. (1996).
#'   Geographically weighted regression: a method for exploring spatial
#'   nonstationarity. Geographical Analysis 28, 281-298. Brunsdon, C.,
#'   Fotheringham, A. S. and Charlton, M. (1998). Geographically weighted
#'   regression - modelling spatial non-stationarity. The Statistician 47,
#'   431-443. Leung, Y., Mei, C.-L. and Zhang, W.-X. (2000). Statistical
#'   tests for spatial nonstationarity based on the geographically weighted
#'   regression model. Environment and Planning A 32, 9-32. Nakaya, T.,
#'   Fotheringham, A. S., Brunsdon, C. and Charlton, M. (2005).
#'   Geographically weighted Poisson regression for disease association
#'   mapping. Statistics in Medicine 24, 2695-2717. Zellner, A. (1962). An
#'   efficient method of estimating seemingly unrelated regressions. JASA 57,
#'   348-368.
#' @examples
#' P <- cbind((0:15) %% 4, (0:15) %/% 4)
#' X <- matrix((0.3 * (0:15)) %% 1.7)
#' y <- 1 + 2 * X[, 1] + 0.1 * P[, 1] + 0.05 * ((0:15) %% 3)
#' gwrcoef(y, X, P, 3, kernel = "gaussian")[1, ]
#' gwrcv(y, X, P, 3, kernel = "gaussian")
#' @export
gwrcoef <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  .gwl_fit(y, X, coords, bw, kernel, adaptive)$betas
}

#' @rdname gwrcoef
#' @export
gwrres <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  .gwl_fit(y, X, coords, bw, kernel, adaptive)$residuals
}

#' @rdname gwrcoef
#' @export
gwrstd <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  .gwl_fit(y, X, coords, bw, kernel, adaptive)$se
}

#' @rdname gwrcoef
#' @export
gwrhat <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  s <- .gwl_setup(y, X, coords, bw, kernel, adaptive)
  vapply(seq_len(s$n), function(i) sum(s$X[i, ] * .gwl_wls(s$X, s$Wc[i, ], s$y)$C[, i]), 0)
}

#' @rdname gwrcoef
#' @export
gwrcv <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  s <- .gwl_setup(y, X, coords, bw, kernel, adaptive)
  cv <- 0
  for (i in seq_len(s$n)) {
    w <- s$Wc[i, ]
    w[i] <- 0
    b <- .gwl_wls(s$X, w, s$y)$beta
    cv <- cv + (s$y[i] - sum(s$X[i, ] * b))^2
  }
  cv
}

#' @rdname gwrcoef
#' @export
gwrdlt <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE, method = "leung") {
  s <- .gwl_setup(y, X, coords, bw, kernel, adaptive)
  GwrFTests(s$y, s$X, s$P, bw, kernel = kernel, adaptive = adaptive, method = method)
}

#' @rdname gwrcoef
#' @export
gwrtst <- function(y, X, coords, bw = 0.5, nsim = 9, seed = 0, kernel = "bisquare", adaptive = FALSE) {
  y <- as.numeric(y)
  n <- length(y)
  Xm <- .gwl_design(X, n)
  P <- unname(as.matrix(coords)) * 1
  obs <- .gwl_betavar(y, Xm, P, bw, kernel, adaptive)
  sims <- matrix(0, nsim, length(obs))
  for (s in seq_len(nsim)) {
    u <- .morie_random_uniform(n, seed = seed, stream = s - 1)
    perm <- seq_len(n)
    for (i in (n - 1):1) {
      j <- floor(u[i + 1] * (i + 1))
      tmp <- perm[i + 1]
      perm[i + 1] <- perm[j + 1]
      perm[j + 1] <- tmp
    }
    sims[s, ] <- .gwl_betavar(y, Xm, P[perm, , drop = FALSE], bw, kernel, adaptive)
  }
  p <- vapply(seq_along(obs), function(a) 1 - (1 + sum(sims[, a] < obs[a])) / (nsim + 1), 0)
  list(observed_variance = obs, simulated_variance = sims, p_values = p)
}

#' @rdname gwrcoef
#' @export
gwrfwl <- function(y, X1, X2, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  s <- .gwl_setup(y, X1, coords, bw, kernel, adaptive)
  X2 <- unname(as.matrix(X2)) * 1
  k1 <- ncol(s$X)
  betas <- full <- matrix(0, s$n, ncol(X2))
  for (i in seq_len(s$n)) {
    w <- s$Wc[i, ]
    C <- .gwl_wls(s$X, w, s$y)$C
    part <- function(v) v - as.vector(s$X %*% (C %*% v))
    Xt <- apply(X2, 2, part)
    betas[i, ] <- .gwl_wls(matrix(Xt, s$n), w, part(s$y))$beta
    full[i, ] <- .gwl_wls(cbind(s$X, X2), w, s$y)$beta[-seq_len(k1)]
  }
  list(betas = betas, full_model_betas = full)
}

#' @rdname gwrcoef
#' @export
gwrsur <- function(ys, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE) {
  .morie_arg(ys, "l")
  s <- .gwl_setup(ys[[1]], X, coords, bw, kernel, adaptive)
  Y <- lapply(ys, as.numeric)
  m <- length(Y)
  betas <- lapply(Y, function(yk) t(vapply(seq_len(s$n), function(i) .gwl_wls(s$X, s$Wc[i, ], yk)$beta,
                                            numeric(ncol(s$X)))))
  res <- vapply(seq_len(m), function(k) Y[[k]] - rowSums(s$X * betas[[k]]), numeric(s$n))
  res <- matrix(res, s$n)
  cov <- lapply(seq_len(s$n), function(i) crossprod(res * s$Wc[i, ], res) / sum(s$Wc[i, ]))
  list(betas = betas, residuals = t(res), local_covariance = cov)
}

#' @rdname gwrcoef
#' @export
gwrpois <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE, tol = 1e-10, maxiter = 200) {
  .gwl_ggwr(y, X, coords, bw, kernel, adaptive, tol, maxiter, "poisson")
}

#' @rdname gwrcoef
#' @export
gwrlgt <- function(y, X, coords, bw = 0.5, kernel = "bisquare", adaptive = FALSE, tol = 1e-10, maxiter = 200) {
  .gwl_ggwr(y, X, coords, bw, kernel, adaptive, tol, maxiter, "binomial")
}
