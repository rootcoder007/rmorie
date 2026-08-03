# Maximum-likelihood spatial models, point-pattern K, co-kriging and
# the local differential privacy mechanism.
#
# R mirror of the corresponding block in
# morie/src/morie/fn/_robust_core.py.  The two spatial regressions are
# checked against spatialreg::lagsarlm and spatialreg::errorsarlm --
# note those live in *spatialreg*, not *spdep*, since spdep was split.

#' @noRd
morie_logdet_I_minus <- function(rho, W) {
  determinant(diag(nrow(W)) - rho * W, logarithm = TRUE)$modulus[1]
}

#' Maximum-likelihood spatial regression models
#'
#' `morie_spatial_lag_model` fits `y = rho W y + X beta + eps` and
#' `morie_spatial_error_model` fits `y = X beta + u`, `u = lambda W u +
#' eps`.  Both maximise Ord's concentrated log-likelihood, so only the
#' spatial parameter is searched and the rest follows in closed form.
#' Ordinary least squares on the lag model is inconsistent, which is
#' why the lag version exists at all.
#' @param y numeric response
#' @param X predictor matrix
#' @param W spatial weights matrix
#' @param add_intercept prepend an intercept column
#' @return list with the spatial parameter, `beta`, `sigma2`,
#'   `loglik` and `residuals`
#' @export
morie_spatial_lag_model <- function(y, X, W, add_intercept = TRUE) {
  X <- as.matrix(X); W <- as.matrix(W)
  n <- length(y)
  if (nrow(X) != n) stop("X has ", nrow(X), " rows but y has ", n)
  if (nrow(W) != n || ncol(W) != n) stop("W must be ", n, " x ", n)
  if (add_intercept) X <- cbind(1, X)
  Wy <- as.numeric(W %*% y)
  e0 <- stats::residuals(stats::lm.fit(X, y))
  ed <- stats::residuals(stats::lm.fit(X, Wy))
  negll <- function(rho) {
    sse <- sum((e0 - rho * ed)^2)
    -(morie_logdet_I_minus(rho, W) - (n / 2) * log(sse / n))
  }
  rho <- stats::optimize(negll, c(-0.999, 0.999),
                         tol = .Machine$double.eps^0.5)$minimum
  fit <- stats::lm.fit(X, y - rho * Wy)
  beta <- as.numeric(fit$coefficients)
  resid <- as.numeric(stats::residuals(fit))
  s2 <- sum(resid^2) / n
  list(rho = rho, beta = beta, residuals = resid, sigma2 = s2,
       loglik = morie_logdet_I_minus(rho, W) -
         (n / 2) * log(2 * pi * s2) - n / 2, n = n)
}

#' @rdname morie_spatial_lag_model
#' @export
morie_spatial_error_model <- function(y, X, W, add_intercept = TRUE) {
  X <- as.matrix(X); W <- as.matrix(W)
  n <- length(y)
  if (nrow(X) != n) stop("X has ", nrow(X), " rows but y has ", n)
  if (nrow(W) != n || ncol(W) != n) stop("W must be ", n, " x ", n)
  if (add_intercept) X <- cbind(1, X)
  Wy <- as.numeric(W %*% y)
  WX <- W %*% X
  negll <- function(lam) {
    fit <- stats::lm.fit(X - lam * WX, y - lam * Wy)
    sse <- sum(stats::residuals(fit)^2)
    -(morie_logdet_I_minus(lam, W) - (n / 2) * log(sse / n))
  }
  lam <- stats::optimize(negll, c(-0.999, 0.999),
                         tol = .Machine$double.eps^0.5)$minimum
  fit <- stats::lm.fit(X - lam * WX, y - lam * Wy)
  beta <- as.numeric(fit$coefficients)
  s2 <- sum(stats::residuals(fit)^2) / n
  resid <- as.numeric(y - X %*% beta)
  list(lambda = lam, beta = beta, residuals = resid, sigma2 = s2,
       loglik = morie_logdet_I_minus(lam, W) -
         (n / 2) * log(2 * pi * s2) - n / 2, n = n)
}

#' Ripley's K function
#'
#' `K(r) = |A| n^-2 sum_i sum_{j != i} w_ij 1(d_ij <= r)` with the
#' isotropic edge correction, without which K is biased downwards near
#' the window boundary.  Under complete spatial randomness
#' `K(r) = pi r^2`; `L = sqrt(K/pi)` is the variance-stabilised form.
#' @param coords n x 2 matrix of point coordinates
#' @param r_grid radii at which to evaluate K
#' @param area window area; the bounding box by default
#' @param edge_correction apply the isotropic correction
#' @return list with `r`, `K`, `L`, `csr_K` and `intensity`
#' @export
morie_ripley_k <- function(coords, r_grid, area = NULL,
                           edge_correction = TRUE) {
  P <- as.matrix(coords)
  if (ncol(P) != 2) stop("coords must be an n x 2 matrix")
  n <- nrow(P)
  if (n < 2) stop("need at least 2 points")
  x0 <- min(P[, 1]); x1 <- max(P[, 1])
  y0 <- min(P[, 2]); y1 <- max(P[, 2])
  if (is.null(area)) area <- (x1 - x0) * (y1 - y0)
  if (area <= 0) stop("degenerate window: zero area")
  lam <- n / area
  D <- as.matrix(stats::dist(P))
  wfun <- function(i, d) {
    if (!edge_correction || d <= 0) return(1)
    out <- 0
    for (dd in c(P[i, 1] - x0, x1 - P[i, 1], P[i, 2] - y0, y1 - P[i, 2]))
      if (dd < d) out <- out + acos(max(-1, min(1, dd / d)))
    frac <- 1 - out / pi
    if (frac > 1e-9) 1 / frac else 1
  }
  K <- vapply(r_grid, function(r) {
    tot <- 0
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (i == j) next
      d <- D[i, j]
      if (d <= r) tot <- tot + wfun(i, d)
    }
    tot / (n * lam)
  }, numeric(1))
  list(r = r_grid, K = K, L = ifelse(K > 0, sqrt(K / pi), 0),
       csr_K = pi * r_grid^2, n = n, area = area, intensity = lam)
}

#' Ordinary co-kriging
#'
#' `Z1*(s0) = sum lambda_i Z1(s_i) + sum mu_j Z2(s_j)`, solved under
#' the constraints `sum lambda = 1` and `sum mu = 0`.  The covariate
#' contributes only through the cross-variogram: set it to zero and the
#' mu weights vanish, leaving ordinary kriging on Z1.
#' @param coords n x d matrix of coordinates
#' @param z1 primary variable
#' @param z2 secondary variable
#' @param s0 prediction location
#' @param cross_vario cross-variogram function of distance
#' @param model direct variogram function of distance
#' @return list with `prediction`, `variance`, `lambda` and `mu`
#' @export
morie_cokriging <- function(coords, z1, z2, s0, cross_vario = NULL,
                            model = NULL) {
  P <- as.matrix(coords)
  n <- nrow(P)
  if (length(z1) != n || length(z2) != n)
    stop("z1 and z2 must have one value per coordinate")
  d <- as.matrix(stats::dist(P))
  rng <- mean(d[upper.tri(d)])
  if (is.null(model)) model <- function(h) 1 - exp(-h / rng)
  if (is.null(cross_vario))
    cross_vario <- function(h) 0.5 * (1 - exp(-h / rng))
  d0 <- vapply(seq_len(n), function(i)
    sqrt(sum((P[i, ] - s0)^2)), numeric(1))
  m <- 2 * n + 2
  A <- matrix(0, m, m)
  rhs <- numeric(m)
  A[1:n, 1:n] <- model(d)
  A[1:n, (n + 1):(2 * n)] <- cross_vario(d)
  A[(n + 1):(2 * n), 1:n] <- cross_vario(d)
  A[(n + 1):(2 * n), (n + 1):(2 * n)] <- model(d)
  A[1:n, 2 * n + 1] <- 1; A[2 * n + 1, 1:n] <- 1
  A[(n + 1):(2 * n), 2 * n + 2] <- 1; A[2 * n + 2, (n + 1):(2 * n)] <- 1
  rhs[1:n] <- model(d0)
  rhs[(n + 1):(2 * n)] <- cross_vario(d0)
  rhs[2 * n + 1] <- 1
  sol <- solve(A, rhs)
  lam <- sol[1:n]; mu <- sol[(n + 1):(2 * n)]
  list(prediction = sum(lam * z1) + sum(mu * z2),
       variance = sum(lam * rhs[1:n]) +
         sum(mu * rhs[(n + 1):(2 * n)]) + sol[2 * n + 1],
       lambda = lam, mu = mu, n = n, range = rng)
}

#' k-ary randomised response (local differential privacy)
#'
#' `P(report = v | true = u)` is `e^eps / (k - 1 + e^eps)` when
#' `v == u` and `1 / (k - 1 + e^eps)` otherwise, so the ratio of any
#' two conditional probabilities is at most `e^eps` -- the
#' eps-local-differential-privacy guarantee.  Each respondent perturbs
#' their own value, so the collector never sees the truth.  Raw counts
#' are biased towards uniform, hence the debiased estimate that
#' inverts the transition matrix.
#' @param truth integer vector of true values in `0..k-1`
#' @param k number of categories
#' @param epsilon privacy parameter; smaller is more private
#' @param seed RNG seed
#' @return list with `reports`, `observed`, `estimate`, `p_keep`,
#'   `p_flip`
#' @export
morie_local_dp_randomised_response <- function(truth, k, epsilon,
                                               seed = 2) {
  k <- as.integer(k)
  if (k < 2) stop("k must be at least 2")
  if (epsilon <= 0) stop("epsilon must be positive")
  v <- as.integer(truth)
  if (any(v < 0 | v >= k)) stop("values must lie in 0..k-1")
  e <- exp(epsilon)
  p_keep <- e / (k - 1 + e)
  p_flip <- 1 / (k - 1 + e)
  set.seed(seed)
  reports <- vapply(v, function(u) {
    if (stats::runif(1) < p_keep) u
    else sample(setdiff(0:(k - 1), u), 1)
  }, numeric(1))
  n <- length(v)
  obs <- vapply(0:(k - 1), function(u) sum(reports == u) / n, numeric(1))
  list(reports = reports, observed = obs,
       estimate = (obs - p_flip) * (k - 1 + e) / (e - 1),
       p_keep = p_keep, p_flip = p_flip, epsilon = epsilon,
       k = k, n = n)
}
