#' Scalable approximations to kriging
#'
#' \code{VecchiaLoglik}: Vecchia's approximate log-likelihood from the m
#' nearest predecessors. \code{NngpPredict}: nearest-neighbour Gaussian
#' process kriging. \code{TaperedKriging}: simple kriging with a Wendland
#' tapered covariance. \code{SparseGpKrige}: subset of regressors, DTC
#' (predictive process) and FITC (modified predictive process) through
#' inducing points. \code{FixedRankKriging}: Cressie-Johannesson fixed rank
#' kriging with bisquare bases and the Sherman-Morrison-Woodbury inverse.
#' Covariance models as \code{\link{KrigingCovariance}}. Identical to the
#' Python arm \code{morie.fn.krigapprox}.
#'
#' @param z Observations.
#' @param coords,new_coords Matrices of data and target coordinates.
#' @param model Covariance model.
#' @param m Number of neighbours.
#' @param mean Known constant mean (NULL: sample mean where allowed).
#' @param taper_range Taper range.
#' @param inducing Matrix of inducing points (knots).
#' @param noise Noise (nugget) variance.
#' @param method One of "sor", "dtc", "fitc".
#' @param centers Matrix of basis-function centres.
#' @param radius Basis radius (scalar or one per centre).
#' @param K Covariance matrix of the basis coefficients.
#' @param sigma2_eps,sigma2_xi Measurement-error and fine-scale variances.
#' @return List.
#' @references Vecchia, A. V. (1988). Estimation and model identification for
#'   continuous spatial processes. Journal of the Royal Statistical Society B
#'   50, 297-312.
#'
#'   Datta, A., Banerjee, S., Finley, A. O. and Gelfand, A. E. (2016).
#'   Hierarchical nearest-neighbor Gaussian process models for large
#'   geostatistical datasets. JASA 111, 800-812.
#'
#'   Furrer, R., Genton, M. G. and Nychka, D. (2006). Covariance tapering for
#'   interpolation of large spatial datasets. JCGS 15, 502-523.
#'
#'   Quinonero-Candela, J. and Rasmussen, C. E. (2005). A unifying view of
#'   sparse approximate Gaussian process regression. JMLR 6, 1939-1959.
#'
#'   Cressie, N. and Johannesson, G. (2008). Fixed rank kriging for very
#'   large spatial data sets. Journal of the Royal Statistical Society B 70,
#'   209-226.
#' @examples
#' m <- list(model = "Exp", psill = 1, range = 1, nugget = 0.1)
#' VecchiaLoglik(c(0.2, -0.1, 0.4), rbind(c(0, 0), c(1, 0), c(0, 1)), m, 2)$loglik
#' @export
VecchiaLoglik <- function(z, coords, model, m = 10L, mean = 0) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  n <- length(z)
  ll <- 0
  mus <- numeric(n)
  vs <- numeric(n)
  cii <- KrigingCovariance(0, model)
  for (i in seq_len(n)) {
    nb <- if (i > 1) .ka_nearest(P, P[i, ], seq_len(i - 1), m) else integer(0)
    if (length(nb)) {
      sk <- .ka_sk(z[nb] - mean, .ka_cmat(P[nb, , drop = FALSE], P[nb, , drop = FALSE], model),
                   .ka_cvec(P[nb, , drop = FALSE], P[i, ], model), cii)
      mu <- mean + sk[1]
      v <- sk[2]
    } else {
      mu <- mean
      v <- cii
    }
    mus[i] <- mu
    vs[i] <- v
    ll <- ll + (-0.5 * log(2 * pi * v) - 0.5 * (z[i] - mu)^2 / v)
  }
  list(loglik = ll, conditional_mean = mus, conditional_variance = vs)
}

.ka_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.ka_d <- function(a, b) {
  s <- 0
  for (t in seq_along(a)) s <- s + (a[t] - b[t]) * (a[t] - b[t])
  sqrt(s)
}

.ka_nearest <- function(P, q, idx, m) {
  d <- vapply(idx, function(j) .ka_d(P[j, ], q), 0)
  idx[order(d, idx)][seq_len(min(m, length(idx)))]
}

.ka_cmat <- function(A, B, model) {
  out <- matrix(0, nrow(A), nrow(B))
  for (i in seq_len(nrow(A))) for (j in seq_len(nrow(B))) out[i, j] <- KrigingCovariance(.ka_d(A[i, ], B[j, ]), model)
  out
}

.ka_cvec <- function(A, q, model) vapply(seq_len(nrow(A)), function(i) KrigingCovariance(.ka_d(A[i, ], q), model), 0)

.ka_sk <- function(zc, C, c0, c00) {
  w <- as.vector(solve(C) %*% c0)
  c(.ka_ss(w * zc), c00 - .ka_ss(w * c0))
}

#' @rdname VecchiaLoglik
#' @export
NngpPredict <- function(z, coords, new_coords, model, m = 10L, mean = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  mu <- if (is.null(mean)) .ka_ss(z) / length(z) else mean
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  nbs <- vector("list", nrow(Q))
  for (k in seq_len(nrow(Q))) {
    nb <- .ka_nearest(P, Q[k, ], seq_len(nrow(P)), m)
    sk <- .ka_sk(z[nb] - mu, .ka_cmat(P[nb, , drop = FALSE], P[nb, , drop = FALSE], model),
                 .ka_cvec(P[nb, , drop = FALSE], Q[k, ], model), KrigingCovariance(0, model))
    pred[k] <- mu + sk[1]
    var[k] <- sk[2]
    nbs[[k]] <- nb
  }
  list(prediction = pred, variance = var, neighbours = nbs, mean = mu)
}

#' @rdname VecchiaLoglik
#' @export
TaperedKriging <- function(z, coords, new_coords, model, taper_range, mean = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- length(z)
  mu <- if (is.null(mean)) .ka_ss(z) / n else mean
  ct <- function(h) {
    r <- h / taper_range
    KrigingCovariance(h, model) * (if (r < 1) (1 - r)^4 * (1 + 4 * r) else 0)
  }
  C <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) C[i, j] <- ct(.ka_d(P[i, ], P[j, ]))
  Ci <- solve(C)
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    c0 <- vapply(seq_len(n), function(i) ct(.ka_d(P[i, ], Q[k, ])), 0)
    w <- as.vector(Ci %*% c0)
    pred[k] <- mu + .ka_ss(w * (z - mu))
    var[k] <- ct(0) - .ka_ss(w * c0)
  }
  list(prediction = pred, variance = var, density = sum(C != 0) / (n * n), mean = mu)
}

#' @rdname VecchiaLoglik
#' @export
SparseGpKrige <- function(z, coords, new_coords, model, inducing, noise, method = "fitc", mean = NULL) {
  if (!method %in% c("sor", "dtc", "fitc")) stop("method must be 'sor', 'dtc' or 'fitc'")
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  U <- as.matrix(inducing)
  n <- length(z)
  mu <- if (is.null(mean)) .ka_ss(z) / n else mean
  Kuu <- .ka_cmat(U, U, model)
  Kuui <- solve(Kuu)
  Kuf <- .ka_cmat(U, P, model)
  c00 <- KrigingCovariance(0, model)
  lam <- if (method == "fitc") c00 - colSums(Kuf * (Kuui %*% Kuf)) + noise else rep(noise, n)
  M <- Kuu + Kuf %*% (t(Kuf) / lam)
  S <- solve(M)
  Sv <- as.vector(S %*% (Kuf %*% ((z - mu) / lam)))
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    ku <- .ka_cvec(U, Q[k, ], model)
    pred[k] <- mu + .ka_ss(ku * Sv)
    part <- sum(ku * (S %*% ku))
    var[k] <- if (method == "sor") part else c00 - sum(ku * (Kuui %*% ku)) + part
  }
  list(prediction = pred, variance = var, mean = mu, method = method)
}

#' @rdname VecchiaLoglik
#' @export
FixedRankKriging <- function(z, coords, new_coords, centers, radius, K, sigma2_eps, sigma2_xi = 0, mean = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  Cn <- as.matrix(centers)
  n <- length(z)
  r <- nrow(Cn)
  rad <- if (length(radius) == 1) rep(radius, r) else radius
  mu <- if (is.null(mean)) .ka_ss(z) / n else mean
  K <- as.matrix(K)
  bis <- function(q) vapply(seq_len(r), function(l) {
    d <- .ka_d(q, Cn[l, ])
    if (d < rad[l]) (1 - (d / rad[l])^2)^2 else 0
  }, 0)
  S <- t(vapply(seq_len(n), function(i) bis(P[i, ]), numeric(r)))
  S <- matrix(S, n, r)
  d <- sigma2_xi + sigma2_eps
  Ki <- solve(K)
  StS <- crossprod(S)
  Mi <- solve(Ki + StS / d)
  res <- z - mu
  sir <- res / d - as.vector(S %*% (Mi %*% crossprod(S, res))) / (d * d)
  Stsir <- as.vector(crossprod(S, sir))
  G <- StS / d - StS %*% Mi %*% StS / (d * d)
  KGK <- K %*% G %*% K
  KStsir <- as.vector(K %*% Stsir)
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    s0 <- bis(Q[k, ])
    pred[k] <- mu + .ka_ss(s0 * KStsir)
    var[k] <- sum(s0 * (K %*% s0)) + sigma2_xi - sum(s0 * (KGK %*% s0))
  }
  list(prediction = pred, mspe = var, basis = S, mean = mu)
}
