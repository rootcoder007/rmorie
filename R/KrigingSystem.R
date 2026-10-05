.krs_comps <- function(model) if (!is.null(model$model)) list(model) else model

#' Covariance of a gstat vgm model
#'
#' Sum over the components (lists with \code{model} Exp, Gau, Sph, Mat with
#' \code{kappa}, or Nug; \code{psill}, \code{range}, \code{nugget}) of
#' \eqn{psill\,\rho(h/range)} plus the nugget at \eqn{h = 0}.
#'
#' @param h Distance(s).
#' @param model One component list or a list of nested components.
#' @return Covariance values.
#' @examples
#' KrigingCovariance(1, list(list(model = "Nug", psill = 0.5),
#'                           list(model = "Exp", psill = 2, range = 2)))
#' @export
KrigingCovariance <- function(h, model) {
  h <- abs(h)
  Reduce(`+`, lapply(.krs_comps(model), function(c) .stk_comp(h, c)))
}

.krs_gauss_legendre <- function(n) {
  xs <- ws <- numeric(n)
  leg <- function(x) {
    p0 <- 1
    p1 <- x
    if (n > 1) {
      for (k in 2:n) {
        p2 <- ((2 * k - 1) * x * p1 - (k - 1) * p0) / k
        p0 <- p1
        p1 <- p2
      }
    }
    c(p1, if (n > 1) n * (x * p1 - p0) / (x * x - 1) else 1)
  }
  for (i in seq_len(n)) {
    x <- cos(pi * (i - 0.25) / (n + 0.5))
    for (it in 1:100) {
      v <- leg(x)
      dx <- v[1] / v[2]
      x <- x - dx
      if (abs(dx) < 1e-16) break
    }
    v <- leg(x)
    xs[i] <- x
    ws[i] <- if (n > 1) 2 / ((1 - x * x) * v[2]^2) else 2
  }
  list(x = xs, w = ws)
}

#' Discretisation of a rectangular block
#'
#' \code{method = "gauss"} (gstat's default for \code{krige(block = c(bx, by))}):
#' n-point Gauss-Legendre nodes \eqn{b_j x_i / 2} per dimension with product
#' weights \eqn{\prod w_i / 2} (gstat keeps these in single precision, so
#' its block kriging differs in the eighth digit); \code{"regular"}: the \eqn{n^d} cell centres
#' \eqn{b_j((i + 1/2)/n - 1/2)} with equal weights.
#'
#' @param block Block sides (1 to 3 values).
#' @param n Points per dimension.
#' @param method \code{"gauss"} or \code{"regular"}.
#' @return List with \code{offsets} (matrix) and \code{weights}.
#' @examples
#' BlockDiscretize(c(2, 2), 2, "regular")
#' @export
BlockDiscretize <- function(block, n = 4L, method = "gauss") {
  if (!length(block) %in% 1:3 || n < 1 || !method %in% c("gauss", "regular")) {
    stop("block must have 1-3 sides, n >= 1 and method gauss or regular")
  }
  if (method == "gauss") {
    g <- .krs_gauss_legendre(as.integer(n))
    ax <- lapply(block, function(b) list(v = b * g$x / 2, w = g$w / 2))
  } else {
    ax <- lapply(block, function(b) list(v = b * ((0:(n - 1) + 0.5) / n - 0.5), w = rep(1 / n, n)))
  }
  off <- matrix(0, 1, 0)
  w <- 1
  for (a in ax) {
    k <- length(a$v)
    off <- cbind(off[rep(seq_len(nrow(off)), each = k), , drop = FALSE], rep(a$v, times = nrow(off)))
    w <- rep(w, each = k) * rep(a$w, times = length(w))
  }
  list(offsets = unname(off), weights = w)
}

.krs_one <- function(z, P, X, q, x0, model, beta, block, blue) {
  n <- length(z)
  C <- KrigingCovariance(as.matrix(stats::dist(P)), model)
  Ci <- solve(C)
  dq <- function(s) sqrt(colSums((t(P) - s)^2))
  if (is.null(block)) {
    c0 <- KrigingCovariance(dq(q), model)
    c00 <- KrigingCovariance(0, model)
  } else {
    pts <- sweep(block$offsets, 2, q, `+`)
    bw <- block$weights
    c0 <- as.vector(vapply(seq_len(nrow(pts)), function(s) KrigingCovariance(dq(pts[s, ]), model), numeric(n)) %*% bw)
    comps <- .krs_comps(model)
    sill0 <- sum(vapply(comps, function(c) if (identical(c$model, "Nug")) 0 else if (is.null(c$psill)) 1 else c$psill, 0))
    Bc <- KrigingCovariance(as.matrix(stats::dist(pts)), model)
    diag(Bc) <- sill0
    c00 <- as.numeric(t(bw) %*% Bc %*% bw)
  }
  Cic0 <- as.vector(Ci %*% c0)
  if (!is.null(beta)) {
    pred <- sum(x0 * beta) + sum(Cic0 * (z - as.vector(X %*% beta)))
    return(list(pred = pred, var = c00 - sum(c0 * Cic0), lam = Cic0, mu = numeric(0), beta = numeric(0)))
  }
  CiX <- Ci %*% X
  Ai <- solve(t(X) %*% CiX)
  bhat <- as.vector(Ai %*% t(X) %*% (Ci %*% z))
  if (blue) {
    return(list(pred = sum(x0 * bhat), var = as.numeric(t(x0) %*% Ai %*% x0), lam = numeric(0), mu = numeric(0),
                beta = bhat))
  }
  r <- x0 - as.vector(t(X) %*% Cic0)
  Air <- as.vector(Ai %*% r)
  lam <- Cic0 + as.vector(CiX %*% Air)
  list(pred = sum(lam * z), var = c00 - sum(c0 * Cic0) + sum(r * Air), lam = lam, mu = -Air, beta = bhat)
}

#' Augmented universal kriging system
#'
#' The system with matrix \eqn{(C, X; X', 0)}, unknowns \eqn{(\lambda, \mu)} and
#' right-hand side \eqn{(c_0, x_0)}, with the data covariance
#' matrix \eqn{C}, covariances \eqn{c_0} with the target and trend design
#' \eqn{X} (default ordinary kriging); returns the matrix, right-hand side,
#' weights and Lagrange multipliers (Cressie 1993, section 3.4).
#'
#' @param coords Matrix of data locations.
#' @param target Target location.
#' @param model Covariance model (\code{\link{KrigingCovariance}}).
#' @param X Trend design at the data.
#' @param x0 Trend design at the target.
#' @return List with \code{matrix}, \code{rhs}, \code{weights},
#'   \code{lagrange}.
#' @examples
#' KrigingSystem(rbind(c(0, 0), c(1, 0)), c(0.5, 0), list(model = "Exp", psill = 1, range = 1))$weights
#' @export
KrigingSystem <- function(coords, target, model, X = NULL, x0 = NULL) {
  P <- as.matrix(coords)
  n <- nrow(P)
  if (is.null(X)) {
    X <- matrix(1, n, 1)
    x0 <- 1
  }
  X <- as.matrix(X)
  p <- ncol(X)
  A <- rbind(cbind(KrigingCovariance(as.matrix(stats::dist(P)), model), X), cbind(t(X), matrix(0, p, p)))
  b <- c(KrigingCovariance(sqrt(colSums((t(P) - target)^2)), model), x0)
  s <- solve(A, b)
  list(matrix = unname(A), rhs = unname(b), weights = unname(s[1:n]), lagrange = unname(s[n + seq_len(p)]))
}

#' Kriging prediction and variance (gstat conventions)
#'
#' Simple kriging (\code{beta} given), universal kriging (\code{X}; default a
#' column of ones, ordinary kriging), the GLS trend (\code{blue}), block
#' kriging (\code{block} from \code{\link{BlockDiscretize}} or a matrix of
#' equally weighted offsets; nugget left out of the block-block average) and
#' local neighbourhoods (\code{nmax}, \code{maxdist}), as \code{gstat::krige}
#' (Cressie 1993; Wackernagel 2003; Pebesma 2004). Identical to the Python
#' arm \code{morie.fn.krgsys.krige}.
#'
#' @param z Observations.
#' @param coords Matrix of locations (any dimension).
#' @param new_coords Matrix of targets (block centres with \code{block}).
#' @param model Covariance model (\code{\link{KrigingCovariance}}).
#' @param X Trend design at the data.
#' @param X0 Trend design at the targets.
#' @param beta Known trend coefficients (simple kriging).
#' @param block Block discretisation.
#' @param nmax Maximum number of nearest neighbours.
#' @param maxdist Maximum neighbour distance.
#' @param blue Return the GLS trend.
#' @return List with \code{prediction}, \code{variance}, \code{weights},
#'   \code{lagrange} and, with a global neighbourhood and unknown trend,
#'   the GLS \code{beta} and \code{trend_residuals}.
#' @references Cressie, N. (1993). Statistics for Spatial Data, rev. edn.
#'   Wiley, New York.
#'
#'   Wackernagel, H. (2003). Multivariate Geostatistics, 3rd edn. Springer.
#'
#'   Pebesma, E. J. (2004). Multivariable geostatistics in S: the gstat
#'   package. Computers and Geosciences 30, 683-691.
#' @examples
#' m <- list(model = "Exp", psill = 1, range = 2, nugget = 0.1)
#' Krige(c(1, 3, 2), rbind(c(0, 0), c(2, 0), c(0, 2)), rbind(c(1, 1)), m)$prediction
#' @export
Krige <- function(z, coords, new_coords, model, X = NULL, X0 = NULL, beta = NULL, block = NULL, nmax = NULL,
                  maxdist = NULL, blue = FALSE) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- length(z)
  if (nrow(P) != n || ncol(Q) != ncol(P)) stop("coords must match z and new_coords must have the same dimension")
  if (is.null(X)) {
    X <- matrix(1, n, 1)
    X0 <- matrix(1, nrow(Q), 1)
  } else {
    if (is.null(X0)) stop("X0 is required with X")
    X <- as.matrix(X)
    X0 <- as.matrix(X0)
    if (nrow(X) != n || nrow(X0) != nrow(Q)) stop("X must have n rows and X0 m rows")
  }
  if (!is.null(block) && !is.list(block)) {
    block <- list(offsets = as.matrix(block), weights = rep(1 / nrow(as.matrix(block)), nrow(as.matrix(block))))
  }
  if (!is.null(block)) block$offsets <- as.matrix(block$offsets)
  local <- !is.null(nmax) || !is.null(maxdist)
  out <- lapply(seq_len(nrow(Q)), function(k) {
    idx <- seq_len(n)
    if (local) {
      d <- sqrt(colSums((t(P) - Q[k, ])^2))
      idx <- order(d)
      if (!is.null(maxdist)) idx <- idx[d[idx] <= maxdist]
      if (!is.null(nmax)) idx <- utils::head(idx, nmax)
    }
    if (!length(idx)) return(list(pred = NaN, var = NaN, lam = numeric(0), mu = numeric(0)))
    .krs_one(z[idx], P[idx, , drop = FALSE], X[idx, , drop = FALSE], Q[k, ], X0[k, ], model, beta, block, blue)
  })
  res <- list(prediction = vapply(out, `[[`, 0, "pred"), variance = vapply(out, `[[`, 0, "var"),
              weights = lapply(out, `[[`, "lam"), lagrange = lapply(out, `[[`, "mu"))
  if (is.null(beta) && !local && n > 0) {
    res$beta <- .krs_one(z, P, X, P[1, ], X[1, ], model, NULL, NULL, TRUE)$beta
    res$trend_residuals <- z - as.vector(X %*% res$beta)
  }
  res
}

#' Cross-validation of kriging (gstat krige.cv)
#'
#' Leave-one-out, or by the integer fold labels \code{folds}: each fold is
#' predicted by \code{\link{Krige}} from the others. Returns predictions,
#' variances, observed values, residuals (observed minus predicted),
#' z-scores, RMSE, MAE, mean error and mean squared z-score.
#'
#' @inheritParams Krige
#' @param folds Fold labels (default leave-one-out).
#' @return List of cross-validation results.
#' @examples
#' m <- list(model = "Sph", psill = 1, range = 3)
#' KrigeCV(c(1, 2, 1.5, 1.2, 0.7), rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(2, 2)), m)$rmse
#' @export
KrigeCV <- function(z, coords, model, X = NULL, beta = NULL, folds = NULL, nmax = NULL, maxdist = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  n <- length(z)
  lab <- if (is.null(folds)) seq_len(n) else as.integer(folds)
  if (length(lab) != n || nrow(P) != n) stop("coords and folds must match z")
  pred <- var <- numeric(n)
  for (f in sort(unique(lab))) {
    o <- which(lab == f)
    k <- which(lab != f)
    r <- Krige(z[k], P[k, , drop = FALSE], P[o, , drop = FALSE], model,
               X = if (is.null(X)) NULL else as.matrix(X)[k, , drop = FALSE],
               X0 = if (is.null(X)) NULL else as.matrix(X)[o, , drop = FALSE],
               beta = beta, nmax = nmax, maxdist = maxdist)
    pred[o] <- r$prediction
    var[o] <- r$variance
  }
  res <- z - pred
  zs <- res / sqrt(var)
  list(prediction = pred, variance = var, observed = z, residual = res, zscore = zs, fold = lab,
       rmse = sqrt(mean(res^2)), mae = mean(abs(res)), me = mean(res), msz = mean(zs^2))
}

#' Lognormal kriging with the unbiased back-transform
#'
#' Kriges \eqn{Y = \log Z}; simple kriging back-transforms as
#' \eqn{\exp(\hat Y + \sigma^2/2)}, ordinary kriging as
#' \eqn{\exp(\hat Y + \sigma^2/2 - m)} with \eqn{m = -\mu} the Lagrange
#' multiplier of the variogram-form system (Journel 1980; Cressie 1993,
#' eq. 3.2.40).
#'
#' @inheritParams Krige
#' @param beta Known mean of the log values (simple kriging).
#' @return List with \code{prediction}, \code{log_prediction},
#'   \code{log_variance}.
#' @references Journel, A. G. (1980). The lognormal approach to predicting
#'   local distributions of selective mining unit grades. Mathematical
#'   Geology 12, 285-303.
#' @examples
#' m <- list(model = "Exp", psill = 0.3, range = 2)
#' KrigeLognormal(c(1, 3, 2), rbind(c(0, 0), c(2, 0), c(0, 2)), rbind(c(1, 1)), m)$prediction
#' @export
KrigeLognormal <- function(z, coords, new_coords, model, beta = NULL) {
  .morie_arg(z, "n")
  if (any(z <= 0)) stop("lognormal kriging needs positive z")
  r <- Krige(log(z), coords, new_coords, model, beta = beta)
  adj <- if (is.null(beta)) vapply(r$lagrange, `[`, 0, 1) else 0
  list(prediction = exp(r$prediction + r$variance / 2 + adj), log_prediction = r$prediction,
       log_variance = r$variance)
}

#' Factorial kriging of one nested component
#'
#' Estimates component \code{component} of a nested covariance by
#' \eqn{\lambda' z} under \eqn{\sum\lambda = 0}: \eqn{C\lambda + \mu 1 =
#' c_{k,0}}, variance \eqn{C_k(0) - \lambda' c_{k,0}} (Wackernagel 2003,
#' chapter 14; Goovaerts 1997, section 5.6).
#'
#' @inheritParams Krige
#' @param component Index (1-based) of the component to estimate.
#' @return List with \code{prediction} and \code{variance}.
#' @references Goovaerts, P. (1997). Geostatistics for Natural Resources
#'   Evaluation. Oxford University Press, New York.
#' @examples
#' m <- list(list(model = "Exp", psill = 1, range = 1), list(model = "Sph", psill = 0.5, range = 4))
#' FactorialKrige(c(1, 3, 2, 2.5), rbind(c(0, 0), c(2, 0), c(0, 2), c(2, 2)), rbind(c(0.5, 0.2)), m, 2)
#' @export
FactorialKrige <- function(z, coords, new_coords, model, component) {
  comps <- .krs_comps(model)
  if (component < 1 || component > length(comps)) stop("component out of range")
  ck <- comps[[component]]
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  Ci <- solve(KrigingCovariance(as.matrix(stats::dist(P)), comps))
  one <- rowSums(Ci)
  out <- t(vapply(seq_len(nrow(Q)), function(k) {
    c0 <- .stk_comp(sqrt(colSums((t(P) - Q[k, ])^2)), ck)
    Cic0 <- as.vector(Ci %*% c0)
    lam <- Cic0 - sum(Cic0) / sum(one) * one
    c(sum(lam * z), .stk_comp(0, ck) - sum(lam * c0))
  }, c(0, 0)))
  list(prediction = out[, 1], variance = out[, 2])
}

#' Gaussian quantile and exceedance maps of kriging predictions
#'
#' \code{KrigingQuantile}: \eqn{\hat Z + z_p\sigma}; \code{KrigingExceedance}:
#' \eqn{P(Z > t) = 1 - \Phi((t - \hat Z)/\sigma)}.
#'
#' @param prediction Kriging predictions.
#' @param variance Kriging variances.
#' @param p Probability in (0, 1).
#' @return Numeric vector.
#' @examples
#' KrigingQuantile(c(1, 2), c(0.25, 1), 0.975)
#' @export
KrigingQuantile <- function(prediction, variance, p) {
  if (p <= 0 || p >= 1) stop("p must be in (0, 1)")
  prediction + .morie_normal_quantile(p) * sqrt(variance)
}

#' @rdname KrigingQuantile
#' @param threshold Threshold \eqn{t}.
#' @examples
#' KrigingExceedance(c(1, 2), c(0.25, 1), 1.5)
#' @export
KrigingExceedance <- function(prediction, variance, threshold) {
  stats::pnorm(threshold, prediction, sqrt(variance), lower.tail = FALSE)
}
