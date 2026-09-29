.spbl_impacts <- function(coef, rho, X, W, link, method) {
  b <- as.numeric(coef)
  X <- unname(as.matrix(X)) * 1
  W <- unname(as.matrix(W)) * 1
  n <- nrow(X)
  Si <- solve(diag(n) - rho * W)
  eta <- as.vector(Si %*% (X %*% b))
  sd <- switch(method, marginal = sqrt(rowSums(Si^2)), lesage_pace = rep(1, n),
               stop("method must be 'lesage_pace' or 'marginal'"))
  z <- eta / sd
  dens <- if (link == "probit") exp(-0.5 * z * z) / sqrt(2 * pi) else 1 / (2 + exp(z) + exp(-z))
  dd <- dens / sd
  cols <- which(apply(X, 2, function(v) length(unique(v)) > 1))
  direct <- mean(dd * diag(Si)) * b[cols]
  total <- mean(dd * rowSums(Si)) * b[cols]
  list(direct = direct, indirect = total - direct, total = total,
       observation_direct = outer(dd * diag(Si), b[cols]), variables = cols - 1L)
}

.spbl_design <- function(X) {
  X <- unname(as.matrix(X)) * 1
  if (any(apply(X, 2, function(v) length(unique(v)) == 1 && v[1] != 0))) X else cbind(1, X)
}

.spbl_logphi <- function(x) if (x > -5) log(stats::pnorm(x)) else log(stats::pnorm(x) + 1e-300)

.spbl_ghk <- function(par, y, X, W, U) {
  n <- length(y)
  p <- ncol(X)
  rho <- tanh(par[p + 1])
  Si <- solve(diag(n) - rho * W)
  s <- 2 * y - 1
  m <- s * as.vector(Si %*% (X %*% par[seq_len(p)]))
  L <- t(chol(outer(s, s) * tcrossprod(Si)))
  logs <- vapply(seq_len(nrow(U)), function(r) {
    eta <- numeric(n)
    lp <- 0
    for (i in seq_len(n)) {
      cc <- (m[i] + if (i > 1) sum(L[i, seq_len(i - 1)] * eta[seq_len(i - 1)]) else 0) / L[i, i]
      lq <- .spbl_logphi(cc)
      lp <- lp + lq
      q <- (1 - U[r, i]) * exp(lq)
      eta[i] <- -stats::qnorm(min(max(q, 1e-300), 1 - 1e-16))
    }
    lp
  }, 0)
  top <- max(logs)
  top + log(sum(exp(logs - top)) / length(logs))
}

.spbl_nm <- function(f, x0, xtol = 1e-10, ftol = 1e-12, maxiter = 20000) {
  k <- length(x0)
  pts <- matrix(x0, k + 1, k, byrow = TRUE)
  for (j in seq_len(k)) pts[j + 1, j] <- if (x0[j] != 0) x0[j] * 1.05 else 0.00025
  vals <- apply(pts, 1, f)
  it <- 0
  while (it < maxiter) {
    o <- order(vals, seq_len(k + 1))
    pts <- pts[o, , drop = FALSE]
    vals <- vals[o]
    if (max(abs(vals[-1] - vals[1])) <= ftol && max(abs(sweep(pts[-1, , drop = FALSE], 2, pts[1, ]))) <= xtol) break
    it <- it + 1
    cen <- colSums(pts[seq_len(k), , drop = FALSE]) / k
    xr <- 2 * cen - pts[k + 1, ]
    fr <- f(xr)
    if (fr < vals[1]) {
      xe <- 3 * cen - 2 * pts[k + 1, ]
      fe <- f(xe)
      if (fe < fr) {
        pts[k + 1, ] <- xe
        vals[k + 1] <- fe
      } else {
        pts[k + 1, ] <- xr
        vals[k + 1] <- fr
      }
    } else if (fr < vals[k]) {
      pts[k + 1, ] <- xr
      vals[k + 1] <- fr
    } else {
      if (fr < vals[k + 1]) {
        xc <- 1.5 * cen - 0.5 * pts[k + 1, ]
        fc <- f(xc)
        accept <- fc <= fr
      } else {
        xc <- 0.5 * cen + 0.5 * pts[k + 1, ]
        fc <- f(xc)
        accept <- fc < vals[k + 1]
      }
      if (accept) {
        pts[k + 1, ] <- xc
        vals[k + 1] <- fc
      } else {
        for (i in 2:(k + 1)) {
          pts[i, ] <- pts[1, ] + 0.5 * (pts[i, ] - pts[1, ])
          vals[i] <- f(pts[i, ])
        }
      }
    }
  }
  b <- which.min(vals)
  list(par = pts[b, ], value = vals[b], iterations = it)
}

.spbl_panel <- function(y, X, W, unit_id, time_id) {
  y <- as.numeric(y)
  X <- unname(as.matrix(X)) * 1
  ids <- sort(unique(unit_id))
  N <- length(ids)
  if (is.null(time_id)) time_id <- stats::ave(seq_along(unit_id), unit_id, FUN = seq_along) - 1
  o <- order(time_id, match(unit_id, ids))
  T <- length(y) %/% N
  if (N * T != length(y)) stop("the panel must be balanced")
  keep <- which(apply(X, 2, function(v) length(unique(v)) > 1))
  list(y = y[o], X = X[o, keep, drop = FALSE], W = kronecker(diag(T), unname(as.matrix(W)) * 1),
       unit = match(unit_id, ids)[o], N = N)
}

#' Spatial probit and logit: impacts, probabilities, simulated ML and panels
#'
#' `spprmf`, `sprmfdi` and `splgtmf` are the LeSage-Pace average direct,
#' indirect and total impacts of SAR probit and logit models (the effect
#' matrix `diag(f(eta)) S^-1 beta_r`, `eta = S^-1 X beta`, computed exactly;
#' `method = "marginal"` differentiates the exact marginal probability);
#' `spprprd` the predicted probabilities `Phi(eta_i / s_i)`; `spprml` the
#' SAR probit by GHK simulated maximum likelihood (Beron and Vijverberg
#' 2004) with Philox common random numbers and Nelder-Mead; `splgtml` the
#' Klier-McMillen linearized GMM logit (`SpatialLogitGmm`); `sptfx` and
#' `sptrx` the fixed-effects (unit dummies) and Mundlak correlated
#' random-effects spatial probit panels by the Pinkse-Slade GMM
#' (`SpatialProbitGmm`) with `I_T kron W`.
#'
#' @param coef Coefficients (intercept first when `X` has one).
#' @param rho Spatial autoregressive parameter.
#' @param X Regressors.
#' @param W Spatial weights.
#' @param method "lesage_pace" or "marginal".
#' @param y Binary response.
#' @param Z Instruments (NULL for the default).
#' @param nsim Number of GHK simulation draws.
#' @param seed Philox seed.
#' @param xtol Nelder-Mead parameter tolerance.
#' @param maxiter Maximum number of Nelder-Mead iterations.
#' @param unit_id,time_id Panel identifiers (`time_id` defaults to the order of
#'   appearance within each unit).
#' @return Lists (see the Python arm); `spprprd` a vector.
#' @references LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press. Beron, K. J. and Vijverberg, W. P. M. (2004).
#'   Probit in a spatial context: a Monte Carlo analysis. In Advances in
#'   Spatial Econometrics. Springer, 169-195. Klier, T. and McMillen, D. P.
#'   (2008). Clustering of auto supplier plants in the United States. JBES
#'   26, 460-471. Pinkse, J. and Slade, M. E. (1998). Contracting in space.
#'   Journal of Econometrics 85, 125-154. Mundlak, Y. (1978). On the pooling
#'   of time series and cross section data. Econometrica 46, 69-85.
#' @examples
#' W <- 1 * (abs(outer(1:10, 1:10, "-")) == 1)
#' W <- W / rowSums(W)
#' X <- cbind(1, c(2, -1, 0.1, 1.5, 0.6, -0.4, 0.9, -1.3, 0.2, 1.1))
#' spprmf(c(-0.2, 0.9), 0.4, X, W)$direct
#' spprprd(c(-0.2, 0.9), X, W, rho = 0.4)[1:2]
#' @export
spprmf <- function(coef, rho, X, W, method = "lesage_pace") .spbl_impacts(coef, rho, X, W, "probit", method)

#' @rdname spprmf
#' @export
sprmfdi <- function(coef, rho, X, W, method = "lesage_pace") {
  r <- .spbl_impacts(coef, rho, X, W, "probit", method)
  r[c("direct", "indirect", "total")]
}

#' @rdname spprmf
#' @export
splgtmf <- function(coef, rho, X, W, method = "lesage_pace") .spbl_impacts(coef, rho, X, W, "logit", method)

#' @rdname spprmf
#' @export
spprprd <- function(coef, X, W, rho = 0.2, method = "marginal") {
  X <- unname(as.matrix(X)) * 1
  n <- nrow(X)
  Si <- solve(diag(n) - rho * (unname(as.matrix(W)) * 1))
  eta <- as.vector(Si %*% (X %*% as.numeric(coef)))
  sd <- switch(method, marginal = sqrt(rowSums(Si^2)), lesage_pace = rep(1, n),
               stop("method must be 'marginal' or 'lesage_pace'"))
  stats::pnorm(eta / sd)
}

#' @rdname spprmf
#' @export
splgtml <- function(y, X, W, Z = NULL) SpatialLogitGmm(y, .spbl_design(X), W, Z)

#' @rdname spprmf
#' @export
spprml <- function(y, X, W, nsim = 9, seed = 0, xtol = 1e-10, maxiter = 20000) {
  y <- as.numeric(y)
  X <- .spbl_design(X)
  W <- unname(as.matrix(W)) * 1
  n <- length(y)
  p <- ncol(X)
  U <- t(vapply(seq_len(nsim) - 1, function(r) .morie_random_uniform(n, seed = seed, stream = r), numeric(n)))
  start <- c(BinaryGlm(y, X, link = "probit")$coefficients, 0)
  o <- .spbl_nm(function(q) -.spbl_ghk(q, y, X, W, matrix(U, nsim)), start, xtol = xtol, maxiter = maxiter)
  list(coefficients = o$par[seq_len(p)], rho = tanh(o$par[p + 1]), loglik = -o$value, iterations = o$iterations,
       nsim = nsim)
}

#' @rdname spprmf
#' @export
sptfx <- function(y, X, W, unit_id, time_id = NULL) {
  s <- .spbl_panel(y, X, W, unit_id, time_id)
  D <- cbind(1, s$X, outer(s$unit, 2:s$N, "==") * 1)
  SpatialProbitGmm(s$y, D, s$W, Z = cbind(D, s$W %*% s$X))
}

#' @rdname spprmf
#' @export
sptrx <- function(y, X, W, unit_id, time_id = NULL) {
  s <- .spbl_panel(y, X, W, unit_id, time_id)
  means <- apply(s$X, 2, function(v) tapply(v, s$unit, mean))
  means <- matrix(means, s$N)
  SpatialProbitGmm(s$y, cbind(1, s$X, means[s$unit, , drop = FALSE]), s$W)
}
