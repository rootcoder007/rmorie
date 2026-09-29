# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial regression extras and the Satorra-Bentler scaled chi-square.
# Identical to the Python arm morie.fn.spregx.

#' Spatial regression extras: Moran permutation test, spautolm, S2SLS lag and Durbin, KP-HET, J-test, Satorra-Bentler
#'
#' \code{MoranPermutationTest}: \code{I = (n / S0) z'Wz / z'z} and the pseudo
#' P-value \code{(1 + #(I_sim >= I)) / (nsim + 1)} over Fisher-Yates
#' permutations on Philox stream \code{k} (as spdep::moran.mc).
#' \code{SpautolmFit}: SAR or CAR error model with case weights by profile
#' likelihood, \code{M = t(I - lambda W) D_w (I - lambda W)} (SAR) or
#' \code{(I - lambda W) D_w} (CAR), as spatialreg::spautolm.
#' \code{S2slsLag}: spatial two-stage least squares with instruments
#' \code{1, X, WX, W^2 X} (and \code{W^3 X} and regressors \code{WX} for the
#' Durbin model), as sphet::spreg(model = "lag"). \code{GmErrorHet}: the
#' heteroskedasticity-robust GM spatial error estimator of Kelejian and
#' Prucha (2010), as sphet::spreg(model = "error", het = TRUE).
#' \code{SpatialJTest}: Kelejian-Piras J-test, the t ratio of the H1
#' prediction in the augmented H0 lag model (\code{method = "sphet"} copies
#' sphet::kpjtest's instrument set). \code{SatorraBentler}:
#' \code{T_ML / c}, \code{c = tr(U Gamma) / df}.
#'
#' @param x Values for Moran's I.
#' @param W Spatial weights matrix.
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @param y Response.
#' @param X Regressor matrix (no intercept column; one is added).
#' @param weights Case weights, or NULL.
#' @param family "SAR" or "CAR".
#' @param intercept Add an intercept column.
#' @param bounds Search interval for the spatial parameter.
#' @param durbin Spatial Durbin model.
#' @param het White-robust variances.
#' @param X0,W0,X1,W1 Regressors and weights of the null and alternative models.
#' @param method "kelejian" or "sphet".
#' @param data Data matrix (rows are cases).
#' @param sigma Model-implied covariance matrix.
#' @param delta Jacobian of vech(Sigma(theta)).
#' @param t_ml Maximum-likelihood chi-square.
#' @param df Degrees of freedom.
#' @return A list.
#' @references Moran, P. A. P. (1950). Biometrika 37, 17-23. Bivand, R. S.,
#'   Pebesma, E. and Gomez-Rubio, V. (2013). Applied Spatial Data Analysis with
#'   R, 2nd ed. Kelejian, H. H. and Prucha, I. R. (1998). J. Real Estate
#'   Finance Econ. 17, 99-121. Kelejian, H. H. and Prucha, I. R. (2010). J.
#'   Econometrics 157, 53-67. Kelejian, H. H. (2008). Letters in Spatial and
#'   Resource Sciences 1, 3-11. Satorra, A. and Bentler, P. M. (1994). In
#'   Latent Variables Analysis, 399-419.
#' @examples
#' W <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
#' MoranPermutationTest(1:4, W, nsim = 19)$statistic
#' @export
MoranPermutationTest <- function(x, W, nsim = 999, seed = 0) {
  x <- as.numeric(x)
  n <- length(x)
  W <- unname(as.matrix(W)) * 1
  s0 <- sum(W)
  moran <- function(v) {
    z <- v - sum(v) / n
    n / s0 * sum(z * as.numeric(W %*% z)) / sum(z * z)
  }
  obs <- moran(x)
  sims <- vapply(seq_len(nsim) - 1, function(k) {
    u <- .morie_random_uniform(n, seed = seed, stream = k)
    p <- x
    for (i in (n - 1):1) {
      j <- floor(u[i + 1] * (i + 1))
      tmp <- p[i + 1]
      p[i + 1] <- p[j + 1]
      p[j + 1] <- tmp
    }
    moran(p)
  }, 0)
  list(statistic = obs, p_value = (1 + sum(sims >= obs)) / (nsim + 1), simulated = sims)
}

.srx_golden <- function(f, lo, hi, tol = 1e-9) {
  r <- (sqrt(5) - 1) / 2
  a <- lo
  b <- hi
  cc <- b - r * (b - a)
  d <- a + r * (b - a)
  fc <- f(cc)
  fd <- f(d)
  while (b - a > tol) {
    if (fc < fd) {
      b <- d
      d <- cc
      fd <- fc
      cc <- b - r * (b - a)
      fc <- f(cc)
    } else {
      a <- cc
      cc <- d
      fc <- fd
      d <- a + r * (b - a)
      fd <- f(d)
    }
  }
  (a + b) / 2
}

.srx_bisect <- function(g, x0, lo, hi, width = 1e-5) {
  a <- max(lo, x0 - width)
  b <- min(hi, x0 + width)
  fa <- g(a)
  fb <- g(b)
  if (fa * fb > 0) return(x0)
  for (it in seq_len(100)) {
    m <- (a + b) / 2
    if (m == a || m == b) break
    fm <- g(m)
    if (fm == 0) return(m)
    if (fa * fm < 0) {
      b <- m
    } else {
      a <- m
      fa <- fm
    }
  }
  (a + b) / 2
}

#' @rdname MoranPermutationTest
#' @export
SpautolmFit <- function(y, X, W, weights = NULL, family = "SAR", intercept = TRUE, bounds = c(-0.99, 0.99)) {
  y <- as.numeric(y)
  n <- length(y)
  Xr <- as.matrix(X)
  if (intercept) Xr <- cbind(1, Xr)
  Xr <- unname(Xr) * 1
  W <- unname(as.matrix(W)) * 1
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  ev <- as.complex(eigen(W, only.values = TRUE)$values)
  slw <- sum(log(w))
  mmat <- function(lam) {
    A <- diag(n) - lam * W
    if (family == "CAR") A %*% diag(w) else crossprod(A, w * A)
  }
  fit <- function(lam) {
    M <- mmat(lam)
    XMX <- crossprod(Xr, M %*% Xr)
    beta <- as.numeric(solve(XMX, crossprod(Xr, M %*% y)))
    e <- y - as.numeric(Xr %*% beta)
    list(beta = beta, e = e, sse = sum(e * as.numeric(M %*% e)), XMX = XMX)
  }
  ll <- function(lam) {
    ld <- sum(log(Mod(1 - lam * ev)))
    det_ <- if (family == "CAR") 0.5 * ld else ld
    det_ + 0.5 * slw - n / 2 * log(2 * pi) - n / 2 * log(fit(lam)$sse / n) - n / 2
  }
  slope <- function(v) {
    f <- fit(v)
    We <- as.numeric(W %*% f$e)
    dsse <- if (family == "CAR") -sum(f$e * We * w) else -2 * sum(We * w * (f$e - v * We))
    dld <- -sum(Re(ev / (1 - v * ev)))
    (if (family == "CAR") 0.5 * dld else dld) - n / 2 * dsse / f$sse
  }
  lam <- .srx_bisect(slope, .srx_golden(function(v) -ll(v), bounds[1], bounds[2]), bounds[1], bounds[2])
  f <- fit(lam)
  s2 <- f$sse / n
  list(lambda_ = lam, beta = f$beta, sigma2 = s2, loglik = ll(lam), se_beta = sqrt(s2 * diag(solve(f$XMX))),
       residuals = f$e, family = family)
}

.srx_tsls <- function(y, Z, H, het = FALSE) {
  bz <- solve(crossprod(H), crossprod(H, Z))
  Zp <- H %*% bz
  Zi <- solve(crossprod(Zp))
  delta <- as.numeric(Zi %*% crossprod(Zp, y))
  e <- y - as.numeric(Z %*% delta)
  s2 <- sum(e^2) / (length(y) - length(delta))
  V <- unname(if (het) Zi %*% crossprod(Zp, Zp * e^2) %*% Zi else Zi * s2)
  list(delta = delta, V = V, e = e, s2 = s2)
}

#' @rdname MoranPermutationTest
#' @export
S2slsLag <- function(y, X, W, durbin = FALSE, het = FALSE) {
  y <- as.numeric(y)
  X <- unname(as.matrix(X)) * 1
  W <- unname(as.matrix(W)) * 1
  wx <- W %*% X
  wwx <- W %*% wx
  if (durbin) {
    Z <- cbind(1, X, wx)
    H <- cbind(1, X, wx, wwx, W %*% wwx)
  } else {
    Z <- cbind(1, X)
    H <- cbind(1, X, wx, wwx)
  }
  Z <- cbind(Z, as.numeric(W %*% y))
  r <- .srx_tsls(y, Z, H, het)
  list(coefficients = r$delta, se = sqrt(diag(r$V)), rho = r$delta[length(r$delta)], residuals = r$e, sigma2 = r$s2,
       vcov = r$V)
}

.srx_gg_het <- function(W, u, n) {
  ub <- as.numeric(W %*% u)
  ubb <- as.numeric(W %*% ub)
  cs <- colSums(W^2)
  first <- sum(ubb * ub) - sum(ub * u * cs)
  second <- sum(ubb^2) - sum(ub^2 * cs)
  third <- sum(u * ubb) + sum(ub^2)
  G <- rbind(c(2 * first / n, -second / n), c(third / n, -sum(ub * ubb) / n))
  list(G = G, g = c((sum(ub^2) - sum(u^2 * cs)) / n, sum(u * ub) / n))
}

.srx_gm_min <- function(gg, P = NULL, lo = -0.9 + .Machine$double.eps, hi = 0.9 - .Machine$double.eps) {
  vfun <- function(r) as.numeric(gg$G %*% c(r, r * r)) - gg$g
  obj <- function(r) {
    v <- vfun(r)
    if (is.null(P)) sum(v^2) else sum(v * (P %*% v))
  }
  slope <- function(r) {
    v <- vfun(r)
    dv <- gg$G[, 1] + 2 * gg$G[, 2] * r
    if (is.null(P)) 2 * sum(v * dv) else 2 * sum(dv * (P %*% v))
  }
  .srx_bisect(slope, .srx_golden(obj, lo, hi, 1e-10), lo, hi)
}

#' @rdname MoranPermutationTest
#' @export
GmErrorHet <- function(y, X, W) {
  y <- as.numeric(y)
  n <- length(y)
  W <- unname(as.matrix(W)) * 1
  H <- cbind(1, unname(as.matrix(X)) * 1)
  k <- ncol(H)
  u0 <- .srx_tsls(y, H, H)$e
  rt <- .srx_gm_min(.srx_gg_het(W, u0, n))
  wZ <- W %*% H
  delta <- .srx_tsls(y - rt * as.numeric(W %*% y), H - rt * wZ, H)$delta
  ut <- y - as.numeric(H %*% delta)
  gg <- .srx_gg_het(W, ut, n)
  A1 <- crossprod(W)
  diag(A1) <- 0
  S1 <- A1 + t(A1)
  S2 <- W + t(W)
  HHi <- solve(crossprod(H) / n)
  psi <- function(rho) {
    Zs <- H - rho * wZ
    eps <- ut - rho * as.numeric(W %*% ut)
    Qhz <- crossprod(H, Zs) / n
    Pm <- HHi %*% Qhz %*% solve(t(Qhz) %*% HHi %*% Qhz)
    Tm <- H %*% Pm
    irw <- t(diag(n) - rho * W)
    al1 <- -as.numeric(crossprod(H, irw %*% (S1 %*% eps))) / n
    al2 <- -as.numeric(crossprod(H, irw %*% (S2 %*% eps))) / n
    a1 <- as.numeric(Tm %*% al1)
    a2 <- as.numeric(Tm %*% al2)
    gam <- eps^2
    tr_ <- function(A, B) sum((A * gam) * t(B * gam)) / 2
    Phi <- matrix(c(tr_(S1, S1) + sum(a1 * gam * a1), tr_(S1, S2) + sum(a1 * gam * a2),
                    tr_(S1, S2) + sum(a1 * gam * a2), tr_(S2, S2) + sum(a2 * gam * a2)), 2) / n
    list(Pinv = solve(Phi), Pm = Pm, a1 = a1, a2 = a2, gam = gam)
  }
  rf <- .srx_gm_min(gg, psi(rt)$Pinv)
  ps <- psi(rf)
  J <- gg$G %*% c(1, 2 * rf)
  om_rr <- 1 / as.numeric(t(J) %*% ps$Pinv %*% J)
  Psidd <- crossprod(H, H * ps$gam) / n
  Psidr <- crossprod(H, cbind(ps$a1, ps$a2) * ps$gam) / n
  Omdd <- t(ps$Pm) %*% Psidd %*% ps$Pm
  Omdr <- as.numeric(t(ps$Pm) %*% Psidr %*% ps$Pinv %*% J) * om_rr
  V <- rbind(cbind(Omdd, Omdr), c(Omdr, om_rr)) / n
  list(beta = delta, rho = rf, rho_initial = rt, vcov = unname(V), se = sqrt(diag(V)), residuals = ut)
}

#' @rdname MoranPermutationTest
#' @export
SpatialJTest <- function(y, X0, W0, X1, W1, het = FALSE, method = "kelejian") {
  y <- as.numeric(y)
  n <- length(y)
  X0 <- unname(as.matrix(X0)) * 1
  X1 <- unname(as.matrix(X1)) * 1
  W0 <- unname(as.matrix(W0)) * 1
  W1 <- unname(as.matrix(W1)) * 1
  alt <- S2slsLag(y, X1, W1)
  yp <- as.numeric(cbind(1, X1, as.numeric(W1 %*% y)) %*% alt$coefficients)
  keep <- vapply(seq_len(ncol(X1)), function(c) !any(vapply(seq_len(ncol(X0)), function(d) identical(X1[, c], X0[, d]), TRUE)), TRUE)
  if (method == "sphet" && (sum(keep) == 0 || all(keep))) keep[] <- FALSE
  w0x0 <- W0 %*% X0
  w1x1 <- W1 %*% X1
  H <- cbind(1, X0, X1[, keep, drop = FALSE], w0x0, W0 %*% w0x0, w1x1, W1 %*% w1x1)
  Z <- cbind(1, X0, as.numeric(W0 %*% y), yp)
  r <- .srx_tsls(y, Z, H, het)
  t_ <- r$delta[length(r$delta)] / sqrt(r$V[length(r$delta), length(r$delta)])
  list(statistic = t_, p_value = 2 * (1 - stats::pnorm(abs(t_))), coefficients = r$delta, se = sqrt(diag(r$V)))
}

#' @rdname MoranPermutationTest
#' @export
SatorraBentler <- function(data, sigma, delta, t_ml, df) {
  X <- unname(as.matrix(data)) * 1
  n <- nrow(X)
  p <- ncol(X)
  idx <- do.call(rbind, lapply(seq_len(p), function(j) cbind(j:p, j)))
  Xc <- sweep(X, 2, colMeans(X))
  D <- Xc[, idx[, 1], drop = FALSE] * Xc[, idx[, 2], drop = FALSE]
  Dc <- sweep(D, 2, colSums(D) / n)
  Gam <- crossprod(Dc) / n
  Si <- solve(as.matrix(sigma))
  q <- nrow(idx)
  V <- matrix(0, q, q)
  for (a in seq_len(q)) for (b in seq_len(q)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    k <- idx[b, 1]
    l <- idx[b, 2]
    v <- 0.5 * (Si[i, k] * Si[j, l] + Si[i, l] * Si[j, k])
    V[a, b] <- 0.5 * (if (i == j) 1 else 2) * (if (k == l) 1 else 2) * v
  }
  Dl <- as.matrix(delta)
  VD <- V %*% Dl
  U <- V - VD %*% solve(t(Dl) %*% VD) %*% t(VD)
  cc <- sum(U * t(Gam)) / df
  list(statistic = t_ml / cc, scaling = cc, gamma = Gam)
}
