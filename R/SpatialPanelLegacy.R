.spl_stack <- function(y, X, time_id, unit_id) {
  y <- as.numeric(y)
  X <- if (is.null(X)) matrix(0, length(y), 0) else unname(as.matrix(X)) * 1
  if (nrow(X) != length(y)) X <- matrix(X, length(y))
  o <- order(time_id, unit_id)
  N <- length(unique(unit_id))
  T <- length(unique(time_id))
  if (N * T != length(y)) stop("the panel must be balanced: every unit observed in every period")
  keep <- which(apply(X, 2, function(v) length(unique(v)) > 1))
  list(y = y[o], X = X[o, keep, drop = FALSE], N = N, T = T)
}

.spl_kron <- function(W, T) kronecker(diag(T), unname(as.matrix(W)) * 1)

.spl_panel_rs <- function(y, X, W, time_id, unit_id, effects) {
  s <- .spl_stack(y, X, time_id, unit_id)
  yv <- s$y
  Xm <- s$X
  if (effects != "pooled") {
    yv <- .spp_demean(yv, s$N, s$T, effects)
    Xm <- apply(Xm, 2, .spp_demean, N = s$N, T = s$T, effects = effects)
  }
  .lmt_core(yv, matrix(Xm, s$N * s$T), .spl_kron(W, s$T), intercept = effects == "pooled")
}

.spl_q0 <- function(v, N, T) {
  M <- matrix(v, N, T)
  as.vector(M - rowSums(M) / T)
}

.spl_q1 <- function(v, N, T) {
  M <- matrix(v, N, T)
  rep(rowSums(M) / T, T)
}

#' Spatial panel models, tests and resampling
#'
#' Long-format panels are matched to `W` by sorting on (period, unit id);
#' constant columns of `X` are dropped.  `sppfe` (within), `sppsar`,
#' `sppsem`, `sppsdm` are the Elhorst (2003) ML spatial lag, error and Durbin
#' panels (`SpatialPanelMl`); `sppre` the random-effects lag panel
#' (`SpatialPanelReLag`); `sppdyn` the dynamic spatial panel
#' (`SpPanelDynamic`); `sppsac` the SAC panel by coordinate-wise
#' concentrated ML; `sppbe` the between estimator; `sppcov` the residual
#' cross-sectional covariance with Pesaran's CD test; `sppres` Moran's I of
#' stacked residuals with `I_T kron W`; `sppdiag` and `spplm` the panel LM
#' tests (`splm::slmtest`); `spphaus` the Hausman contrast; `sppiv` spatial
#' 2SLS of the fixed-effects lag panel; `sppgmm` the Kapoor-Kelejian-Prucha
#' GM random-effects error panel; `sppboot` a Philox residual bootstrap of
#' the fixed-effects lag panel.
#'
#' @param y Response (long format).
#' @param X Regressors (long format).
#' @param Z Additional instruments (NULL for none).
#' @param W Spatial weights (N by N, units in increasing id order).
#' @param time_id,unit_id Period and unit identifiers.
#' @param effects "individual", "time", "twoways" or "pooled".
#' @param lee_yu Apply the Lee-Yu variance correction.
#' @param space_time_lag Include `W y_{t-1}` in the dynamic panel.
#' @param interval Search interval for the spatial parameters.
#' @param tol Convergence tolerance.
#' @param maxit Maximum number of iterations.
#' @param resid Residuals (long format, or stacked by period for `sppres`).
#' @param alternative Alternative hypothesis for the Moran test.
#' @param robust Robust (adjusted) LM lag test.
#' @param coef_fe,coef_re,vcov_fe,vcov_re Fixed- and random-effects
#'   estimates and covariance matrices.
#' @param B Number of bootstrap replicates.
#' @param seed Philox seed.
#' @return Lists of estimates and statistics.
#' @references Elhorst, J. P. (2003). Specification and estimation of
#'   spatial panel data models. International Regional Science Review 26,
#'   244-268. Elhorst, J. P. (2014). Spatial Econometrics: From
#'   Cross-Sectional Data to Spatial Panels. Springer. Kapoor, M., Kelejian,
#'   H. H. and Prucha, I. R. (2007). Panel data models with spatially
#'   correlated error components. Journal of Econometrics 140, 97-130.
#'   Pesaran, M. H. (2004). General diagnostic tests for cross section
#'   dependence in panels. CESifo Working Paper 1229. Hausman, J. A. (1978).
#'   Specification tests in econometrics. Econometrica 46, 1251-1271.
#'   Anselin, L., Le Gallo, J. and Jayet, H. (2008). Spatial panel
#'   econometrics. In The Econometrics of Panel Data, 3rd ed. Springer.
#'   Baltagi, B. H. (2021). Econometric Analysis of Panel Data, 6th ed.
#'   Springer.
#' @examples
#' W <- rbind(c(0, 0.5, 0, 0.5), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0.5, 0, 0.5, 0))
#' tid <- rep(0:4, each = 4)
#' uid <- rep(0:3, 5)
#' X <- matrix(sin(1.3 * (0:19)) + 0.1 * (0:19))
#' y <- 1 + 0.8 * X[, 1] + 0.3 * cos(2.1 * (0:19)) + 0.2 * ((0:19) %% 4)
#' sppfe(y, X, W, tid, uid)$rho
#' spphaus(c(1, 0.5), c(0.8, 0.6), rbind(c(0.05, 0.01), c(0.01, 0.04)), diag(c(0.02, 0.03)))$statistic
#' @export
sppfe <- function(y, X, W, time_id, unit_id, lee_yu = FALSE) {
  s <- .spl_stack(y, X, time_id, unit_id)
  SpatialPanelMl(s$y, s$X, W, s$N, model = "lag", effects = "individual", lee_yu = lee_yu)
}

#' @rdname sppfe
#' @export
sppsar <- function(y, X, W, time_id, unit_id, effects = "individual", lee_yu = FALSE) {
  s <- .spl_stack(y, X, time_id, unit_id)
  SpatialPanelMl(s$y, s$X, W, s$N, model = "lag", effects = effects, lee_yu = lee_yu)
}

#' @rdname sppfe
#' @export
sppsem <- function(y, X, W, time_id, unit_id, effects = "individual", lee_yu = FALSE) {
  s <- .spl_stack(y, X, time_id, unit_id)
  SpatialPanelMl(s$y, s$X, W, s$N, model = "error", effects = effects, lee_yu = lee_yu)
}

#' @rdname sppfe
#' @export
sppsdm <- function(y, X, W, time_id, unit_id, effects = "individual", lee_yu = FALSE) {
  s <- .spl_stack(y, X, time_id, unit_id)
  SpatialPanelMl(s$y, s$X, W, s$N, model = "durbin", effects = effects, lee_yu = lee_yu)
}

#' @rdname sppfe
#' @export
sppre <- function(y, X, W, time_id, unit_id) {
  s <- .spl_stack(y, X, time_id, unit_id)
  SpatialPanelReLag(s$y, s$X, W, s$N)
}

#' @rdname sppfe
#' @export
sppdyn <- function(y, X, W, time_id, unit_id, space_time_lag = TRUE, effects = "individual") {
  s <- .spl_stack(y, X, time_id, unit_id)
  Y <- matrix(s$y, s$T, s$N, byrow = TRUE)
  Xa <- array(0, c(s$T, s$N, ncol(s$X)))
  for (t in seq_len(s$T)) Xa[t, , ] <- s$X[(t - 1) * s$N + seq_len(s$N), ]
  SpPanelDynamic(Y, Xa, unname(as.matrix(W)) * 1, effects = effects, space_time_lag = space_time_lag)
}

#' @rdname sppfe
#' @export
sppsac <- function(y, X, W, time_id, unit_id, effects = "individual", interval = c(-0.99, 0.99), tol = 1e-13,
                   maxit = 500) {
  s <- .spl_stack(y, X, time_id, unit_id)
  W <- unname(as.matrix(W)) * 1
  N <- s$N
  T <- s$T
  NT <- N * T
  if (effects == "pooled") {
    yt <- s$y
    Xt <- cbind(1, s$X)
  } else {
    yt <- .spp_demean(s$y, N, T, effects)
    Xt <- matrix(apply(s$X, 2, .spp_demean, N = N, T = T, effects = effects), NT)
  }
  wy <- .spp_lag(W, yt, N, T)
  wx <- matrix(apply(Xt, 2, .spp_lag, W = W, N = N, T = T), NT)
  tf <- function(v, lam) v - lam * .spp_lag(W, v, N, T)
  rho_step <- function(lam) {
    bx <- Xt - lam * wx
    e0 <- .spp_ols(bx, tf(yt, lam))$res
    e1 <- .spp_ols(bx, tf(wy, lam))$res
    a <- sum(e0^2)
    b <- sum(e0 * e1)
    c <- sum(e1^2)
    r <- .spp_golden(function(r) -NT / 2 * log(a - 2 * r * b + r * r * c) + T * .spp_ldet(W, r),
                     interval[1], interval[2])
    .spp_polish(function(r) NT * (b - r * c) / (a - 2 * r * b + r * r * c) - T * .spp_trace_aw(W, r), r,
                interval[1], interval[2])
  }
  lam_fit <- function(rho, lam) {
    z <- yt - rho * wy
    wz <- wy - rho * .spp_lag(W, wy, N, T)
    f <- .spp_ols(Xt - lam * wx, z - lam * wz)
    list(beta = f$beta, e = f$res, wz = wz)
  }
  lam_step <- function(rho) {
    f <- function(lam) -NT / 2 * log(sum(lam_fit(rho, lam)$e^2)) + T * .spp_ldet(W, lam)
    df <- function(lam) {
      q <- lam_fit(rho, lam)
      wu <- q$wz - as.vector(wx %*% q$beta)
      NT * sum(q$e * wu) / sum(q$e^2) - T * .spp_trace_aw(W, lam)
    }
    .spp_polish(df, .spp_golden(f, interval[1], interval[2]), interval[1], interval[2])
  }
  rho <- rho_step(0)
  lam <- 0
  for (it in seq_len(maxit)) {
    lam_new <- lam_step(rho)
    rho_new <- rho_step(lam_new)
    done <- abs(rho_new - rho) + abs(lam_new - lam) < tol
    rho <- rho_new
    lam <- lam_new
    if (done) break
  }
  q <- lam_fit(rho, lam)
  s2 <- sum(q$e^2) / NT
  list(rho = rho, lambda = lam, coefficients = q$beta, sigma2 = s2,
       loglik = T * .spp_ldet(W, rho) + T * .spp_ldet(W, lam) - NT / 2 * (log(2 * pi) + log(s2) + 1),
       residuals = q$e, iterations = it)
}

#' @rdname sppfe
#' @export
sppbe <- function(y, X, unit_id) {
  y <- as.numeric(y)
  X <- unname(as.matrix(X)) * 1
  X <- X[, apply(X, 2, function(v) length(unique(v)) > 1), drop = FALSE]
  ids <- sort(unique(unit_id))
  ym <- vapply(ids, function(g) mean(y[unit_id == g]), 0)
  Xm <- cbind(1, t(vapply(ids, function(g) colMeans(X[unit_id == g, , drop = FALSE]), numeric(ncol(X)))))
  if (ncol(X) == 1) Xm <- cbind(1, vapply(ids, function(g) mean(X[unit_id == g, 1]), 0))
  G <- solve(crossprod(Xm))
  b <- as.vector(G %*% crossprod(Xm, ym))
  res <- as.vector(ym - Xm %*% b)
  s2 <- sum(res^2) / (length(ym) - ncol(Xm))
  list(coefficients = b, se = sqrt(s2 * diag(G)), residuals = res, sigma2 = s2, units = ids)
}

#' @rdname sppfe
#' @export
sppcov <- function(resid, unit_id, time_id) {
  units <- sort(unique(unit_id))
  periods <- sort(unique(time_id))
  N <- length(units)
  T <- length(periods)
  E <- matrix(0, N, T)
  E[cbind(match(unit_id, units), match(time_id, periods))] <- as.numeric(resid)
  cv <- tcrossprod(E) / T
  D <- E - rowMeans(E)
  cr <- tcrossprod(D) / sqrt(outer(rowSums(D^2), rowSums(D^2)))
  cd <- sqrt(2 * T / (N * (N - 1))) * sum(cr[upper.tri(cr)])
  list(statistic = cd, p_value = 2 * stats::pnorm(-abs(cd)), covariance = cv, correlation = cr)
}

#' @rdname sppfe
#' @export
sppres <- function(resid, W, alternative = "greater") {
  e <- as.numeric(resid)
  T <- length(e) %/% nrow(as.matrix(W))
  if (T * nrow(as.matrix(W)) != length(e)) stop("len(resid) must be a multiple of the size of W")
  r <- miml(e, .spl_kron(W, T), alternative = alternative)
  r$T <- T
  r
}

#' @rdname sppfe
#' @export
sppdiag <- function(y, X, W, time_id, unit_id, effects = "pooled") {
  c0 <- .spl_panel_rs(y, X, W, time_id, unit_id, effects)
  df <- c(RSerr = 1, RSlag = 1, adjRSerr = 1, adjRSlag = 1, SARMA = 2)
  out <- list()
  for (nm in names(df)) {
    out[[nm]] <- c0[[nm]]
    out[[paste0("p_", nm)]] <- stats::pchisq(c0[[nm]], df[[nm]], lower.tail = FALSE)
  }
  out
}

#' @rdname sppfe
#' @export
spplm <- function(y, X, W, time_id, unit_id, effects = "pooled", robust = FALSE) {
  c0 <- .spl_panel_rs(y, X, W, time_id, unit_id, effects)
  st <- if (robust) c0$adjRSlag else c0$RSlag
  list(statistic = st, p_value = stats::pchisq(st, 1, lower.tail = FALSE))
}

#' @rdname sppfe
#' @export
spphaus <- function(coef_fe, coef_re, vcov_fe, vcov_re) {
  d <- as.numeric(coef_fe) - as.numeric(coef_re)
  h <- sum(d * solve(as.matrix(vcov_fe) - as.matrix(vcov_re), d))
  list(statistic = h, p_value = stats::pchisq(h, length(d), lower.tail = FALSE), df = length(d))
}

#' @rdname sppfe
#' @export
sppiv <- function(y, X, Z, W, time_id, unit_id, effects = "individual") {
  s <- .spl_stack(y, X, time_id, unit_id)
  W <- unname(as.matrix(W)) * 1
  N <- s$N
  T <- s$T
  xc <- s$X
  zc <- if (is.null(Z)) matrix(0, N * T, 0) else .spl_stack(y, Z, time_id, unit_id)$X
  if (effects == "pooled") {
    xc <- cbind(1, xc)
    tr <- function(v) v
  } else {
    tr <- function(v) .spp_demean(v, N, T, effects)
  }
  yt <- tr(s$y)
  xt <- matrix(apply(xc, 2, tr), N * T)
  lagged <- NULL
  for (j in seq_len(ncol(xc))) {
    if (length(unique(xc[, j])) == 1) next
    w1 <- .spp_lag(W, xc[, j], N, T)
    lagged <- cbind(lagged, tr(w1), tr(.spp_lag(W, w1, N, T)))
  }
  H <- cbind(xt, lagged, if (ncol(zc)) matrix(apply(zc, 2, tr), N * T))
  D <- cbind(.spp_lag(W, yt, N, T), xt)
  HtD <- crossprod(H, D)
  F <- t(HtD) %*% solve(crossprod(H))
  A <- solve(F %*% HtD)
  coef <- as.vector(A %*% (F %*% crossprod(H, yt)))
  res <- as.vector(yt - D %*% coef)
  s2 <- sum(res^2) / (N * T)
  list(coefficients = coef, rho = coef[1], se = sqrt(s2 * diag(A)), sigma2 = s2, residuals = res)
}

#' @rdname sppfe
#' @export
sppgmm <- function(y, X, W, time_id, unit_id) {
  s <- .spl_stack(y, X, time_id, unit_id)
  W <- unname(as.matrix(W)) * 1
  N <- s$N
  T <- s$T
  Xa <- cbind(1, s$X)
  u <- .spp_ols(Xa, s$y)$res
  ub <- .spp_lag(W, u, N, T)
  ubb <- .spp_lag(W, ub, N, T)
  d <- N * (T - 1)
  qu <- .spl_q0(u, N, T)
  qub <- .spl_q0(ub, N, T)
  qubb <- .spl_q0(ubb, N, T)
  g <- c(sum(u * qu), sum(ub * qub), sum(ub * qu)) / d
  G <- rbind(c(2 * sum(u * qub) / d, -sum(ub * qub) / d, 1),
             c(2 * sum(ubb * qub) / d, -sum(ubb * qubb) / d, sum(W^2) / N),
             c((sum(u * qubb) + sum(ub * qub)) / d, -sum(ub * qubb) / d, 0))
  fit <- function(lam) {
    r0 <- g - G[, 1] * lam - G[, 2] * lam^2
    s2 <- sum(G[, 3] * r0) / sum(G[, 3]^2)
    list(obj = sum((r0 - G[, 3] * s2)^2), s2 = s2)
  }
  lam <- .spp_golden(function(v) -fit(v)$obj, -0.99, 0.99)
  s2nu <- fit(lam)$s2
  e <- u - lam * ub
  s21 <- sum(e * .spl_q1(e, N, T)) / N
  ys <- s$y - lam * .spp_lag(W, s$y, N, T)
  xs <- Xa - lam * matrix(apply(Xa, 2, .spp_lag, W = W, N = N, T = T), N * T)
  om <- function(v) .spl_q0(v, N, T) / sqrt(s2nu) + .spl_q1(v, N, T) / sqrt(s21)
  beta <- .spp_ols(matrix(apply(xs, 2, om), N * T), om(ys))$beta
  list(lambda = lam, coefficients = beta, sigma2_nu = s2nu, sigma2_1 = s21, sigma2_mu = (s21 - s2nu) / T,
       moment_objective = fit(lam)$obj)
}

#' @rdname sppfe
#' @export
sppboot <- function(y, X, W, time_id, unit_id, B = 9, seed = 0) {
  s <- .spl_stack(y, X, time_id, unit_id)
  W <- unname(as.matrix(W)) * 1
  N <- s$N
  T <- s$T
  NT <- N * T
  f <- SpatialPanelMl(s$y, s$X, W, N, model = "lag", effects = "individual")
  Xt <- matrix(apply(s$X, 2, .spp_demean, N = N, T = T, effects = "individual"), NT)
  ec <- f$residuals - mean(f$residuals)
  Ai <- solve(diag(N) - f$rho * W)
  mu <- as.vector(Xt %*% f$coefficients)
  rd <- numeric(B)
  bd <- matrix(0, B, length(f$coefficients))
  for (b in seq_len(B)) {
    u <- .morie_random_uniform(NT, seed = seed, stream = b - 1)
    es <- ec[pmin(NT - 1, floor(u * NT)) + 1]
    ys <- as.vector(Ai %*% matrix(mu + es, N, T))
    g <- SpatialPanelMl(ys, Xt, W, N, model = "lag", effects = "individual")
    rd[b] <- g$rho
    bd[b, ] <- g$coefficients
  }
  list(rho = f$rho, coefficients = f$coefficients, rho_se = stats::sd(rd), coefficient_se = apply(bd, 2, stats::sd),
       rho_draws = rd, coefficient_draws = bd)
}
