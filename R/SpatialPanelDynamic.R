# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial panel data models (Elhorst 2014).
# Identical to the Python arm morie.fn.sppaneldyn.

#' Spatial panel models: fixed-effects lag, error and Durbin, random-effects lag, dynamic spatial ARX
#'
#' Data are \code{y} (\code{T x N}, rows are times), \code{X} a \code{T x N x K}
#' array (or nested list \code{X[[t]][[i]]}) and \code{W} (\code{N x N}).
#' \code{SpPanelFe}: within-transformed (\code{effects} "individual", "time" or
#' "twoways") concentrated maximum likelihood: lag model
#' \code{rho} maximising \code{-NT/2 ln((e0 - rho e1)'(e0 - rho e1)) + T ln|I - rho W|}
#' by golden section refined by bisection on the score (log-determinants from
#' the eigenvalues of \code{W}), \code{beta = b0 - rho b1}; "durbin" adds \code{WX};
#' "error" maximises \code{-NT/2 ln(e'e) + T ln|I - lambda W|} with
#' \code{beta} from the spatially filtered data. Standard errors from the
#' inverse information matrix. \code{method = "splm"} lags the demeaned
#' \code{y} instead of demeaning \code{Wy}, as splm::spml.
#' \code{SpPanelRe}: random-effects spatial lag model, \code{phi = sigma_mu^2 / sigma^2}
#' and \code{rho} by L-BFGS-B on the concentrated likelihood with
#' quasi-demeaning (as splm::spreml with \code{errors = "re"}, \code{lag = TRUE}).
#' \code{SpPanelDynamic}: \code{y_(t-1)} and \code{W y_(t-1)} added as regressors
#' of the fixed-effects lag model, conditional on the first period.
#'
#' @param y Response matrix, times by units.
#' @param X Regressor array, times by units by variables.
#' @param W Spatial weights matrix.
#' @param model "lag", "error" or "durbin".
#' @param effects "individual", "time" or "twoways".
#' @param bounds Interval for the spatial parameter.
#' @param tol Golden-section tolerance.
#' @param method "elhorst" or "splm".
#' @param max_iter L-BFGS-B iterations.
#' @param space_time_lag Include \code{W y_(t-1)}.
#' @return A list of estimates.
#' @references Elhorst, J. P. (2014). Spatial Econometrics: From
#'   Cross-Sectional Data to Spatial Panels. Springer. Millo, G. and Piras, G.
#'   (2012). J. Stat. Software 47(1). Yu, J., de Jong, R. and Lee, L.-F. (2008).
#'   J. Econometrics 146, 118-134.
#' @examples
#' W <- rbind(c(0, 1, 0), c(0.5, 0, 0.5), c(0, 1, 0))
#' y <- rbind(c(1, 2, 1.5), c(2, 2.5, 1), c(1.5, 3, 2.5), c(2.5, 2, 3))
#' X <- array(c(0.5, 1.5, 0.4, 1.2, 1, 0.7, 2, 0.3, 0.2, 0.1, 1.1, 1.9), c(4, 3, 1))
#' SpPanelFe(y, X, W)$rho
#' @export
SpPanelFe <- function(y, X, W, model = "lag", effects = "individual", bounds = c(-0.99, 0.99), tol = 1e-8,
                      method = "elhorst") {
  st <- .spp_stack(y, X, W, model == "durbin")
  T_ <- st$T
  N <- st$N
  NT <- T_ * N
  yt <- .spp_demean(st$y, T_, N, effects)
  Xm <- apply(st$X, 2, .spp_demean, T = T_, N = N, effects = effects)
  Xm <- matrix(Xm, NT)
  K <- ncol(Xm)
  wstack <- function(v) as.numeric(W %*% matrix(v, N, T_))
  ev <- as.complex(eigen(W, only.values = TRUE)$values)
  ld <- function(p) .spp_ld(ev, p)
  if (model %in% c("lag", "durbin")) {
    wyt <- if (method == "splm") wstack(yt) else .spp_demean(st$wy, T_, N, effects)
    XtX <- crossprod(Xm)
    b0 <- as.numeric(solve(XtX, crossprod(Xm, yt)))
    b1 <- as.numeric(solve(XtX, crossprod(Xm, wyt)))
    e0 <- yt - as.numeric(Xm %*% b0)
    e1 <- wyt - as.numeric(Xm %*% b1)
    negll <- function(p) 0.5 * NT * log(sum((e0 - p * e1)^2)) - T_ * ld(p)
    score <- function(p) {
      r <- e0 - p * e1
      NT * sum(r * e1) / sum(r * r) - T_ * .spp_tr(ev, p)
    }
    rho <- .spp_refine(score, .spp_golden(negll, bounds[1], bounds[2], tol), bounds[1], bounds[2])
    beta <- b0 - rho * b1
    e <- e0 - rho * e1
    s2 <- sum(e^2) / NT
    Wt <- W %*% solve(diag(N) - rho * W)
    wxb <- .spp_wstack_m(Wt, as.numeric(Xm %*% beta), N, T_)
    info <- matrix(0, K + 2, K + 2)
    info[1:K, 1:K] <- XtX / s2
    info[1:K, K + 1] <- info[K + 1, 1:K] <- as.numeric(crossprod(Xm, wxb)) / s2
    info[K + 1, K + 1] <- T_ * (sum(Wt * t(Wt)) + sum(Wt * Wt)) + sum(wxb^2) / s2
    info[K + 1, K + 2] <- info[K + 2, K + 1] <- T_ * sum(diag(Wt)) / s2
    info[K + 2, K + 2] <- NT / (2 * s2 * s2)
    V <- solve(info)
    return(list(rho = rho, beta = beta, sigma2 = s2, loglik = -0.5 * NT * (log(2 * pi * s2) + 1) + T_ * ld(rho),
                se_beta = sqrt(diag(V)[1:K]), se_rho = sqrt(V[K + 1, K + 1]), residuals = e, model = model,
                effects = effects))
  }
  wy_all <- wstack(yt)
  wX <- apply(Xm, 2, wstack)
  wX <- matrix(wX, NT)
  fit <- function(lam) {
    Xs <- Xm - lam * wX
    ys <- yt - lam * wy_all
    XtX <- crossprod(Xs)
    b <- as.numeric(solve(XtX, crossprod(Xs, ys)))
    list(b = b, e = ys - as.numeric(Xs %*% b), XtX = XtX)
  }
  negll_e <- function(lam) 0.5 * NT * log(sum(fit(lam)$e^2)) - T_ * ld(lam)
  score_e <- function(lam) {
    f <- fit(lam)
    d <- wy_all - as.numeric(wX %*% f$b)
    NT * sum(f$e * d) / sum(f$e^2) - T_ * .spp_tr(ev, lam)
  }
  lam <- .spp_refine(score_e, .spp_golden(negll_e, bounds[1], bounds[2], tol), bounds[1], bounds[2])
  f <- fit(lam)
  s2 <- sum(f$e^2) / NT
  Bt <- W %*% solve(diag(N) - lam * W)
  trB <- sum(diag(Bt))
  trBB <- sum(Bt * t(Bt)) + sum(Bt * Bt)
  Vb <- solve(f$XtX / s2)
  V2 <- solve(rbind(c(T_ * trBB, T_ * trB / s2), c(T_ * trB / s2, NT / (2 * s2 * s2))))
  list(lambda_ = lam, beta = f$b, sigma2 = s2, loglik = -0.5 * NT * (log(2 * pi * s2) + 1) + T_ * ld(lam),
       se_beta = sqrt(diag(Vb)), se_lambda = sqrt(V2[1, 1]), residuals = f$e, model = model, effects = effects)
}

.spp_wstack_m <- function(M, v, N, T_) as.numeric(M %*% matrix(v, N, T_))

.spp_stack <- function(y, X, W, lagx) {
  y <- as.matrix(y)
  T_ <- nrow(y)
  N <- ncol(y)
  if (is.list(X)) {
    K <- length(X[[1]][[1]])
    Xa <- array(0, c(T_, N, K))
    for (t in seq_len(T_)) for (i in seq_len(N)) Xa[t, i, ] <- as.numeric(X[[t]][[i]])
  } else {
    Xa <- X
    K <- dim(X)[3]
  }
  cols <- matrix(0, T_ * N, K)
  for (k in seq_len(K)) cols[, k] <- as.numeric(t(Xa[, , k, drop = FALSE][, , 1]))
  if (lagx) cols <- cbind(cols, apply(cols, 2, function(v) as.numeric(W %*% matrix(v, N, T_))))
  list(T = T_, N = N, y = as.numeric(t(y)), wy = as.numeric(W %*% t(y)), X = cols)
}

.spp_demean <- function(v, T, N, effects) {
  m <- matrix(v, N, T)
  out <- m
  if (effects %in% c("individual", "twoways")) out <- out - rowSums(m) / T
  if (effects %in% c("time", "twoways")) {
    out <- sweep(out, 2, colSums(m) / N)
    if (effects == "twoways") out <- out + sum(v) / (T * N)
  }
  as.numeric(out)
}

.spp_golden <- function(f, lo, hi, tol = 1e-12, max_iter = 300) {
  r <- (sqrt(5) - 1) / 2
  a <- lo
  b <- hi
  cc <- b - r * (b - a)
  d <- a + r * (b - a)
  fc <- f(cc)
  fd <- f(d)
  for (it in seq_len(max_iter)) {
    if (b - a <= tol) break
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

.spp_ld <- function(ev, p) sum(log(Mod(1 - p * ev)))

.spp_tr <- function(ev, p) sum(Re(ev / (1 - p * ev)))

.spp_refine <- function(dfun, x0, lo, hi, width = 1e-6) {
  a <- max(lo, x0 - width)
  b <- min(hi, x0 + width)
  fa <- dfun(a)
  fb <- dfun(b)
  if (fa * fb > 0) return(x0)
  for (it in seq_len(100)) {
    m <- (a + b) / 2
    if (m == a || m == b) break
    fm <- dfun(m)
    if (fm == 0) return(m)
    if (fa * fm < 0) {
      b <- m
      fb <- fm
    } else {
      a <- m
      fa <- fm
    }
  }
  (a + b) / 2
}

#' @rdname SpPanelFe
#' @export
SpPanelRe <- function(y, X, W, bounds = c(-0.99, 0.99), max_iter = 500) {
  st <- .spp_stack(y, X, W, FALSE)
  T_ <- st$T
  N <- st$N
  NT <- T_ * N
  Xr <- cbind(1, st$X)
  qd <- function(v, th) {
    m <- matrix(v, N, T_)
    as.numeric(m - th * rowSums(m) / T_)
  }
  ev <- as.complex(eigen(W, only.values = TRUE)$values)
  conc <- function(par, grad = FALSE) {
    phi <- par[1]
    rho <- par[2]
    th <- 1 - 1 / sqrt(1 + T_ * phi)
    raw <- st$y - rho * st$wy
    ay <- qd(raw, th)
    Xs <- matrix(apply(Xr, 2, qd, th = th), NT)
    b <- as.numeric(solve(crossprod(Xs), crossprod(Xs, ay)))
    e <- ay - as.numeric(Xs %*% b)
    ee <- sum(e^2)
    s2 <- ee / NT
    ll <- -0.5 * NT * (log(2 * pi * s2) + 1) - 0.5 * N * log(1 + T_ * phi) + T_ * .spp_ld(ev, rho)
    if (!grad) return(list(ll = ll, b = b, s2 = s2))
    g_rho <- NT * sum(e * qd(st$wy, th)) / ee - T_ * .spp_tr(ev, rho)
    rr <- matrix(raw - as.numeric(Xr %*% b), N, T_)
    de_dth <- -rep(rowSums(rr) / T_, T_)
    dth <- 0.5 * T_ * (1 + T_ * phi)^-1.5
    g_phi <- -NT * sum(e * de_dth) / ee * dth - 0.5 * N * T_ / (1 + T_ * phi)
    c(g_phi, g_rho)
  }
  res <- LbfgsbMinimize(function(p) -conc(p)$ll, c(0.5, 0), grad = function(p) -conc(p, TRUE), lower = c(0, bounds[1]),
                        upper = c(1e6, bounds[2]), pgtol = 1e-12, factr = 1, max_iter = max_iter)
  x <- res$x
  g <- function(p) -conc(p, TRUE)
  for (it in seq_len(20)) {
    gx <- g(x)
    H <- matrix(0, 2, 2)
    for (a in 1:2) {
      h <- 1e-6 * max(1, abs(x[a]))
      xp <- x
      xm <- x
      xp[a] <- xp[a] + h
      xm[a] <- xm[a] - h
      H[a, ] <- (g(xp) - g(xm)) / (2 * h)
    }
    H <- (H + t(H)) / 2
    det_ <- H[1, 1] * H[2, 2] - H[1, 2] * H[2, 1]
    if (det_ <= 0 || H[1, 1] <= 0) break
    step <- c((H[2, 2] * gx[1] - H[1, 2] * gx[2]) / det_, (H[1, 1] * gx[2] - H[2, 1] * gx[1]) / det_)
    nw <- c(min(max(x[1] - step[1], 0), 1e6), min(max(x[2] - step[2], bounds[1]), bounds[2]))
    done <- max(abs(nw - x) / pmax(1, abs(x))) <= 1e-14
    x <- nw
    if (done) break
  }
  phi <- x[1]
  rho <- x[2]
  cc <- conc(c(phi, rho))
  list(phi = phi, rho = rho, intercept = cc$b[1], beta = cc$b[-1], sigma2 = cc$s2, sigma2_mu = phi * cc$s2,
       loglik = cc$ll)
}

#' @rdname SpPanelFe
#' @export
SpPanelDynamic <- function(y, X, W, effects = "individual", space_time_lag = TRUE, bounds = c(-0.99, 0.99)) {
  y <- as.matrix(y)
  T_ <- nrow(y)
  N <- ncol(y)
  if (is.list(X)) {
    K <- length(X[[1]][[1]])
    Xa <- array(0, c(T_, N, K))
    for (t in seq_len(T_)) for (i in seq_len(N)) Xa[t, i, ] <- as.numeric(X[[t]][[i]])
  } else {
    Xa <- X
    K <- dim(X)[3]
  }
  extra <- if (space_time_lag) 2 else 1
  Xn <- array(0, c(T_ - 1, N, K + extra))
  for (t in 2:T_) {
    Xn[t - 1, , 1] <- y[t - 1, ]
    if (space_time_lag) Xn[t - 1, , 2] <- as.numeric(W %*% y[t - 1, ])
    Xn[t - 1, , extra + seq_len(K)] <- Xa[t, , ]
  }
  SpPanelFe(y[-1, , drop = FALSE], Xn, W, model = "lag", effects = effects, bounds = bounds)
}
