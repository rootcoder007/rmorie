#' Kriging with change of support, nonstationarity and variogram uncertainty
#'
#' \code{AreaToPointKriging}: Kyriakidis area-to-point (downscaling) kriging,
#' coherent with the areal data. \code{NonstationaryKriging}: ordinary
#' kriging with the Paciorek-Schervish nonstationary exponential
#' covariance. \code{EmpiricalVariogramBins}: Matheron's binned sample
#' variogram (gstat defaults). \code{FitVariogramWls}: nugget plus Exp, Gau
#' or Sph variogram by weighted least squares (weights N/h^2).
#' \code{GaKriging}: variogram fitted by a real-coded genetic algorithm,
#' then ordinary kriging. \code{EmpiricalBayesianKriging}: kriging averaged
#' over a likelihood-weighted spectrum of variograms refitted to
#' simulations. Identical to the Python arm \code{morie.fn.krigsupport}.
#'
#' @param values Areal data.
#' @param supports List of matrices discretising each area.
#' @param new_coords Matrix of target points.
#' @param model Covariance model (\code{\link{KrigingCovariance}}), or model
#'   name (Exp, Gau, Sph) for the fitting functions.
#' @param mean Known mean (simple kriging) or NULL.
#' @param z Observations.
#' @param coords Matrix of data coordinates.
#' @param ell,ell_new Kernel scales at the data and at the targets.
#' @param sigma2 Partial sill.
#' @param nugget Nugget variance.
#' @param cutoff,width Variogram cutoff and bin width.
#' @param ev Sample variogram (list with dist, gamma, np).
#' @param pop,generations GA population size and generations.
#' @param nsim Number of simulated variograms.
#' @param seed Philox seed.
#' @return List.
#' @references Kyriakidis, P. C. (2004). A geostatistical framework for
#'   area-to-point spatial interpolation. Geographical Analysis 36, 259-289.
#'
#'   Paciorek, C. J. and Schervish, M. J. (2006). Spatial modelling using a
#'   new class of nonstationary covariance functions. Environmetrics 17,
#'   483-506.
#'
#'   Cressie, N. (1985). Fitting variogram models by weighted least squares.
#'   Mathematical Geology 17, 563-586.
#'
#'   Krivoruchko, K. (2012). Empirical Bayesian kriging. ArcUser, Fall 2012.
#' @examples
#' ev <- list(dist = c(0.5, 1, 1.5, 2), gamma = c(0.45, 0.72, 0.86, 0.93), np = c(10, 12, 14, 9))
#' FitVariogramWls(ev)$range
#' @export
AreaToPointKriging <- function(values, supports, new_coords, model, mean = NULL) {
  z <- as.numeric(values)
  D <- lapply(supports, as.matrix)
  Q <- as.matrix(new_coords)
  K <- length(z)
  C <- matrix(0, K, K)
  for (a in seq_len(K)) {
    for (b in seq_len(K)) {
      s <- 0
      for (i in seq_len(nrow(D[[a]]))) for (j in seq_len(nrow(D[[b]]))) s <- s + KrigingCovariance(.ks_d(D[[a]][i, ], D[[b]][j, ]), model)
      C[a, b] <- s / (nrow(D[[a]]) * nrow(D[[b]]))
    }
  }
  c00 <- KrigingCovariance(0, model)
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  W <- vector("list", nrow(Q))
  for (k in seq_len(nrow(Q))) {
    c0 <- vapply(seq_len(K), function(a) .ks_ss(vapply(seq_len(nrow(D[[a]])), function(i) KrigingCovariance(.ks_d(D[[a]][i, ], Q[k, ]), model), 0)) / nrow(D[[a]]), 0)
    if (is.null(mean)) {
      o <- .ks_ok(z, C, c0, c00)
    } else {
      lam <- as.vector(solve(C) %*% c0)
      o <- list(pred = mean + .ks_ss(lam * (z - mean)), var = c00 - .ks_ss(lam * c0), lam = lam)
    }
    pred[k] <- o$pred
    var[k] <- o$var
    W[[k]] <- o$lam
  }
  list(prediction = pred, variance = var, weights = W)
}

.ks_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.ks_d <- function(a, b) {
  s <- 0
  for (t in seq_along(a)) s <- s + (a[t] - b[t]) * (a[t] - b[t])
  sqrt(s)
}

.ks_ok <- function(z, C, c0, c00) {
  Ci <- solve(C)
  Cic0 <- as.vector(Ci %*% c0)
  one <- rowSums(Ci)
  a <- .ks_ss(one)
  r <- 1 - .ks_ss(Cic0)
  lam <- Cic0 + one * r / a
  list(pred = .ks_ss(lam * z), var = c00 - .ks_ss(c0 * Cic0) + r * r / a, lam = lam)
}

.ks_nscov <- function(a, b, la, lb, sigma2, nugget, d) {
  if (.ks_d(a, b) == 0 && la == lb) return(sigma2 + nugget)
  s <- la * la + lb * lb
  sigma2 * (2 * la * lb / s)^(d / 2) * exp(-sqrt(2 * .ks_d(a, b)^2 / s))
}

#' @rdname AreaToPointKriging
#' @export
NonstationaryKriging <- function(z, coords, new_coords, ell, ell_new, sigma2, nugget = 0) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- length(z)
  d <- ncol(P)
  C <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) C[i, j] <- .ks_nscov(P[i, ], P[j, ], ell[i], ell[j], sigma2, if (i == j) nugget else 0, d)
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    c0 <- vapply(seq_len(n), function(i) .ks_nscov(P[i, ], Q[k, ], ell[i], ell_new[k], sigma2, 0, d), 0)
    o <- .ks_ok(z, C, c0, sigma2 + nugget)
    pred[k] <- o$pred
    var[k] <- o$var
  }
  list(prediction = pred, variance = var)
}

#' @rdname AreaToPointKriging
#' @export
EmpiricalVariogramBins <- function(z, coords, cutoff = NULL, width = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  n <- length(z)
  hh <- numeric(0)
  gg <- numeric(0)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      hh <- c(hh, .ks_d(P[i, ], P[j, ]))
      gg <- c(gg, 0.5 * (z[i] - z[j])^2)
    }
  }
  if (is.null(cutoff)) cutoff <- sqrt(.ks_ss((apply(P, 2, max) - apply(P, 2, min))^2)) / 3
  if (is.null(width)) width <- cutoff / 15
  nb <- as.integer(ceiling(cutoff / width - 1e-12))
  sh <- numeric(nb)
  sg <- numeric(nb)
  cnt <- integer(nb)
  for (t in seq_along(hh)) {
    if (hh[t] > cutoff) next
    b <- min(max(ceiling(hh[t] / width), 1), nb)
    sh[b] <- sh[b] + hh[t]
    sg[b] <- sg[b] + gg[t]
    cnt[b] <- cnt[b] + 1L
  }
  keep <- which(cnt > 0)
  list(dist = sh[keep] / cnt[keep], gamma = sg[keep] / cnt[keep], np = cnt[keep])
}

.ks_rho <- function(h, a, kind) {
  t <- h / a
  switch(kind, Exp = exp(-t), Gau = exp(-t * t), Sph = ifelse(t < 1, 1 - 1.5 * t + 0.5 * t^3, 0),
         stop("model must be Exp, Gau or Sph"))
}

.ks_wls <- function(ev, a, kind) {
  h <- ev$dist
  g <- ev$gamma
  w <- ev$np / h^2
  f <- 1 - .ks_rho(h, a, kind)
  s11 <- .ks_ss(w)
  s12 <- .ks_ss(w * f)
  s22 <- .ks_ss(w * f * f)
  t1 <- .ks_ss(w * g)
  t2 <- .ks_ss(w * f * g)
  det <- s11 * s22 - s12 * s12
  if (det <= 1e-12 * s11 * s22) {
    if (s22 > 0) {
      c0 <- 0
      c1 <- max(t2 / s22, 0)
    } else {
      c0 <- max(t1 / s11, 0)
      c1 <- 0
    }
  } else {
    c0 <- (s22 * t1 - s12 * t2) / det
    c1 <- (s11 * t2 - s12 * t1) / det
  }
  if (c0 < 0) {
    c0 <- 0
    c1 <- max(t2 / s22, 0)
  } else if (c1 < 0) {
    c0 <- max(t1 / s11, 0)
    c1 <- 0
  }
  c(c0, c1, .ks_ss(w * (g - c0 - c1 * f)^2))
}

.ks_sse <- function(ev, c0, c1, a, kind) .ks_ss(ev$np / ev$dist^2 * (ev$gamma - c0 - c1 * (1 - .ks_rho(ev$dist, a, kind)))^2)

#' @rdname AreaToPointKriging
#' @export
FitVariogramWls <- function(ev, model = "Exp") {
  lo <- log(min(ev$dist) / 20)
  hi <- log(max(ev$dist) * 20)
  grid <- lo + (hi - lo) * (0:99) / 99
  ss <- vapply(grid, function(t) .ks_wls(ev, exp(t), model)[3], 0)
  b <- which.min(ss)
  a_ <- grid[max(b - 1, 1)]
  b_ <- grid[min(b + 1, 100)]
  gr <- (sqrt(5) - 1) / 2
  x1 <- b_ - gr * (b_ - a_)
  x2 <- a_ + gr * (b_ - a_)
  f1 <- .ks_wls(ev, exp(x1), model)[3]
  f2 <- .ks_wls(ev, exp(x2), model)[3]
  for (it in 1:200) {
    if (f1 <= f2) {
      b_ <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b_ - gr * (b_ - a_)
      f1 <- .ks_wls(ev, exp(x1), model)[3]
    } else {
      a_ <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a_ + gr * (b_ - a_)
      f2 <- .ks_wls(ev, exp(x2), model)[3]
    }
    if (b_ - a_ < 1e-13) break
  }
  a <- exp(0.5 * (a_ + b_))
  w <- .ks_wls(ev, a, model)
  list(nugget = w[1], psill = w[2], range = a, sse = w[3], model = model)
}

.ks_model <- function(f) list(model = f$model, psill = f$psill, range = f$range, nugget = f$nugget)

#' @rdname AreaToPointKriging
#' @export
GaKriging <- function(z, coords, new_coords, model = "Exp", pop = 40L, generations = 60L, seed = 1, ev = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  if (is.null(ev)) ev <- EmpiricalVariogramBins(z, P)
  gmax <- 2 * max(ev$gamma)
  lo <- c(0, 0, log(min(ev$dist) / 20))
  hi <- c(gmax, gmax, log(max(ev$dist) * 20))
  u <- .morie_random_uniform(pop * 3 + generations * pop * 5, seed = seed)
  nz <- .morie_random_normal(generations * pop * 3, seed = seed, stream = 1)
  iu <- 0
  inn <- 0
  X <- matrix(0, pop, 3)
  for (i in seq_len(pop)) {
    X[i, ] <- lo + (hi - lo) * u[iu + 1:3]
    iu <- iu + 3
  }
  fit <- function(x) .ks_sse(ev, x[1], x[2], exp(x[3]), model)
  Fv <- apply(X, 1, fit)
  for (g in seq_len(generations)) {
    best <- which.min(Fv)
    new <- matrix(0, pop, 3)
    new[1, ] <- X[best, ]
    for (c in 2:pop) {
      par <- matrix(0, 2, 3)
      for (t in 1:2) {
        a <- min(floor(u[iu + 1] * pop), pop - 1) + 1
        b <- min(floor(u[iu + 2] * pop), pop - 1) + 1
        iu <- iu + 2
        par[t, ] <- if (Fv[a] < Fv[b] || (Fv[a] == Fv[b] && a <= b)) X[a, ] else X[b, ]
      }
      w <- u[iu + 1]
      iu <- iu + 1
      child <- numeric(3)
      for (t in 1:3) {
        v <- w * par[1, t] + (1 - w) * par[2, t] + 0.1 * (hi[t] - lo[t]) * nz[inn + 1]
        inn <- inn + 1
        child[t] <- min(max(v, lo[t]), hi[t])
      }
      new[c, ] <- child
    }
    X <- new
    Fv <- apply(X, 1, fit)
  }
  best <- which.min(Fv)
  f <- list(nugget = X[best, 1], psill = X[best, 2], range = exp(X[best, 3]), sse = Fv[best], model = model)
  kr <- Krige(z, P, as.matrix(new_coords), .ks_model(f))
  list(prediction = kr$prediction, variance = kr$variance, fit = f)
}

.ks_loglik <- function(z, P, m) {
  n <- length(z)
  C <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) C[i, j] <- KrigingCovariance(.ks_d(P[i, ], P[j, ]), m)
  L <- .s03chol(C)
  mu <- .ks_ss(z) / n
  y <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0
    if (i > 1) for (k in seq_len(i - 1)) s <- s + L[i, k] * y[k]
    y[i] <- (z[i] - mu - s) / L[i, i]
  }
  -0.5 * n * log(2 * pi) - .ks_ss(log(diag(L))) - 0.5 * .ks_ss(y * y)
}

#' @rdname AreaToPointKriging
#' @export
EmpiricalBayesianKriging <- function(z, coords, new_coords, nsim = 50L, model = "Exp", seed = 1) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- length(z)
  base <- FitVariogramWls(EmpiricalVariogramBins(z, P), model)
  m0 <- .ks_model(base)
  C <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) C[i, j] <- KrigingCovariance(.ks_d(P[i, ], P[j, ]), m0)
  L <- .s03chol(C + diag(1e-10, n))
  mu <- .ks_ss(z) / n
  fits <- list()
  preds <- list()
  varis <- list()
  lls <- numeric(0)
  for (s in seq_len(nsim) - 1) {
    e <- .morie_random_normal(n, seed = seed, stream = s)
    sim <- vapply(seq_len(n), function(i) mu + .ks_ss(L[i, seq_len(i)] * e[seq_len(i)]), 0)
    fs <- FitVariogramWls(EmpiricalVariogramBins(sim, P), model)
    if (fs$psill + fs$nugget <= 0) next
    ms <- .ks_model(fs)
    kr <- Krige(z, P, Q, ms)
    fits[[length(fits) + 1]] <- fs
    preds[[length(preds) + 1]] <- kr$prediction
    varis[[length(varis) + 1]] <- kr$variance
    lls <- c(lls, .ks_loglik(z, P, ms))
  }
  w <- exp(lls - max(lls))
  w <- w / .ks_ss(w)
  pred <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    pk <- vapply(preds, function(p) p[k], 0)
    vk <- vapply(varis, function(v) v[k], 0)
    pred[k] <- .ks_ss(w * pk)
    var[k] <- .ks_ss(w * (vk + pk^2)) - pred[k]^2
  }
  list(prediction = pred, variance = var, weights = w, variograms = fits, base = base)
}
