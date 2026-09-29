.vg_models <- c("Nug", "Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Wav", "Hol", "Cub",
                "Cau", "JBes", "Dmp", "Per", "Cos", "Log", "Pow")
.vg_bounded <- .vg_models[1:17]

.vg_jbes_rho <- function(x, nu) {
  g <- .krs_gauss_legendre(16L)
  m <- ceiling(x) + 8
  h <- pi / m
  th <- as.vector(outer((g$x + 1) / 2, 0:(m - 1), `+`)) * h
  w <- rep(g$w, m)
  gamma(nu + 1) / (sqrt(pi) * gamma(nu + 0.5)) * sum(0.5 * h * w * cos(x * cos(th)) * sin(th)^(2 * nu))
}

.vg_matern_rho <- function(r, k) {
  p <- k - 0.5
  if (p == round(p) && p >= 0 && p <= 20) {
    i <- 0:p
    return(exp(-r) * factorial(p) / factorial(2 * p) *
             colSums(matrix(factorial(p + i) / (factorial(i) * factorial(p - i)), length(i), length(r)) *
                       outer(p - i, 2 * r, function(e, x) x^e)))
  }
  2^(1 - k) / gamma(k) * r^k * besselK(r, k)
}

.vg_unit <- function(h, c) {
  a <- if (is.null(c$range)) 1 else c$range
  k <- if (is.null(c$kappa)) 0.5 else c$kappa
  r <- if (a > 0) h / a else h
  switch(c$model,
    Nug = rep(1, length(h)),
    Exp = 1 - exp(-r),
    Sph = ifelse(r < 1, 1.5 * r - 0.5 * r^3, 1),
    Gau = 1 - exp(-r^2),
    Exc = 1 - exp(-r^k),
    Mat = 1 - .vg_matern_rho(r, k),
    Ste = 1 - .vg_matern_rho(2 * sqrt(k) * r, k),
    Cir = ifelse(r < 1, (2 / pi) * (r * sqrt(pmax(0, 1 - r^2)) + asin(pmin(r, 1))), 1),
    Lin = if (a > 0) pmin(r, 1) else h,
    Bes = 1 - r * besselK(r, 1),
    Pen = ifelse(r < 1, 15 / 8 * r - 5 / 4 * r^3 + 3 / 8 * r^5, 1),
    Wav = 1 - sin(pi * r) / (pi * r),
    Hol = 1 - sin(r) / r,
    Cub = ifelse(r < 1, 7 * r^2 - 35 / 4 * r^3 + 7 / 2 * r^5 - 3 / 4 * r^7, 1),
    Cau = 1 - (1 + r^2)^(-k),
    JBes = 1 - vapply(r, .vg_jbes_rho, 0, nu = k),
    Dmp = 1 - exp(-r) * cos(h / (if (is.null(c$period)) 1 else c$period)),
    Per = 1 - cos(2 * pi * r),
    Cos = 1 - cos(r),
    Log = log(h + a),
    Pow = h^a,
    stop("model must be one of ", paste(.vg_models, collapse = ", "))
  )
}

#' Variogram models (gstat vgm shapes and more)
#'
#' \code{VgmSemivariance}: \eqn{\gamma(0) = 0}, \eqn{\gamma(h) = \sum psill\,g(h)}
#' plus nuggets, with the unit-sill shapes of gstat vgm (Nug, Exp, Sph, Gau,
#' Exc, Mat, Ste, Cir, Lin, Bes, Pen, Per, Wav, Hol, Log, Pow) and Cub
#' (cubic), Cau (Cauchy), JBes (J-Bessel), Dmp (damped oscillation), Cos
#' (Chiles and Delfiner 2012). \code{VgmCovariance} and \code{VgmCorrelogram}
#' give \eqn{C(0) - \gamma(h)} and \eqn{1 - \gamma(h)/C(0)} for bounded models.
#' Identical to the Python arm \code{morie.fn.vgmods}.
#'
#' @param h Distance(s).
#' @param model Component list or list of nested components.
#' @return Numeric vector.
#' @references Chiles, J.-P. and Delfiner, P. (2012). Geostatistics: Modeling
#'   Spatial Uncertainty, 2nd edn. Wiley.
#' @examples
#' VgmSemivariance(0.9, list(model = "Pen", psill = 1, range = 2))
#' VgmCovariance(0.9, list(model = "Cub", psill = 2, range = 2))
#' VgmCorrelogram(1, list(model = "Exp", psill = 3, range = 1))
#' @export
VgmSemivariance <- function(h, model) {
  comps <- .krs_comps(model)
  for (c in comps) if (!c$model %in% .vg_models) stop("model must be one of ", paste(.vg_models, collapse = ", "))
  h <- abs(h)
  g <- Reduce(`+`, lapply(comps, function(c) {
    (if (is.null(c$psill)) 1 else c$psill) * .vg_unit(h, c) + (if (is.null(c$nugget)) 0 else c$nugget)
  }))
  ifelse(h == 0, 0, g)
}

.vg_sill <- function(model) {
  comps <- .krs_comps(model)
  if (any(!vapply(comps, function(c) c$model %in% .vg_bounded, TRUE))) stop("the model is unbounded")
  sum(vapply(comps, function(c) (if (is.null(c$psill)) 1 else c$psill) + (if (is.null(c$nugget)) 0 else c$nugget), 0))
}

#' @rdname VgmSemivariance
#' @export
VgmCovariance <- function(h, model) .vg_sill(model) - VgmSemivariance(h, model)

#' @rdname VgmSemivariance
#' @export
VgmCorrelogram <- function(h, model) 1 - VgmSemivariance(h, model) / .vg_sill(model)

#' Spectral density of Matern, exponential and Gaussian covariances
#'
#' \eqn{f} with \eqn{C(h) = \int e^{i\omega'h} f(\omega)d\omega} in \eqn{R^d}
#' (Stein 1999, section 2.10).
#'
#' @param omega Frequency magnitude.
#' @param model Component list (Exp, Mat with kappa, Gau).
#' @param d Dimension.
#' @return Spectral density.
#' @references Stein, M. L. (1999). Interpolation of Spatial Data. Springer.
#' @examples
#' VgmSpectralDensity(0, list(model = "Exp", psill = 1, range = 2), d = 1)
#' @export
VgmSpectralDensity <- function(omega, model, d = 2) {
  s <- if (is.null(model$psill)) 1 else model$psill
  a <- if (is.null(model$range)) 1 else model$range
  w <- abs(omega)
  if (model$model %in% c("Exp", "Mat")) {
    k <- if (model$model == "Exp" || is.null(model$kappa)) 0.5 else model$kappa
    return(s * gamma(k + d / 2) * a^d / (gamma(k) * pi^(d / 2) * (1 + a^2 * w^2)^(k + d / 2)))
  }
  if (model$model == "Gau") return(s * (a / (2 * sqrt(pi)))^d * exp(-a^2 * w^2 / 4))
  stop("VgmSpectralDensity supports Exp, Mat and Gau")
}

.vg_est <- function(dz, est, zi, zj) {
  N <- length(dz)
  switch(est,
    classical = sum(dz^2) / (2 * N),
    cressie = (sum(sqrt(abs(dz))) / N)^4 / (0.457 + 0.494 / N) / 2,
    mad = 1.099 * stats::median(abs(dz))^2,
    pairwise_relative = sum((dz / ((zi + zj) / 2))^2) / (2 * N),
    relative = sum(dz^2) / (2 * N) / mean(c(zi, zj))^2,
    stop("estimator must be classical, cressie, mad, pairwise_relative or relative")
  )
}

#' Sample variogram (gstat variogram conventions)
#'
#' Pairs binned into \eqn{(b_k, b_{k+1}]} (default cutoff a third of the
#' bounding-box diagonal, width cutoff/15); estimators classical, cressie
#' (Cressie and Hawkins 1980), mad (Dowd 1984), pairwise_relative and
#' relative (Isaaks and Srivastava 1989); covariogram (with gstat's lag-zero
#' row); pseudo cross-variogram against \code{z2} (Myers 1991); indicator
#' variogram of \code{z <= threshold}; directions \code{alpha} (degrees
#' clockwise from north) with tolerance \code{tol_hor}; \code{cloud}.
#'
#' @param z Values.
#' @param coords Two-column matrix.
#' @param cutoff,width,boundaries Distance bins.
#' @param alpha Directions in degrees.
#' @param tol_hor Angular tolerance.
#' @param estimator Estimator name.
#' @param covariogram Estimate the covariogram.
#' @param cloud Return the variogram cloud.
#' @param z2 Second variable (pseudo cross-variogram).
#' @param threshold Indicator threshold.
#' @return List with np, dist, gamma, dir_hor (or the cloud).
#' @references Cressie, N. and Hawkins, D. M. (1980). Robust estimation of
#'   the variogram: I. Mathematical Geology 12, 115-125.
#'
#'   Myers, D. E. (1991). Pseudo-cross variograms, positive-definiteness, and
#'   cokriging. Mathematical Geology 23, 805-816.
#' @examples
#' SampleVariogram(c(1, 2, 4, 3), rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1)), boundaries = c(0,
#'   1.2, 1.5))
#' @export
SampleVariogram <- function(z, coords, cutoff = NULL, width = NULL, boundaries = NULL, alpha = NULL, tol_hor = NULL,
                            estimator = "classical", covariogram = FALSE, cloud = FALSE, z2 = NULL,
                            threshold = NULL) {
  P <- as.matrix(coords)
  n <- length(z)
  if (nrow(P) != n) stop("coords must match z")
  if (!is.null(threshold)) z <- as.numeric(z <= threshold)
  if (is.null(boundaries)) {
    if (is.null(cutoff)) cutoff <- sqrt(diff(range(P[, 1]))^2 + diff(range(P[, 2]))^2) / 3
    if (is.null(width)) width <- cutoff / 15
    B <- (0:floor(cutoff / width + 1e-9)) * width
    if (B[length(B)] < cutoff - 1e-12) B <- c(B, cutoff)
  } else {
    B <- boundaries
  }
  if (is.null(z2)) {
    ij <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
    ij <- ij[order(ij[, 1], ij[, 2]), , drop = FALSE]
  } else {
    ij <- as.matrix(expand.grid(j = seq_len(n), i = seq_len(n))[, 2:1])
    ij <- ij[ij[, 1] != ij[, 2], , drop = FALSE]
  }
  I <- ij[, 1]
  J <- ij[, 2]
  dx <- P[J, 1] - P[I, 1]
  dy <- P[J, 2] - P[I, 2]
  d <- sqrt(dx^2 + dy^2)
  zj <- if (is.null(z2)) z[J] else z2[J]
  if (cloud) return(list(left = I, right = J, dist = d, gamma = 0.5 * (z[I] - zj)^2))
  dirs <- if (is.null(alpha)) NA else alpha
  tol <- if (is.null(tol_hor)) 90 / length(dirs) else tol_hor
  ang <- (atan2(dx, dy) * 180 / pi) %% 180
  zm <- mean(z)
  out <- list(np = numeric(0), dist = numeric(0), gamma = numeric(0), dir_hor = numeric(0))
  for (a in dirs) {
    ok_dir <- if (is.na(a)) rep(TRUE, length(d)) else {
      df <- abs(ang - a %% 180)
      pmin(df, 180 - df) <= tol
    }
    for (k in seq_len(length(B) - 1)) {
      s <- ok_dir & ((d > B[k] & d <= B[k + 1]) | (k == 1 & d == 0 & B[1] == 0))
      if (!any(s)) next
      g <- if (covariogram) mean((z[I[s]] - zm) * (z[J[s]] - zm)) else .vg_est(z[I[s]] - zj[s], estimator, z[I[s]], z[J[s]])
      out$np <- c(out$np, sum(s))
      out$dist <- c(out$dist, mean(d[s]))
      out$gamma <- c(out$gamma, g)
      out$dir_hor <- c(out$dir_hor, if (is.na(a)) 0 else a)
    }
  }
  if (covariogram) {
    out$np <- c(out$np, n)
    out$dist <- c(out$dist, 0)
    out$gamma <- c(out$gamma, mean((z - zm)^2))
    out$dir_hor <- c(out$dir_hor, 0)
  }
  out
}

#' Variogram map
#'
#' Semivariance \eqn{\sum dz^2/(2N)} of ordered pairs by lag cell
#' \eqn{(dx, dy)} of side \code{width}, cells centred at multiples of
#' \code{width} up to \code{cutoff} (Isaaks and Srivastava 1989).
#'
#' @param z Values.
#' @param coords Two-column matrix.
#' @param cutoff Maximum lag per axis.
#' @param width Cell width.
#' @return List with dx, dy, gamma, np (non-empty cells, sorted by dx, dy).
#' @references Isaaks, E. H. and Srivastava, R. M. (1989). An Introduction
#'   to Applied Geostatistics. Oxford University Press.
#' @examples
#' VariogramMap(c(1, 2, 4), rbind(c(0, 0), c(1, 0), c(0, 1)), cutoff = 1, width = 1)
#' @export
VariogramMap <- function(z, coords, cutoff, width) {
  P <- as.matrix(coords)
  n <- length(z)
  K <- floor(cutoff / width + 1e-9)
  ij <- as.matrix(expand.grid(j = seq_len(n), i = seq_len(n))[, 2:1])
  ij <- ij[ij[, 1] != ij[, 2], , drop = FALSE]
  cx <- round((P[ij[, 2], 1] - P[ij[, 1], 1]) / width)
  cy <- round((P[ij[, 2], 2] - P[ij[, 1], 2]) / width)
  keep <- abs(cx) <= K & abs(cy) <= K
  g <- 0.5 * (z[ij[, 1]] - z[ij[, 2]])^2
  key <- paste(cx, cy)[keep]
  s <- tapply(g[keep], key, sum)
  cnt <- tapply(g[keep], key, length)
  xy <- do.call(rbind, lapply(strsplit(names(s), " "), as.numeric))
  o <- order(xy[, 1], xy[, 2])
  list(dx = xy[o, 1] * width, dy = xy[o, 2] * width, gamma = unname(s[o] / cnt[o]), np = unname(as.vector(cnt[o])))
}

.vg_nelder_mead <- function(f, x0, tol = 1e-14, maxit = 5000L) {
  n <- length(x0)
  x0 <- unname(x0)
  S <- unname(rbind(x0, t(vapply(seq_len(n), function(i) x0 + (seq_len(n) == i) * 0.1, numeric(n)))))
  Fv <- apply(S, 1, f)
  for (it in seq_len(maxit)) {
    o <- order(Fv)
    S <- S[o, , drop = FALSE]
    Fv <- Fv[o]
    if (abs(Fv[n + 1] - Fv[1]) <= tol * (abs(Fv[1]) + 1e-300) &&
          max(abs(sweep(S[-1, , drop = FALSE], 2, S[1, ]))) < 1e-10) break
    cc <- colMeans(S[1:n, , drop = FALSE])
    xr <- cc + (cc - S[n + 1, ])
    fr <- f(xr)
    if (fr < Fv[1]) {
      xe <- cc + 2 * (cc - S[n + 1, ])
      fe <- f(xe)
      if (fe < fr) {
        S[n + 1, ] <- xe
        Fv[n + 1] <- fe
      } else {
        S[n + 1, ] <- xr
        Fv[n + 1] <- fr
      }
    } else if (fr < Fv[n]) {
      S[n + 1, ] <- xr
      Fv[n + 1] <- fr
    } else {
      xc <- cc + 0.5 * (S[n + 1, ] - cc)
      fc <- f(xc)
      if (fc < Fv[n + 1]) {
        S[n + 1, ] <- xc
        Fv[n + 1] <- fc
      } else {
        for (i in 2:(n + 1)) S[i, ] <- S[1, ] + 0.5 * (S[i, ] - S[1, ])
        Fv[-1] <- apply(S[-1, , drop = FALSE], 1, f)
      }
    }
  }
  S[which.min(Fv), ]
}

#' Weighted least squares variogram fitting (gstat fit.variogram weights)
#'
#' Weights: method 1 \eqn{N_k}, 6 unweighted, 7 \eqn{N_k/h_k^2}; sills solved
#' exactly for given ranges, log ranges by Nelder-Mead. Method 2 minimises
#' Cressie's criterion \eqn{\sum N_k(\gamma_k - \gamma(h_k))^2/\gamma(h_k)^2}
#' jointly (gstat instead iterates reweighted fits) (Cressie 1985).
#'
#' @param sample Output of \code{\link{SampleVariogram}}.
#' @param model Initial component(s).
#' @param method 1, 2, 6 or 7.
#' @param fit_ranges Fit the ranges.
#' @return List with fitted \code{model} and \code{sse}.
#' @references Cressie, N. (1985). Fitting variogram models by weighted least
#'   squares. Mathematical Geology 17, 563-586.
#' @examples
#' sv <- list(np = c(10, 12, 9), dist = c(0.5, 1, 1.5), gamma = c(0.4, 0.63, 0.78))
#' FitVariogram(sv, list(model = "Exp", psill = 1, range = 1), fit_ranges = FALSE)$model[[1]]$psill
#' @export
FitVariogram <- function(sample, model, method = 7L, fit_ranges = TRUE) {
  comps <- .krs_comps(model)
  if (!method %in% c(1, 2, 6, 7)) stop("method must be 1, 2, 6 or 7")
  nr <- which(vapply(comps, function(c) c$model != "Nug", TRUE))
  h <- sample$dist
  g <- sample$gamma
  N <- sample$np
  if (method == 2) {
    start <- FitVariogram(sample, comps, 7L, fit_ranges)$model
    build <- function(v) {
      cs <- start
      for (i in seq_along(cs)) cs[[i]]$psill <- v[i]
      if (fit_ranges) for (t in seq_along(nr)) cs[[nr[t]]]$range <- exp(v[length(cs) + t])
      cs
    }
    crit <- function(v) {
      e <- VgmSemivariance(h, build(v))
      if (min(e) <= 0) return(Inf)
      sum(N * (g - e)^2 / e^2)
    }
    x <- .vg_nelder_mead(crit, c(vapply(start, `[[`, 0, "psill"),
                                 if (fit_ranges) log(vapply(start[nr], `[[`, 0, "range"))))
    return(list(model = build(x), sse = crit(x)))
  }
  w <- switch(as.character(method), "1" = N, "6" = rep(1, length(N)), "7" = N / h^2)
  solve_s <- function(lr) {
    cs <- comps
    for (t in seq_along(nr)) cs[[nr[t]]]$range <- exp(lr[t])
    Fm <- vapply(cs, function(c) if (c$model == "Nug") rep(1, length(h)) else .vg_unit(h, c), numeric(length(h)))
    Fm <- matrix(Fm, length(h))
    s <- as.vector(solve(t(Fm) %*% (w * Fm), t(Fm) %*% (w * g)))
    for (i in seq_along(cs)) cs[[i]]$psill <- s[i]
    list(model = cs, sse = sum(w * (g - as.vector(Fm %*% s))^2))
  }
  x <- log(vapply(comps[nr], function(c) if (is.null(c$range)) 1 else c$range, 0))
  if (fit_ranges && length(x)) x <- .vg_nelder_mead(function(v) solve_s(v)$sse, x)
  solve_s(x)
}

.vg_profile <- function(z, P, X, shape, phi, nu2, reml) {
  n <- length(z)
  p <- ncol(X)
  c <- shape
  c$range <- phi
  R <- 1 - matrix(.vg_unit(as.vector(as.matrix(stats::dist(P))), c), n)
  diag(R) <- 1 + nu2
  L <- chol(R)
  Ri <- chol2inv(L)
  A <- t(X) %*% Ri %*% X
  b <- as.vector(solve(A, t(X) %*% Ri %*% z))
  e <- z - as.vector(X %*% b)
  m <- if (reml) n - p else n
  s2 <- as.numeric(t(e) %*% Ri %*% e) / m
  ll <- -0.5 * (m * log(2 * pi) + 2 * sum(log(diag(L))) + m * log(s2) + m)
  if (reml) ll <- ll + sum(log(diag(chol(crossprod(X))))) - sum(log(diag(chol(A))))
  list(ll = ll, s2 = s2, beta = b)
}

#' Gaussian log-likelihood and ML/REML fitting of a covariance model (geoR likfit)
#'
#' \code{VariogramLoglik}: log-likelihood of \eqn{\Sigma = psill\,R(range) +
#' nugget\,I} with the GLS trend (REML adds \eqn{(\log|X'X| - \log|X'\Sigma^{-1}X|)/2},
#' Harville 1974, as geoR).
#' \code{Likfit}: \eqn{\beta, \sigma^2} profiled out, \eqn{(\log\phi,
#' \log\tau^2/\sigma^2)} by Nelder-Mead (Mardia and Marshall 1984; Diggle and
#' Ribeiro 2007). \code{SelectVariogramModel}: smallest AIC.
#'
#' @param z Values.
#' @param coords Two-column matrix.
#' @param model Component list (psill, range, kappa as used).
#' @param nugget Nugget (fixed value or starting value).
#' @param X Trend design (default intercept).
#' @param method \code{"ML"} or \code{"REML"}.
#' @param fix_nugget Hold the nugget fixed.
#' @param models List of candidate components.
#' @return Log-likelihood, or a list of estimates with loglik, npars, AIC, BIC.
#' @references Mardia, K. V. and Marshall, R. J. (1984). Maximum likelihood
#'   estimation of models for residual covariance in spatial regression.
#'   Biometrika 71, 135-146.
#'
#'   Diggle, P. J. and Ribeiro, P. J. (2007). Model-based Geostatistics.
#'   Springer.
#' @examples
#' VariogramLoglik(c(0.5, -0.2, 0.9), rbind(c(0, 0), c(1, 0), c(0, 1)),
#'                 list(model = "Exp", psill = 1, range = 1), nugget = 0.1)
#' @export
VariogramLoglik <- function(z, coords, model, nugget = 0, X = NULL, method = "ML") {
  if (!method %in% c("ML", "REML")) stop("method must be ML or REML")
  P <- as.matrix(coords)
  n <- length(z)
  X <- if (is.null(X)) matrix(1, n, 1) else as.matrix(X)
  ps <- if (is.null(model$psill)) 1 else model$psill
  c <- model
  c$psill <- 1
  S <- ps * (1 - matrix(.vg_unit(as.vector(as.matrix(stats::dist(P))), c), n))
  diag(S) <- ps + nugget
  Si <- solve(S)
  A <- t(X) %*% Si %*% X
  b <- as.vector(solve(A, t(X) %*% Si %*% z))
  e <- z - as.vector(X %*% b)
  m <- if (method == "REML") n - ncol(X) else n
  ll <- -0.5 * (m * log(2 * pi) + 2 * sum(log(diag(chol(S)))) + as.numeric(t(e) %*% Si %*% e))
  if (method == "REML") ll <- ll + sum(log(diag(chol(crossprod(X))))) - sum(log(diag(chol(A))))
  ll
}

#' @rdname VariogramLoglik
#' @export
Likfit <- function(z, coords, model, X = NULL, method = "ML", fix_nugget = FALSE, nugget = 0) {
  if (!method %in% c("ML", "REML")) stop("method must be ML or REML")
  P <- as.matrix(coords)
  n <- length(z)
  X <- if (is.null(X)) matrix(1, n, 1) else as.matrix(X)
  reml <- method == "REML"
  shape <- model[setdiff(names(model), c("psill", "range", "nugget"))]
  var0 <- mean((z - mean(z))^2)
  r0 <- if (is.null(model$range)) 1 else model$range
  if (!fix_nugget) {
    neg <- function(v) tryCatch(-.vg_profile(z, P, X, shape, exp(v[1]), exp(v[2]), reml)$ll, error = function(e) Inf)
    x <- .vg_nelder_mead(neg, c(log(r0), log(max(nugget, 1e-3 * var0) / var0)))
    pr <- .vg_profile(z, P, X, shape, exp(x[1]), exp(x[2]), reml)
    out <- list(psill = pr$s2, range = exp(x[1]), nugget = pr$s2 * exp(x[2]), beta = pr$beta, loglik = pr$ll)
    k <- ncol(X) + 3
  } else {
    negf <- function(v) {
      tryCatch(-VariogramLoglik(z, P, c(shape, list(psill = exp(v[2]), range = exp(v[1]))), nugget, X, method),
               error = function(e) Inf)
    }
    x <- .vg_nelder_mead(negf, c(log(r0), log(var0)))
    out <- list(psill = exp(x[2]), range = exp(x[1]), nugget = nugget,
                beta = .vg_profile(z, P, X, shape, exp(x[1]), nugget / exp(x[2]), reml)$beta, loglik = -negf(x))
    k <- ncol(X) + 2
  }
  c(out, list(npars = k, AIC = -2 * out$loglik + 2 * k, BIC = -2 * out$loglik + k * log(n)))
}

#' @rdname VariogramLoglik
#' @export
SelectVariogramModel <- function(z, coords, models, X = NULL, method = "ML") {
  fits <- lapply(models, function(m) Likfit(z, coords, m, X = X, method = method, nugget = 0.1))
  aic <- vapply(fits, `[[`, 0, "AIC")
  list(fits = fits, AIC = aic, best = which.min(aic))
}
