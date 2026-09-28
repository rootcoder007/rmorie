.stg_unpack <- function(kind, p, sm, tm, jm) {
  switch(kind,
    separable = list(space = list(psill = 1 - p[2], model = sm, range = p[1], nugget = p[2]),
                     time = list(psill = 1 - p[4], model = tm, range = p[3], nugget = p[4]), sill = p[5]),
    productSum = list(space = list(psill = p[1], model = sm, range = p[2], nugget = p[3]),
                      time = list(psill = p[4], model = tm, range = p[5], nugget = p[6]), k = p[7]),
    metric = list(joint = list(psill = p[1], model = jm, range = p[2], nugget = p[3]), stani = p[4]),
    sumMetric = list(space = list(psill = p[1], model = sm, range = p[2], nugget = p[3]),
                     time = list(psill = p[4], model = tm, range = p[5], nugget = p[6]),
                     joint = list(psill = p[7], model = jm, range = p[8], nugget = p[9]), stani = p[10]),
    stop("model must be separable, productSum, metric or sumMetric", call. = FALSE)
  )
}

#' Fit a space-time variogram model and space-time kriging variants
#'
#' \code{StFit}: weighted least-squares fit of a \code{vgmST} model to an
#' empirical space-time variogram, as \code{gstat::fit.StVariogram}
#' (parameters in \code{extractPar} order, gstat's default bounds, weights
#' \code{fit_method} 1, 2, 6 or 7), by L-BFGS-B, with least-squares AIC and
#' BIC. \code{StUniversalKriging}: kriging with drift \eqn{x'\beta}, as
#' \code{krigeST(z ~ x)}. \code{StLocalKriging}: the \code{ceiling(buffer
#' nmax)} nearest observations in the metric
#' \eqn{\sqrt{dx^2 + dy^2 + (a\,dt)^2}}, of which the \code{nmax} most
#' covarying are kept, as \code{krigeST(nmax, stAni)}.
#' \code{StBlockKriging}: kriging of a space-time block mean (square block,
#' time window, discretised). \code{StLeaveHOut}: buffered cross-validation.
#' \code{StKrigingDiagnostics}: conditioning, weights, Lagrange multiplier,
#' relative variance. \code{StSmoothness}: mean-square differentiability
#' order in space and time. \code{StPredictGrid}: grid prediction per time.
#' \code{StSimulate}: Cholesky simulation, conditioned on data by simple
#' kriging of residuals. Identical to the Python arm \code{morie.fn.stgeo};
#' neighbour indices are 1-based here.
#'
#' @param dist,timelag,gamma,np_ Empirical variogram columns.
#' @param model Model name (\code{StFit}) or \code{STCovariance} model list.
#' @param start Starting parameters.
#' @param fit_method Weighting (1, 2, 6 or 7).
#' @param stani Space-time anisotropy.
#' @param space_model,time_model,joint_model Marginal model families.
#' @param lower,upper Optional bounds.
#' @param z Observed values.
#' @param X,X0 Drift design matrices for data and targets.
#' @param coords,times Observation locations and times.
#' @param new_coords,new_times Target locations and times.
#' @param nmax Neighbourhood size.
#' @param buffer Candidate multiplier.
#' @param beta Known mean (simple kriging).
#' @param block,duration Spatial block side and time window.
#' @param n_space,n_time Block discretisation.
#' @param h,tau Spatial and temporal exclusion radii.
#' @param xs,ys Grid coordinates.
#' @param nsim Number of realisations.
#' @param seed Philox seed.
#' @param mean Field mean.
#' @param data_coords,data_times Conditioning data locations and times.
#' @return List.
#' @references Graeler, B., Pebesma, E. and Heuvelink, G. (2016).
#'   Spatio-temporal interpolation using gstat. The R Journal 8(1), 204-218.
#'
#'   Journel, A. G. and Huijbregts, C. J. (1978). Mining Geostatistics.
#'   Academic Press.
#'
#'   Stein, M. L. (1999). Interpolation of Spatial Data: Some Theory for
#'   Kriging. Springer.
#' @examples
#' m <- list(type = "metric", stAni = 1, joint = list(model = "Exp", psill = 1, range = 1))
#' P <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
#' StBlockKriging(c(1, 2, 1.5, 1.2), P, c(0, 0, 1, 1), rbind(c(0.5, 0.5)), 0.5, m, block = 1, duration = 1)$variance
#' StSmoothness(m)$space
#' @export
StFit <- function(dist, timelag, gamma, np_, model, start, fit_method = 6, stani = NULL, space_model = "Exp",
                  time_model = "Exp", joint_model = "Exp", lower = NULL, upper = NULL) {
  ok <- np_ > 0 & !is.na(gamma)
  H <- dist[ok]
  U <- timelag[ok]
  G <- gamma[ok]
  N <- np_[ok]
  min_s <- min(H[H > 0]) * 0.05
  min_t <- min(H[U > 0]) * 0.05
  pos <- sqrt(.Machine$double.eps)
  lo <- switch(model, separable = c(min_s, 0, min_t, 0, 0), productSum = c(0, min_s, 0, 0, min_t, 0, pos),
               metric = c(0, pos, 0, pos), sumMetric = c(0, min_s, 0, 0, min_t, 0, 0, pos, 0, pos),
               stop("model must be separable, productSum, metric or sumMetric", call. = FALSE))
  hi <- if (model == "separable") c(Inf, 1, Inf, 1, Inf) else rep(Inf, length(lo))
  if (!is.null(lower)) lo <- lower
  if (!is.null(upper)) hi <- upper
  gm <- function(p) {
    a <- .stg_unpack(model, p, space_model, time_model, joint_model)
    StModelVariogram(H, U, model, space = a$space, time = a$time, joint = a$joint, sill = a$sill, k = a$k,
                     stani = a$stani)
  }
  wts <- function(p, g) {
    switch(as.character(fit_method), "1" = N, "2" = N / g^2, "6" = rep(1, length(N)),
           "7" = {
             a <- if (is.null(stani)) .stg_unpack(model, p, space_model, time_model, joint_model)$stani else stani
             if (is.null(a)) a <- 1
             N / (H^2 + (a * U)^2)
           },
           stop("fit_method must be 1, 2, 6 or 7", call. = FALSE))
  }
  obj <- function(p) {
    g <- gm(p)
    mean(wts(p, g) * (G - g)^2)
  }
  res <- LbfgsbMinimize(obj, as.numeric(start), lower = lo, upper = hi, pgtol = 1e-12, factr = 0, max_iter = 2000)
  par <- res$x
  g <- gm(par)
  n <- length(G)
  sse <- obj(par) * n
  list(par = par, model = .stg_unpack(model, par, space_model, time_model, joint_model), objective = obj(par),
       mse = mean((G - g)^2), aic = n * log(sse / n) + 2 * length(par), bic = n * log(sse / n) + length(par) * log(n),
       converged = res$converged, fitted = g)
}

.stg_d <- function(A, B) sqrt(outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2)

.stg_krige <- function(C, c0s, C00s, z, X = NULL, X0 = NULL, beta = NULL) {
  Ci <- solve(C)
  if (!is.null(beta)) {
    X <- NULL
  } else if (is.null(X)) {
    X <- matrix(1, length(z), 1)
    X0 <- matrix(1, nrow(c0s), 1)
  }
  bgls <- NULL
  if (!is.null(X)) {
    CiX <- Ci %*% X
    Ai <- solve(crossprod(X, CiX))
    bgls <- as.vector(Ai %*% crossprod(X, Ci %*% z))
  }
  pr <- numeric(nrow(c0s))
  va <- numeric(nrow(c0s))
  W <- matrix(0, nrow(c0s), length(z))
  for (k in seq_len(nrow(c0s))) {
    c0 <- c0s[k, ]
    Cic0 <- as.vector(Ci %*% c0)
    if (is.null(X)) {
      w <- Cic0
      pr[k] <- beta + sum(w * (z - beta))
      va[k] <- C00s[k] - sum(c0 * Cic0)
    } else {
      d <- X0[k, ] - as.vector(crossprod(X, Cic0))
      lam <- as.vector(Ai %*% d)
      w <- Cic0 + as.vector(CiX %*% lam)
      pr[k] <- sum(w * z)
      va[k] <- C00s[k] - sum(c0 * Cic0) + sum(d * lam)
    }
    W[k, ] <- w
  }
  list(prediction = pr, variance = va, weights = W, beta = bgls)
}

#' @rdname StFit
#' @export
StUniversalKriging <- function(z, X, coords, times, X0, new_coords, new_times, model) {
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  C <- STCovariance(.stg_d(P, P), outer(times, times, "-"), model)
  c0 <- STCovariance(.stg_d(Q, P), outer(new_times, times, "-"), model)
  r <- .stg_krige(C, c0, rep(STCovariance(0, 0, model), nrow(Q)), as.numeric(z), as.matrix(X), as.matrix(X0))
  list(prediction = r$prediction, variance = r$variance, beta = r$beta)
}

#' @rdname StFit
#' @export
StLocalKriging <- function(z, coords, times, new_coords, new_times, model, nmax, stani, buffer = 2, beta = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- length(z)
  m <- min(n, ceiling(buffer * nmax))
  pr <- numeric(nrow(Q))
  va <- numeric(nrow(Q))
  nb <- vector("list", nrow(Q))
  for (k in seq_len(nrow(Q))) {
    d <- sqrt((P[, 1] - Q[k, 1])^2 + (P[, 2] - Q[k, 2])^2 + (stani * (times - new_times[k]))^2)
    cand <- sort(order(d, seq_len(n))[1:m])
    keep <- if (m > nmax) {
      cv <- STCovariance(.stg_d(P[cand, , drop = FALSE], Q[k, , drop = FALSE]), times[cand] - new_times[k], model)
      sort(cand[order(-cv, seq_along(cand))[1:nmax]])
    } else {
      cand
    }
    Pk <- P[keep, , drop = FALSE]
    C <- STCovariance(.stg_d(Pk, Pk), outer(times[keep], times[keep], "-"), model)
    c0 <- matrix(STCovariance(.stg_d(Pk, Q[k, , drop = FALSE]), times[keep] - new_times[k], model), 1)
    r <- .stg_krige(C, c0, STCovariance(0, 0, model), z[keep], beta = beta)
    pr[k] <- r$prediction
    va[k] <- r$variance
    nb[[k]] <- keep
  }
  list(prediction = pr, variance = va, neighbours = nb)
}

#' @rdname StFit
#' @export
StBlockKriging <- function(z, coords, times, new_coords, new_times, model, block, duration, n_space = 4L,
                           n_time = 3L, beta = NULL) {
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  offs <- (-0.5 + (seq_len(n_space) - 0.5) / n_space) * block
  toffs <- (-0.5 + (seq_len(n_time) - 0.5) / n_time) * duration
  C <- STCovariance(.stg_d(P, P), outer(times, times, "-"), model)
  c0s <- matrix(0, nrow(Q), nrow(P))
  C00 <- numeric(nrow(Q))
  for (k in seq_len(nrow(Q))) {
    g <- expand.grid(c = toffs, b = offs, a = offs)
    B <- cbind(Q[k, 1] + g$a, Q[k, 2] + g$b)
    tb <- new_times[k] + g$c
    c0s[k, ] <- rowMeans(STCovariance(.stg_d(P, B), outer(times, tb, "-"), model))
    C00[k] <- mean(STCovariance(.stg_d(B, B), outer(tb, tb, "-"), model))
  }
  r <- .stg_krige(C, c0s, C00, as.numeric(z), beta = beta)
  list(prediction = r$prediction, variance = r$variance, block_variance = C00)
}

#' @rdname StFit
#' @export
StLeaveHOut <- function(z, coords, times, model, h, tau, beta = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  n <- length(z)
  D <- .stg_d(P, P)
  out <- t(vapply(seq_len(n), function(i) {
    keep <- which(D[i, ] > h | abs(times[i] - times) > tau)
    Pk <- P[keep, , drop = FALSE]
    C <- STCovariance(.stg_d(Pk, Pk), outer(times[keep], times[keep], "-"), model)
    c0 <- matrix(STCovariance(D[keep, i], times[keep] - times[i], model), 1)
    r <- .stg_krige(C, c0, STCovariance(0, 0, model), z[keep], beta = beta)
    c(r$prediction, r$variance, length(keep))
  }, numeric(3)))
  res <- z - out[, 1]
  list(prediction = out[, 1], variance = out[, 2], residuals = res, n_used = as.integer(out[, 3]),
       rmse = sqrt(mean(res^2)), msdr = mean(res^2 / out[, 2]))
}

#' @rdname StFit
#' @export
StKrigingDiagnostics <- function(coords, times, new_coords, new_times, model) {
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- nrow(P)
  C <- STCovariance(.stg_d(P, P), outer(times, times, "-"), model)
  ev <- eigen(C, symmetric = TRUE, only.values = TRUE)$values
  Ci <- solve(C)
  s1 <- sum(Ci)
  sill <- STCovariance(0, 0, model)
  c0s <- STCovariance(.stg_d(Q, P), outer(new_times, times, "-"), model)
  res <- lapply(seq_len(nrow(Q)), function(k) {
    Cic0 <- as.vector(Ci %*% c0s[k, ])
    mu <- (1 - sum(Cic0)) / s1
    w <- Cic0 + mu * rowSums(Ci)
    list(w = w, mu = mu, rel = (sill - sum(c0s[k, ] * Cic0) + (1 - sum(Cic0))^2 / s1) / sill)
  })
  W <- lapply(res, `[[`, "w")
  list(condition_number = max(ev) / min(ev), eigenvalues = sort(ev), weights = W,
       weight_sums = vapply(W, sum, 0), negative_share = vapply(W, function(w) mean(w < 0), 0),
       negative_total = vapply(W, function(w) sum(w[w < 0]), 0), lagrange = vapply(res, `[[`, 0, "mu"),
       relative_variance = vapply(res, `[[`, 0, "rel"))
}

.stg_order <- function(c) {
  if (is.null(c)) return(Inf)
  kind <- if (is.null(c$model)) "Exp" else c$model
  switch(kind, Gau = Inf, Mat = ceiling(if (is.null(c$kappa)) 0.5 else c$kappa) - 1, Exp = 0, Sph = 0, Lin = 0, Inf)
}

#' @rdname StFit
#' @export
StSmoothness <- function(model) {
  typ <- model$type
  sp <- list()
  tm <- list()
  if (typ %in% c("separable", "productSum", "sumMetric")) {
    sp <- list(model$space)
    tm <- list(model$time)
  }
  if (typ %in% c("metric", "sumMetric")) {
    sp <- c(sp, list(model$joint))
    tm <- c(tm, list(model$joint))
  }
  ord <- function(cs) {
    if (any(vapply(cs, function(c) !is.null(c$nugget) && c$nugget > 0, TRUE))) return(-1)
    min(vapply(cs, .stg_order, 0))
  }
  os <- ord(sp)
  ot <- ord(tm)
  list(space = os, time = ot, continuous_space = os >= 0, continuous_time = ot >= 0)
}

#' @rdname StFit
#' @export
StPredictGrid <- function(z, coords, times, model, xs, ys, new_times, beta = NULL) {
  P <- as.matrix(coords)
  g <- expand.grid(x = xs, y = ys, t = new_times)
  Q <- cbind(g$x, g$y)
  C <- STCovariance(.stg_d(P, P), outer(times, times, "-"), model)
  c0 <- STCovariance(.stg_d(Q, P), outer(g$t, times, "-"), model)
  r <- .stg_krige(C, c0, rep(STCovariance(0, 0, model), nrow(Q)), as.numeric(z), beta = beta)
  nx <- length(xs)
  ny <- length(ys)
  fr <- lapply(seq_along(new_times), function(t) matrix(r$prediction[(t - 1) * nx * ny + seq_len(nx * ny)], ny, nx, byrow = TRUE))
  vr <- lapply(seq_along(new_times), function(t) matrix(r$variance[(t - 1) * nx * ny + seq_len(nx * ny)], ny, nx, byrow = TRUE))
  list(frames = fr, variance = vr)
}

.stg_chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j] - sum(L[i, seq_len(j - 1)] * L[j, seq_len(j - 1)])
      L[i, j] <- if (i == j) sqrt(max(s, 0)) else if (L[j, j] > 0) s / L[j, j] else 0
    }
  }
  L
}

#' @rdname StFit
#' @export
StSimulate <- function(coords, times, model, nsim = 1L, seed = 1, mean = 0, z = NULL, data_coords = NULL,
                       data_times = NULL) {
  Q <- as.matrix(coords)
  cond <- !is.null(z)
  A <- if (cond) rbind(as.matrix(data_coords), Q) else Q
  at <- if (cond) c(data_times, times) else times
  nd <- if (cond) length(z) else 0
  C <- STCovariance(.stg_d(A, A), outer(at, at, "-"), model)
  L <- .stg_chol(C)
  N <- nrow(A)
  if (cond) lam <- C[nd + seq_len(nrow(Q)), seq_len(nd), drop = FALSE] %*% solve(C[seq_len(nd), seq_len(nd), drop = FALSE])
  out <- lapply(seq_len(nsim) - 1, function(r) {
    x <- mean + as.vector(L %*% .morie_random_normal(N, seed = seed, stream = r))
    if (cond) x[nd + seq_len(nrow(Q))] + as.vector(lam %*% (z - x[seq_len(nd)])) else x
  })
  list(realisations = out, cholesky = L)
}
