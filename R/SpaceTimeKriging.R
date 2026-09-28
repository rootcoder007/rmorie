.stk_comp <- function(d, c) {
  if (is.null(c)) return(0 * d)
  ps <- if (is.null(c$psill)) 1 else c$psill
  nug <- if (is.null(c$nugget)) 0 else c$nugget
  h <- d / (if (is.null(c$range)) 1 else c$range)
  rho <- switch(c$model,
    Exp = exp(-h),
    Gau = exp(-h^2),
    Sph = ifelse(h < 1, 1 - 1.5 * h + 0.5 * h^3, 0),
    Mat = {
      k <- if (is.null(c$kappa)) 0.5 else c$kappa
      ifelse(h > 0, 2^(1 - k) / gamma(k) * h^k * besselK(pmax(h, 1e-300), k), 1)
    },
    Nug = 0 * h,
    stop("component model must be Exp, Gau, Sph, Mat or Nug")
  )
  ifelse(d <= 0, ps + nug, ps * rho)
}

#' Space-time covariance models (gstat vgmST conventions)
#'
#' separable \eqn{sill\,C_s(h)C_t(u)}; productSum \eqn{C_t + C_s + k C_t C_s}
#' (De Cesare et al. 2001); metric \eqn{C_j(\sqrt{h^2 + (\kappa u)^2})};
#' sumMetric \eqn{C_s + C_t + C_j(\sqrt{h^2 + (\kappa u)^2})}. Components
#' are lists with \code{model} (Exp, Gau, Sph, Mat with \code{kappa}, Nug),
#' \code{psill}, \code{range}, \code{nugget}, as gstat \code{vgm} (Graeler,
#' Pebesma and Heuvelink 2016).
#'
#' @param ds Spatial lag(s).
#' @param dt Time lag(s).
#' @param model List with \code{type} and its components.
#' @return Covariance values.
#' @references Graeler, B., Pebesma, E. and Heuvelink, G. (2016).
#'   Spatio-temporal interpolation using gstat. The R Journal 8, 204-218.
#' @examples
#' m <- list(type = "separable", sill = 2, space = list(model = "Exp", range = 1),
#'           time = list(model = "Gau", range = 2))
#' STCovariance(1, 1, m)
#' @export
STCovariance <- function(ds, dt, model) {
  ds <- abs(ds)
  dt <- abs(dt)
  k <- if (is.null(model$stAni)) 1 else model$stAni
  switch(model$type,
    separable = (if (is.null(model$sill)) 1 else model$sill) * .stk_comp(ds, model$space) * .stk_comp(dt, model$time),
    productSum = {
      cs <- .stk_comp(ds, model$space)
      ct <- .stk_comp(dt, model$time)
      ct + cs + (if (is.null(model$k)) 0 else model$k) * ct * cs
    },
    metric = .stk_comp(sqrt(ds^2 + (k * dt)^2), model$joint),
    sumMetric = .stk_comp(ds, model$space) + .stk_comp(dt, model$time) + .stk_comp(sqrt(ds^2 + (k * dt)^2), model$joint),
    stop("type must be separable, productSum, metric or sumMetric")
  )
}

#' Empirical space-time variogram on a full space-time grid
#'
#' For time lag u and spatial bin (b_k, b_k+1], preceded by a zero-distance
#' bin (the same location at two times, empty for u = 0), half the mean
#' squared difference of \eqn{z(s_i, t)} and \eqn{z(s_j, t + u)}, as
#' \code{gstat::variogramST} on an STFDF.
#'
#' @param z Matrix, times by locations.
#' @param coords Two-column matrix of locations.
#' @param times Time values.
#' @param tlags Integer time lags.
#' @param boundaries Spatial bin boundaries.
#' @return Data frame with \code{gamma}, \code{np}, \code{dist},
#'   \code{timelag}, \code{spacelag}.
#' @examples
#' z <- rbind(c(1, 2, 4), c(2, 2.5, 3))
#' STVariogram(z, cbind(0:2, 0), 0:1, tlags = 0:1, boundaries = c(0, 1.5, 3))
#' @export
STVariogram <- function(z, coords, times, tlags, boundaries) {
  Z <- as.matrix(z)
  D <- as.matrix(stats::dist(as.matrix(coords)))
  nt <- nrow(Z)
  ns <- ncol(Z)
  out <- NULL
  for (u in tlags) {
    bins <- c(list(NULL), lapply(seq_len(length(boundaries) - 1), function(k) boundaries[k + 0:1]))
    for (b in bins) {
      acc <- 0
      cnt <- 0
      dsum <- 0
      for (t in seq_len(nt - u)) {
        for (i in seq_len(ns)) {
          for (j in seq_len(ns)) {
            if (u == 0 && j <= i) next
            d <- D[i, j]
            if (if (is.null(b)) d == 0 else (d > b[1] && d <= b[2])) {
              acc <- acc + 0.5 * (Z[t, i] - Z[t + u, j])^2
              dsum <- dsum + d
              cnt <- cnt + 1
            }
          }
        }
      }
      out <- rbind(out, data.frame(gamma = if (cnt) acc / cnt else NaN, np = cnt, dist = if (cnt) dsum / cnt else NaN,
                                   timelag = u, spacelag = if (is.null(b)) 0 else mean(b)))
    }
  }
  out
}

#' Space-time kriging prediction and variance
#'
#' Simple kriging with known mean \code{beta}, else ordinary kriging with the
#' GLS mean; global neighbourhood, as \code{gstat::krigeST} with
#' \code{z ~ 1}.
#'
#' @param z Observed values.
#' @param coords Two-column matrix of observation locations.
#' @param times Observation times.
#' @param new_coords Two-column matrix of target locations.
#' @param new_times Target times.
#' @param model Space-time covariance model (\code{STCovariance}).
#' @param beta Known mean for simple kriging.
#' @return List with \code{prediction}, \code{variance}, \code{mean}.
#' @references Graeler, B., Pebesma, E. and Heuvelink, G. (2016).
#'   Spatio-temporal interpolation using gstat. The R Journal 8, 204-218.
#' @examples
#' m <- list(type = "separable", sill = 1, space = list(model = "Exp", range = 1),
#'           time = list(model = "Exp", range = 2))
#' STKriging(c(1, 2, 1.5), rbind(c(0, 0), c(1, 0), c(0, 1)), c(0, 0, 1), rbind(c(.5, .5)), .5, m)
#' @export
STKriging <- function(z, coords, times, new_coords, new_times, model, beta = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  dsm <- function(A, B) sqrt(outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2)
  C <- STCovariance(dsm(P, P), outer(times, times, "-"), model)
  C0 <- STCovariance(dsm(P, Q), outer(times, new_times, "-"), model)
  Ci <- solve(C)
  one <- rowSums(Ci)
  s1 <- sum(one)
  mu <- if (is.null(beta)) sum(one * z) / s1 else beta
  CiC0 <- Ci %*% C0
  pred <- mu + as.vector(crossprod(C0, Ci %*% (z - mu)))
  v <- STCovariance(0, 0, model) - colSums(C0 * CiC0)
  if (is.null(beta)) v <- v + (1 - colSums(CiC0))^2 / s1
  list(prediction = pred, variance = v, mean = mu)
}

#' Leave-one-out cross-validation of space-time kriging
#'
#' @inheritParams STKriging
#' @return List with \code{prediction}, \code{variance}, \code{residuals},
#'   \code{rmse}, \code{msdr} (mean squared standardised residual).
#' @examples
#' m <- list(type = "metric", stAni = 1, joint = list(model = "Exp", psill = 1, range = 1))
#' STKrigingCV(c(1, 2, 1.5, 1.2), rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1)), c(0, 0, 1, 1), m)$rmse
#' @export
STKrigingCV <- function(z, coords, times, model, beta = NULL) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  n <- length(z)
  pv <- vapply(seq_len(n), function(i) {
    r <- STKriging(z[-i], P[-i, , drop = FALSE], times[-i], P[i, , drop = FALSE], times[i], model, beta)
    c(r$prediction, r$variance)
  }, numeric(2))
  res <- z - pv[1, ]
  list(prediction = pv[1, ], variance = pv[2, ], residuals = res, rmse = sqrt(mean(res^2)),
       msdr = mean(res^2 / pv[2, ]))
}
