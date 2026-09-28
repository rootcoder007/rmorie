.kf_cov <- function(A, B, model) {
  D <- sqrt(Reduce(`+`, lapply(seq_len(ncol(A)), function(k) outer(A[, k], B[, k], "-")^2)))
  matrix(KrigingCovariance(D, model), nrow(A))
}

#' Kriging filters, indicator ccdf, kriging efficiency and collocated co-kriging
#'
#' \code{FilteredKrige}: kriging of the error-free signal from data with
#' white measurement error of variance \code{error_variance} (gstat's
#' \code{Err} component): the error enters only the data covariance, so
#' predictions at data locations smooth. \code{IndicatorCcdf}: ordinary
#' kriging of indicators at several thresholds (one model for all is median
#' indicator kriging), clipped and order-corrected by averaging upward and
#' downward passes (GSLIB), with E-type mean and variance (uniform within
#' classes). \code{KrigingEfficiency}: \eqn{KE = (BV - KV)/BV} and the slope
#' of regression \eqn{(BV - KV + |\mu|)/(BV - KV + 2|\mu|)} of ordinary
#' (block) kriging (Krige 1996). \code{CollocatedCokriging}: simple
#' co-kriging with the collocated secondary value under Markov model 1
#' (Xu et al. 1992). Identical to the Python arm \code{morie.fn.krigfilt}.
#'
#' @param z Observations.
#' @param coords,new_coords Data and target coordinates.
#' @param model Covariance model (as \code{KrigingCovariance}), or a list of
#'   models per threshold for \code{IndicatorCcdf}.
#' @param error_variance Measurement-error variance.
#' @param X,X0 Trend designs (universal kriging).
#' @param beta Known mean (simple kriging).
#' @param thresholds Increasing thresholds.
#' @param models Indicator covariance model(s).
#' @param zmin,zmax Tail bounds of the ccdf.
#' @param block Block discretisation (as \code{Krige}).
#' @param y0 Secondary values at the targets.
#' @param rho Correlation of primary and secondary.
#' @param mean_z,mean_y,var_y Means and secondary variance.
#' @return List.
#' @references Cressie, N. (1993). Statistics for Spatial Data, rev. edn.
#'   Wiley.
#'
#'   Deutsch, C. V. and Journel, A. G. (1998). GSLIB: Geostatistical Software
#'   Library and User's Guide, 2nd edn. Oxford University Press.
#'
#'   Krige, D. G. (1996). A practical analysis of the effects of spatial
#'   structure and of data available and accessed, on conditional biases in
#'   ordinary kriging. Geostatistics Wollongong '96, 799-810.
#'
#'   Xu, W., Tran, T. T., Srivastava, R. M. and Journel, A. G. (1992).
#'   Integrating seismic data in reservoir modeling: the collocated cokriging
#'   alternative. SPE 24742.
#' @examples
#' m <- list(model = "Exp", psill = 1, range = 2)
#' P <- rbind(c(0, 0), c(2, 0), c(0, 2))
#' FilteredKrige(c(1, 3, 2), P, rbind(c(0, 0)), m, 0.2)$prediction
#' KrigingEfficiency(c(1, 3, 2), P, rbind(c(1, 1)), m)$efficiency
#' @export
FilteredKrige <- function(z, coords, new_coords, model, error_variance, X = NULL, X0 = NULL, beta = NULL) {
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  C <- .kf_cov(P, P, model) + diag(error_variance, nrow(P))
  c0 <- .kf_cov(Q, P, model)
  r <- .stg_krige(C, c0, rep(KrigingCovariance(0, model), nrow(Q)), as.numeric(z),
                  if (is.null(X)) NULL else as.matrix(X), if (is.null(X0)) NULL else as.matrix(X0), beta)
  list(prediction = r$prediction, variance = r$variance, weights = r$weights, beta = r$beta)
}

#' @rdname FilteredKrige
#' @export
IndicatorCcdf <- function(z, coords, new_coords, thresholds, models, zmin = NULL, zmax = NULL) {
  z <- as.numeric(z)
  T <- as.numeric(thresholds)
  if (any(diff(T) <= 0)) stop("thresholds must increase", call. = FALSE)
  ms <- if (!is.null(models$model)) rep(list(models), length(T)) else models
  lo <- if (is.null(zmin)) min(z) else zmin
  hi <- if (is.null(zmax)) max(z) else zmax
  raw <- sapply(seq_along(T), function(k) Krige(as.numeric(z <= T[k]), coords, new_coords, ms[[k]])$prediction)
  raw <- matrix(raw, ncol = length(T))
  K <- length(T)
  cuts <- c(lo, T, hi)
  mid <- (cuts[-1] + cuts[-length(cuts)]) / 2
  wid <- diff(cuts)
  res <- lapply(seq_len(nrow(raw)), function(q) {
    F <- pmin(1, pmax(0, raw[q, ]))
    up <- cummax(F)
    dn <- rev(cummin(rev(F)))
    G <- (up + dn) / 2
    p <- c(G[1], diff(G), 1 - G[K])
    mu <- sum(p * mid)
    list(G = G, mu = mu, v = sum(p * (mid^2 + wid^2 / 12)) - mu^2)
  })
  list(raw = raw, ccdf = t(vapply(res, `[[`, numeric(K), "G")), etype = vapply(res, `[[`, 0, "mu"),
       conditional_variance = vapply(res, `[[`, 0, "v"), thresholds = T)
}

#' @rdname FilteredKrige
#' @export
KrigingEfficiency <- function(z, coords, new_coords, model, block = NULL) {
  r <- Krige(z, coords, new_coords, model, block = block)
  if (is.null(block)) {
    bv <- rep(KrigingCovariance(0, model), length(r$prediction))
  } else {
    offs <- if (is.list(block) && !is.null(block$offsets)) as.matrix(block$offsets) else as.matrix(block)
    bw <- if (is.list(block) && !is.null(block$weights)) block$weights else rep(1 / nrow(offs), nrow(offs))
    comps <- if (!is.null(model$model)) list(model) else model
    sig <- Filter(function(c) c$model != "Nug", comps)
    sig <- lapply(sig, function(c) {
      c$nugget <- 0
      c
    })
    bbv <- sum(outer(bw, bw) * .kf_cov(offs, offs, sig))
    bv <- rep(bbv, length(r$prediction))
  }
  mu <- abs(vapply(r$lagrange, `[`, 0, 1))
  list(efficiency = (bv - r$variance) / bv, slope = (bv - r$variance + mu) / (bv - r$variance + 2 * mu),
       block_variance = bv, kriging_variance = r$variance, lagrange = mu)
}

#' @rdname FilteredKrige
#' @export
CollocatedCokriging <- function(z, coords, y0, new_coords, model, rho, mean_z = 0, mean_y = 0, var_y = 1) {
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  n <- nrow(P)
  c00 <- KrigingCovariance(0, model)
  s <- rho * sqrt(c00 * var_y) / c00
  C <- .kf_cov(P, P, model)
  out <- lapply(seq_len(nrow(Q)), function(k) {
    c0 <- as.vector(.kf_cov(Q[k, , drop = FALSE], P, model))
    A <- rbind(cbind(C, s * c0), c(s * c0, var_y))
    w <- solve(A, c(c0, s * c00))
    lam <- w[1:n]
    nu <- w[n + 1]
    list(p = mean_z + sum(lam * (z - mean_z)) + nu * (y0[k] - mean_y), v = c00 - sum(lam * c0) - nu * s * c00,
         lam = lam, nu = nu)
  })
  list(prediction = vapply(out, `[[`, 0, "p"), variance = vapply(out, `[[`, 0, "v"),
       weights = lapply(out, `[[`, "lam"), nu = vapply(out, `[[`, 0, "nu"))
}
