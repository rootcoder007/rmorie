# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Semivariogram estimators and kriging diagnostics from Schabenberger &
# Gotway (2005), Statistical Methods for Spatial Data Analysis. Equation
# numbers below are the book's own.
#
# Mirrors the shared `morie.fn._schaben` core. The three functions here
# are the ones R did not already have: `morie_spatial_variogram` covers
# Matheron and `morie_spatial_variogram_fit` covers ML, but the robust
# estimator, the composite likelihood and the Prasad-Rao prediction-error
# correction had no R counterpart.

#' .morie_sb_pairs
#'
#' A step of the schaben_native implementation. Called by \code{morie_cressie_hawkins}, \code{morie_matheron_estimator}, \code{morie_variogram_composite_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{h}, \code{d}.
#' @export
.morie_sb_pairs <- function(coords, z) {
  P <- as.matrix(coords)
  zz <- as.numeric(z)
  n <- length(zz)
  if (nrow(P) != n) P <- t(P)
  if (nrow(P) != n) {
    stop(sprintf("coords has %d rows for %d values.", nrow(P), n),
         call. = FALSE)
  }
  if (n < 2L) stop("need at least 2 locations.", call. = FALSE)
  i <- rep.int(seq_len(n - 1L), (n - 1L):1L)
  j <- unlist(lapply(seq_len(n - 1L), function(k) (k + 1L):n),
              use.names = FALSE)
  list(h = sqrt(rowSums((P[i, , drop = FALSE] - P[j, , drop = FALSE])^2)),
       d = zz[i] - zz[j])
}

#' .morie_sb_groups
#'
#' A step of the schaben_native implementation. Called by \code{morie_cressie_hawkins}, \code{morie_matheron_estimator}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param h A vector; indexed elementwise.
#' @param bins Optional; may be \code{NULL}. A vector; its length is taken.
#' @param cutoff Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param exact A flag; the body branches on it.
#' @return A list with \code{centres}, \code{idx}.
#' @export
.morie_sb_groups <- function(h, bins, cutoff, exact) {
  if (isTRUE(exact)) {
    u <- unique(round(h, 12))
    u <- sort(u)
    return(list(centres = u,
                idx = lapply(u, function(v) which(abs(h - v) < 1e-9))))
  }
  if (!is.null(bins) && length(bins) > 1L) {
    edges <- as.numeric(bins)
  } else {
    k <- if (is.null(bins)) 15L else as.integer(bins)
    top <- if (is.null(cutoff)) max(h) / 2 else as.numeric(cutoff)
    edges <- seq(0, top, length.out = k + 1L)
  }
  centres <- numeric(0); idx <- list()
  for (b in seq_len(length(edges) - 1L)) {
    m <- which(h > edges[b] - 1e-12 & h <= edges[b + 1L] + 1e-12)
    if (length(m)) {
      centres <- c(centres, mean(h[m]))
      idx[[length(idx) + 1L]] <- m
    }
  }
  list(centres = centres, idx = idx)
}

#' Cressie-Hawkins robust semivariogram estimator
#'
#' Implements equation (4.26) of Schabenberger & Gotway (2005), p. 160:
#' \deqn{\hat\gamma(h) = \frac{1}{2}\left\{\frac{1}{|N(h)|}\sum
#'   |Z(s_i)-Z(s_j)|^{1/2}\right\}^4 / \left(0.457 +
#'   \frac{0.494}{|N(h)|}\right).}
#'
#' Averaging the square-root differences BEFORE raising to the fourth
#' power is what limits an outlier's leverage; the denominator restores
#' approximate unbiasedness. Robust here means resistant to slight
#' contamination of a Gaussian field only: the influence function is
#' unbounded and the breakdown point is zero, and on clean data this
#' estimator is the MORE variable of the two.
#'
#' The bias correction is derived on p. 160 as
#' \code{0.457 + 0.494/|N| + 0.045/|N|^2} and then written without the
#' last term in equation (4.26). The printed equation is the default
#' because it is what the book's own worked Example 4.3 evaluates: its
#' factor 0.704 at \code{|N(h)| = 2} is \code{0.457 + 0.494/2} exactly.
#'
#' @param coords Matrix of coordinates, one row per location.
#' @param z Observed values.
#' @param bins Lag-class count, or explicit bin edges.
#' @param cutoff Largest separation used; half the maximum by default.
#' @param exact Treat every distinct separation as its own lag class.
#' @param full_correction Include the \code{0.045/|N(h)|^2} term.
#' @return A list with `lag`, `gamma`, `n_pairs`, `matheron`, `ratio`.
#' @references Schabenberger O, Gotway CA (2005), Sec 4.4.2, eq (4.26),
#'   pp. 159-161. Cressie N, Hawkins DM (1980).
#' @export
morie_cressie_hawkins <- function(coords, z, bins = NULL, cutoff = NULL,
                                  exact = FALSE, full_correction = FALSE) {
  pr <- .morie_sb_pairs(coords, z)
  gr <- .morie_sb_groups(pr$h, bins, cutoff, exact)
  gam <- numeric(length(gr$idx)); np <- integer(length(gr$idx))
  mat <- numeric(length(gr$idx))
  for (b in seq_along(gr$idx)) {
    m <- gr$idx[[b]]
    nh <- length(m)
    root <- mean(sqrt(abs(pr$d[m])))
    denom <- 0.457 + 0.494 / nh
    if (isTRUE(full_correction)) denom <- denom + 0.045 / nh^2
    gam[b] <- 0.5 * root^4 / denom
    np[b] <- nh
    mat[b] <- mean(pr$d[m]^2) / 2
  }
  list(lag = gr$centres, gamma = gam, n_pairs = np, matheron = mat,
       ratio = ifelse(mat > 0, gam / mat, NA_real_),
       full_correction = isTRUE(full_correction), n = length(z),
       method = "Cressie-Hawkins robust semivariogram estimator")
}

#' Matheron classical semivariogram estimator with its (4.25) variance
#'
#' Equation (4.24). Provided alongside [morie_cressie_hawkins()] so the
#' two can be read on identical lag classes; `morie_spatial_variogram`
#' bins differently and is kept for backwards compatibility.
#'
#' @inheritParams morie_cressie_hawkins
#' @return A list with `lag`, `gamma`, `n_pairs`, `variance`,
#'   `sparse_lags`.
#' @references Schabenberger O, Gotway CA (2005), Sec 4.4.1, eq (4.24)
#'   and (4.25), pp. 153-158.
#' @export
morie_matheron_estimator <- function(coords, z, bins = NULL, cutoff = NULL,
                                     exact = FALSE) {
  pr <- .morie_sb_pairs(coords, z)
  gr <- .morie_sb_groups(pr$h, bins, cutoff, exact)
  gam <- vapply(gr$idx, function(m) mean(pr$d[m]^2) / 2, numeric(1))
  np <- vapply(gr$idx, length, integer(1))
  list(lag = gr$centres, gamma = gam, n_pairs = np,
       variance = 2 * gam^2 / np,
       sparse_lags = which(np < 30L) - 1L,
       n = length(z),
       method = "Matheron classical semivariogram estimator")
}

#' .morie_sb_vgm
#'
#' A step of the schaben_native implementation. Called by \code{morie_kriging_pred_error}, \code{morie_variogram_composite_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param h Numeric; combined arithmetically in the body.
#' @param model See Usage.
#' @param nugget Numeric; combined arithmetically in the body.
#' @param psill Numeric; combined arithmetically in the body.
#' @param rng Numeric; passed to \code{max}.
#' @return The value of \code{ifelse}.
#' @export
.morie_sb_vgm <- function(h, model, nugget, psill, rng) {
  a <- max(rng, 1e-12)
  g <- switch(
    model,
    exponential = 1 - exp(-3 * h / a),
    gaussian = 1 - exp(-3 * (h / a)^2),
    spherical = { t <- pmin(pmax(h / a, 0), 1); 1.5 * t - 0.5 * t^3 },
    linear = pmin(h / a, 1),
    stop(sprintf("unknown model %s.", model), call. = FALSE)
  )
  ifelse(h <= 0, 0, nugget + psill * g)
}

#' Composite-likelihood semivariogram estimation
#'
#' Solves the composite likelihood score, equation (4.44) of
#' Schabenberger & Gotway (2005), p. 171:
#' \deqn{CS(\theta) = 2\sum_{i<j}
#'   \frac{\partial\gamma}{\partial\theta}\frac{1}{8\gamma^2}
#'   \{T^{(3)}_{ij} - 2\gamma(h_{ij},\theta)\} = 0,}
#' which differs from the generalised estimating equation (4.43) only by
#' the factor \code{1/(8 gamma^2)}. That factor is not a tuning choice:
#' under the Gaussian assumption \code{T3/(2 gamma)} is chi-square with
#' one degree of freedom, so \code{Var\[T3\] = 8 gamma^2} exactly.
#'
#' The fit is to the semivariogram CLOUD, pair by pair, so no lag
#' classes, tolerance or cutoff can move the answer.
#'
#' Solved as iteratively re-weighted least squares, which is what the
#' book's "Gauss-Newton" instruction means: the weight is held fixed
#' within each pass. Minimising the weighted sum with the weight left
#' free is a different and badly behaved problem, because the fit can
#' then shrink its own weight by inflating gamma.
#'
#' @param coords,z As in [morie_cressie_hawkins()].
#' @param model One of "exponential", "spherical", "gaussian", "linear".
#' @param max_iter,tol Re-weighting controls.
#' @return A list with `nugget`, `psill`, `range`, `sill`, `n_pairs`,
#'   `objective`, `iterations`.
#' @references Schabenberger O, Gotway CA (2005), Sec 4.5.3, eq (4.43)
#'   and (4.44), pp. 169-172. Lindsay BG (1988).
#' @export
morie_variogram_composite_likelihood <- function(coords, z,
                                                 model = "exponential",
                                                 max_iter = 60L,
                                                 tol = 1e-10) {
  pr <- .morie_sb_pairs(coords, z)
  h <- pr$h; t3 <- pr$d^2
  v0 <- stats::var(as.numeric(z))
  theta <- c(max(0.1 * v0, 1e-8), max(0.9 * v0, 1e-8), max(max(h) / 3, 1e-8))
  prev <- NULL; it <- 0L
  for (it in seq_len(max_iter)) {
    g_cur <- pmax(.morie_sb_vgm(h, model, theta[1], theta[2], theta[3]), 1e-12)
    w <- 1 / (8 * g_cur^2)
    obj <- function(p) {
      t <- exp(p)
      g <- .morie_sb_vgm(h, model, t[1], t[2], t[3])
      sum(w * (t3 - 2 * g)^2)
    }
    op <- stats::optim(log(pmax(theta, 1e-10)), obj, method = "Nelder-Mead",
                       control = list(maxit = 2000L, reltol = 1e-12))
    theta <- exp(op$par)
    if (!is.null(prev) && max(abs(theta - prev)) < tol) break
    prev <- theta
  }
  g_fin <- pmax(.morie_sb_vgm(h, model, theta[1], theta[2], theta[3]), 1e-12)
  # A bounded model cannot fit an unbounded variogram. Under a trend in
  # the mean the semivariance keeps climbing (eq 5.35) and the fit answers
  # by pushing the range towards infinity; that is a diagnosis, not a fit.
  hmax <- max(h)
  diverged <- theta[3] > 10 * hmax
  list(nugget = theta[1], psill = theta[2], range = theta[3],
       sill = theta[1] + theta[2], model = model,
       objective = sum((t3 - 2 * g_fin)^2 / (8 * g_fin^2)),
       iterations = it, n_pairs = length(h),
       converged = !diverged,
       diverged_note = if (!diverged) NULL else sprintf(
         paste("the fitted range (%.3g) exceeds ten times the largest",
               "separation (%.3g); no bounded sill was found, usually",
               "because of a trend in the mean (eq 5.35). Detrend first,",
               "or fit the 'linear' model."), theta[3], hmax),
       method = "Composite-likelihood semivariogram estimation")
}

#' Ordinary kriging with a Prasad-Rao corrected prediction error
#'
#' The plug-in kriging variance is not the prediction error of the
#' predictor actually in use. Substituting an estimated \eqn{\theta} into
#' the predictor gives an EBLUP, which is no longer best, while
#' substituting into the variance formula gives the prediction error of a
#' DIFFERENT predictor - the one that would apply had \eqn{\theta} been
#' known. Schabenberger & Gotway are blunt that this "can be
#' substantially biased", and always downward.
#'
#' Kackar & Harville (1984) supply the missing term
#' \eqn{\mathrm{tr}\{A(\theta)B(\theta)\}}; evaluating it at
#' \eqn{\hat\theta} removes only half the bias, so Prasad & Rao's
#' approximately unbiased form, equation (5.53), doubles the correction.
#'
#' @param coords,z As in [morie_cressie_hawkins()].
#' @param target Matrix of prediction locations.
#' @param model Covariance model name.
#' @param nugget,psill,rng Covariance parameters; estimated by weighted
#'   least squares when any is NULL.
#' @param jitter,n_jitter,seed Controls for approximating \eqn{B}.
#' @return A list with `prediction`, `mse`, `mse_plugin`, `correction`,
#'   `se`, `parameters`, `parameters_estimated`.
#' @references Schabenberger O, Gotway CA (2005), Sec 5.5.4, eq
#'   (5.51)-(5.53), pp. 263-266. Kackar RN, Harville DA (1984).
#'   Prasad NGN, Rao JNK (1990).
#' @export
morie_kriging_pred_error <- function(coords, z, target,
                                     model = "exponential",
                                     nugget = NULL, psill = NULL,
                                     rng = NULL, jitter = 0.05,
                                     n_jitter = 24L, seed = 0L) {
  P <- as.matrix(coords); zz <- as.numeric(z); n <- length(zz)
  if (nrow(P) != n) P <- t(P)
  T0 <- as.matrix(target)
  if (ncol(T0) != ncol(P)) T0 <- t(T0)
  estimated <- is.null(nugget) || is.null(psill) || is.null(rng)
  if (estimated) {
    em <- morie_matheron_estimator(P, zz)
    v0 <- stats::var(zz)
    obj <- function(p) {
      t <- exp(p)
      g <- .morie_sb_vgm(em$lag, model, t[1], t[2], t[3])
      sum(em$n_pairs / (2 * pmax(g, 1e-12)^2) * (em$gamma - g)^2)
    }
    op <- stats::optim(log(c(max(0.1 * v0, 1e-8), max(0.9 * v0, 1e-8),
                             max(max(em$lag) / 2, 1e-8))),
                       obj, method = "Nelder-Mead",
                       control = list(maxit = 2000L, reltol = 1e-12))
    th <- exp(op$par)
    nugget <- th[1]; psill <- th[2]; rng <- th[3]
  }
  theta <- c(nugget, psill, rng)
  D <- as.matrix(stats::dist(P))
  d0 <- outer(seq_len(n), seq_len(nrow(T0)), Vectorize(function(i, j) {
    sqrt(sum((P[i, ] - T0[j, ])^2))
  }))
  krige <- function(t) {
    sill <- t[1] + t[2]
    C <- sill - .morie_sb_vgm(D, model, t[1], t[2], t[3])
    C <- C + diag(1e-10 * max(sill, 1e-12), n)
    c0 <- sill - .morie_sb_vgm(d0, model, t[1], t[2], t[3])
    Ci1 <- solve(C, rep(1, n)); Cic <- solve(C, c0)
    den <- sum(Ci1)
    lam <- Cic + outer(Ci1, (1 - colSums(Cic)) / den)
    var <- sill - colSums(c0 * Cic) + (1 - colSums(Cic))^2 / den
    list(lam = lam, var = pmax(var, 0))
  }
  k0 <- krige(theta)
  pred <- as.numeric(crossprod(k0$lam, zz))
  m <- nrow(T0)
  dlam <- array(0, dim = c(3L, n, m))
  for (k in 1:3) {
    step <- max(abs(theta[k]) * 1e-4, 1e-8)
    tp <- theta; tm <- theta
    tp[k] <- tp[k] + step; tm[k] <- max(tm[k] - step, 1e-12)
    dlam[k, , ] <- (krige(tp)$lam - krige(tm)$lam) / (tp[k] - tm[k])
  }
  set.seed(seed)
  draws <- matrix(rep(theta, each = n_jitter), nrow = n_jitter) *
    (1 + jitter * matrix(stats::rnorm(n_jitter * 3L), nrow = n_jitter))
  draws <- pmax(draws, 1e-10)
  cen <- sweep(draws, 2L, theta)
  B <- crossprod(cen) / n_jitter
  sill <- theta[1] + theta[2]
  Cth <- sill - .morie_sb_vgm(D, model, theta[1], theta[2], theta[3])
  Cth <- Cth + diag(1e-10 * max(sill, 1e-12), n)
  corr <- numeric(m)
  for (j in seq_len(m)) {
    G <- dlam[, , j, drop = TRUE]
    if (is.null(dim(G))) G <- matrix(G, nrow = 3L)
    A <- G %*% Cth %*% t(G)
    corr[j] <- sum(diag(A %*% B))
  }
  mse <- k0$var + 2 * corr
  list(prediction = pred, mse = mse, se = sqrt(pmax(mse, 0)),
       mse_plugin = k0$var, correction = 2 * corr,
       parameters = list(nugget = theta[1], psill = theta[2],
                         range = theta[3]),
       parameters_estimated = estimated, model = model,
       n = n, n_target = m,
       method = "Ordinary kriging with Prasad-Rao corrected prediction error")
}
