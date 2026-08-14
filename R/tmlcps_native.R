# Doubly robust estimation of a continuous-treatment effect curve.
# Sources: Kennedy, E. H., Ma, Z., McHugh, M. D. & Small, D. S. (2017)
# Non-parametric methods for doubly robust estimation of continuous
# treatment effects, Journal of the Royal Statistical Society, Series
# B 79(4), 1229-1245, doi:10.1111/rssb.12212; arXiv:1507.00747.
# Theorem 1 and Sec. 3.2; Hirano, K. & Imbens, G. W. (2004) The
# propensity score with continuous treatments, in Applied Bayesian
# Modeling and Causal Inference from Incomplete-Data Perspectives,
# Wiley, 73-84, doi:10.1002/0470090456.ch7 -- the generalized
# propensity score that pi(a | l) is.
#
# Native implementation mirroring Python morie.fn.tmlcps exactly: the
# same doubly robust pseudo-outcome xi(Z; pi, mu) with both integrals
# taken across the sample's covariates at the row's own treatment
# value, and the same kernel / local-linear / polynomial stage-2.

.FITS <- c("kernel", "locallinear", "polynomial")

.kern <- function(u) exp(-0.5 * u * u)

.smooth_at <- function(xv, av, g, h, fit) {
  n <- length(xv)
  w <- .kern((av - g) / h)
  sw <- sum(w)
  if (sw <= 0) return(NaN)
  if (fit == "kernel")
    return(sum(w * xv) / sw)
  s1 <- sum(w * (av - g))
  s2 <- sum(w * (av - g)^2)
  t0 <- sum(w * xv)
  t1 <- sum(w * (av - g) * xv)
  det <- sw * s2 - s1 * s1
  if (abs(det) < 1e-300) return(t0 / sw)
  (s2 * t0 - s1 * t1) / det
}

.cv_bandwidth <- function(xv, av, fit, n_folds) {
  n <- length(xv)
  spread <- max(av) - min(av)
  if (spread <= 0) stop("_cv_bandwidth: the treatment is constant")
  grid <- spread * c(0.02, 0.05, 0.08, 0.12, 0.2, 0.3, 0.5, 0.8)
  nf <- as.integer(n_folds)
  folds <- lapply(seq_len(nf) - 1L, function(f)
    which(seq_len(n) %% nf == f))
  best <- NULL; best_h <- grid[1]
  for (h in grid) {
    err <- 0
    for (f in folds) {
      tr <- setdiff(seq_len(n), f)
      xtr <- xv[tr]; atr <- av[tr]
      for (i in f) {
        pred <- .smooth_at(xtr, atr, av[i], h, fit)
        if (is.nan(pred)) err <- err + 1e12
        else err <- err + (xv[i] - pred)^2
      }
    }
    if (is.null(best) || err < best) { best <- err; best_h <- h }
  }
  best_h
}

#' Kennedy et al. Theorem 1's xi(Z; pi, mu), one value per row
#'
#' The nuisances are a Gaussian conditional treatment density and a
#' linear outcome regression with a treatment interaction; both are the
#' working models, and the point of the construction is that only one
#' of them has to be right.
#'
#' @param y Outcome vector.
#' @param A Treatment vector.
#' @param X Covariate matrix.
#' @param ridge Ridge regulariser.
#' @return A list with \code{xi} and \code{info} (marginal_density,
#'   standardized_mu, treatment_coef, treatment_sigma2, outcome_coef,
#'   pi_obs).
#' @references Kennedy, E. H. et al. (2017). Theorem 1.
#' @export
pseudo_outcome <- function(y, A, X, ridge = 1e-8) {
  yv <- as.numeric(y); av <- as.numeric(A); n <- length(yv)
  if (length(av) != n)
    stop("pseudo_outcome: outcome and treatment differ in length")
  Xm <- if (is.null(X)) matrix(0, nrow = n, ncol = 0) else as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != n)
    stop("pseudo_outcome: covariates and outcome differ in length")
  p <- ncol(Xm)
  Zt <- if (p > 0) cbind(1, Xm) else matrix(1, nrow = n, ncol = 1)
  bt <- as.numeric(solve(crossprod(Zt) + ridge * diag(ncol(Zt)),
                          crossprod(Zt, av)))
  mu_a <- as.numeric(Zt %*% bt)
  res <- av - mu_a
  s2 <- sum(res^2) / max(1L, n - length(bt))
  if (s2 <= 0) stop("pseudo_outcome: the treatment model fits the dose exactly, so pi(A|L) is degenerate")
  c_ <- 1 / sqrt(2 * pi * s2)
  pi_at <- function(a, j) c_ * exp(-0.5 * (a - mu_a[j])^2 / s2)
  if (p > 0) {
    Zy <- cbind(1, av, Xm,
                do.call(cbind, lapply(seq_len(p), function(q) av * Xm[, q])))
  } else {
    Zy <- matrix(1, nrow = n, ncol = 1)
  }
  by <- as.numeric(solve(crossprod(Zy) + ridge * diag(ncol(Zy)),
                          crossprod(Zy, yv)))
  mu_at <- function(a, j) {
    if (p > 0)
      sum(c(1, a, Xm[j, ], a * Xm[j, ]) * by)
    else sum(c(1, a) * by)
  }
  xi <- numeric(n); marg <- numeric(n); stand <- numeric(n)
  pi_obs <- numeric(n)
  for (i in seq_len(n)) {
    a_i <- av[i]
    m <- sum(vapply(seq_len(n), function(j) pi_at(a_i, j),
                     numeric(1))) / n
    s <- sum(vapply(seq_len(n), function(j) mu_at(a_i, j),
                     numeric(1))) / n
    den <- pi_at(a_i, i)
    if (den <= 0)
      stop("pseudo_outcome: pi(A|L) is zero at observation, so positivity fails and xi is undefined")
    xi[i] <- (yv[i] - mu_at(a_i, i)) * m / den + s
    marg[i] <- m; stand[i] <- s; pi_obs[i] <- den
  }
  list(xi = xi, info = list(marginal_density = marg,
                            standardized_mu = stand,
                            treatment_coef = bt,
                            treatment_sigma2 = s2,
                            outcome_coef = by,
                            pi_obs = pi_obs))
}

#' Stage 2: regress the pseudo-outcome on the treatment
#'
#' \code{kernel} is the Nadaraya-Watson estimator the paper analyses,
#' \code{locallinear} the local-linear version that is less biased at
#' the boundary, and \code{polynomial} a global cubic for when the
#' curve is genuinely smooth and the sample is small.
#'
#' @param xi Pseudo-outcome vector.
#' @param A Treatment vector.
#' @param grid Grid of treatment values to evaluate the curve at.
#' @param fit One of \code{"kernel"}, \code{"locallinear"},
#'   \code{"polynomial"}.
#' @param bandwidth Optional bandwidth; otherwise chosen by
#'   cross-validation.
#' @param n_folds Number of CV folds.
#' @return A list with the curve and \code{info} (bandwidth, coef).
#' @references Kennedy, E. H. et al. (2017). Sec. 3.2-3.3.
#' @export
effect_curve <- function(xi, A, grid, fit = "kernel",
                         bandwidth = NULL, n_folds = 5) {
  if (!fit %in% .FITS)
    stop("effect_curve: fit must be one of kernel, locallinear, polynomial")
  xv <- as.numeric(xi); av <- as.numeric(A); n <- length(xv)
  gr <- as.numeric(grid)
  if (fit == "polynomial") {
    X <- cbind(1, av, av^2, av^3)
    b <- as.numeric(solve(crossprod(X), crossprod(X, xv)))
    curve <- b[1] + b[2] * gr + b[3] * gr^2 + b[4] * gr^3
    return(list(curve = curve, info = list(coef = b, bandwidth = NULL)))
  }
  h <- if (is.null(bandwidth)) .cv_bandwidth(xv, av, fit, n_folds)
       else as.numeric(bandwidth)
  if (h <= 0) stop("effect_curve: bandwidth must be positive")
  curve <- vapply(gr, function(g) .smooth_at(xv, av, g, h, fit),
                  numeric(1))
  list(curve = curve, info = list(bandwidth = h, coef = NULL))
}

#' The effect curve theta(a) = E(Y^a) for a continuous treatment
#'
#' @param y Outcome vector.
#' @param A Continuous treatment vector.
#' @param X Covariate matrix.
#' @param a_grid Grid of treatment values.
#' @param fit Stage-2 regression.
#' @param bandwidth Optional bandwidth.
#' @param n_folds Number of CV folds.
#' @return A list with \code{estimate}, \code{se}, \code{curve},
#'   \code{grid}, \code{slopes}, \code{pseudo_outcome},
#'   \code{bandwidth}, \code{marginal_density}, \code{standardized_mu},
#'   \code{pi_obs}, \code{fit}, \code{n}, \code{method}.
#' @references Kennedy, E. H. et al. (2017).
#' @export
morie_tmlcps <- function(y, A, X, a_grid = NULL, fit = "kernel",
                         bandwidth = NULL, n_folds = 5) {
  av <- as.numeric(A)
  if (length(unique(av)) < 3L)
    stop("tmle_continuous_treatment: the treatment takes fewer than 3 distinct values; this estimates a continuous effect curve and a binary exposure belongs elsewhere")
  po <- pseudo_outcome(y, A, X); xi <- po$xi
  if (is.null(a_grid)) {
    lo <- min(av); hi <- max(av)
    a_grid <- lo + (hi - lo) * seq(0, 20) / 20
  }
  ec <- effect_curve(xi, av, a_grid, fit = fit,
                     bandwidth = bandwidth, n_folds = n_folds)
  gr <- as.numeric(a_grid)
  cv <- ec$curve
  slopes <- numeric(0)
  for (t in seq_len(length(gr) - 1L))
    if (gr[t + 1] != gr[t])
      slopes <- c(slopes, (cv[t + 1] - cv[t]) / (gr[t + 1] - gr[t]))
  est <- if (length(slopes) > 0) mean(slopes) else NaN
  n <- length(av)
  xbar <- mean(xi)
  se <- if (n > 1) sqrt(sum((xi - xbar)^2) / (n * (n - 1))) else NaN
  list(estimate = est, se = se, curve = cv, grid = gr, slopes = slopes,
       pseudo_outcome = xi, bandwidth = ec$info$bandwidth,
       marginal_density = po$info$marginal_density,
       standardized_mu = po$info$standardized_mu,
       pi_obs = po$info$pi_obs, fit = fit, n = n,
       method = paste0("doubly robust effect curve for a continuous ",
                       "treatment, Kennedy, Ma, McHugh & Small (2017) ",
                       "Theorem 1 and Sec. 3.2"))
}

#' Compact alias per ledger/NAMING.md
#' @export
morie_tmlcontinuoustreatment <- morie_tmlcps
