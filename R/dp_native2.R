# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Differential-privacy shelf, part 2: covariance and PCA, the range
# substitute, the accounting functions, and the two empirical auditors.
# R mirrors of dpcov, dppca, dpmnp, dpamp, dprnyi, dprcl, dpepsm, dpedm
# and dpunit.
#
# The accounting members (amplification, Renyi composition, calibration,
# the privacy unit) are pure arithmetic and match the Python exactly.
# The rest draw noise and are checked on their deterministic parts plus
# their sampling behaviour, as in dp_native.R.

#' Differentially private covariance matrix
#'
#' Rows are clipped to L2 norm C, the second-moment matrix formed, and
#' symmetric Gaussian noise added with sigma calibrated to
#' \eqn{C^2/n}.
#'
#' Noise breaks positive semi-definiteness, which matters because a
#' covariance with negative eigenvalues is not a covariance and breaks
#' anything downstream. Projecting onto the PSD cone is pure
#' post-processing and therefore FREE -- it costs no privacy at all.
#' Many negative eigenvalues is the diagnostic worth watching: it means
#' the budget is too small for this dimension, not that the projection
#' failed.
#'
#' @param X data matrix, one row per record.
#' @param C L2 clipping norm for a row.
#' @param epsilon,delta privacy budget.
#' @param seed optional integer seed.
#' @param project_psd project the result onto the PSD cone.
#' @return list with \code{release}, \code{raw} (pre-projection),
#'   \code{sigma}, \code{clipped_fraction},
#'   \code{n_negative_eigenvalues}.
#' @references Dwork, C. et al. (2014). Analyze Gauss: optimal bounds
#'   for privacy-preserving PCA. \emph{STOC}, 11-20.
#' @examples
#' set.seed(1)
#' r <- morie_dp_covariance(matrix(rnorm(200), ncol = 2), C = 3, epsilon = 2)
#' dim(r$release)
#' @export
morie_dp_covariance <- function(X, C = 1, epsilon = 1, delta = 1e-5,
                                seed = NULL, project_psd = TRUE) {
  bud <- .morie_dp_check_budget(epsilon, delta)
  C <- as.numeric(C)[1L]
  if (C <= 0) stop("C must be positive", call. = FALSE)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  norms <- sqrt(rowSums(X^2))
  scale <- pmin(1, C / pmax(norms, 1e-12))
  Xc <- X * scale
  clipped <- mean(norms > C)
  S <- crossprod(Xc) / n
  sigma <- .morie_dp_gaussian_sigma(C^2 / n, bud$epsilon, bud$delta)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  noise <- matrix(stats::rnorm(p * p, 0, sigma), p, p)
  # Symmetrise by reflecting the upper triangle, exactly as _dp does:
  # an unsymmetric perturbation of a symmetric matrix is not a
  # perturbation of a covariance.
  up <- noise
  up[lower.tri(up)] <- 0
  strict <- noise
  strict[lower.tri(strict, diag = TRUE)] <- 0
  noise <- up + t(strict)
  raw <- S + noise
  ev <- eigen(raw, symmetric = TRUE)
  n_neg <- sum(ev$values < 0)
  out <- if (project_psd && n_neg > 0L) {
    o <- ev$vectors %*% diag(pmax(ev$values, 0), p, p) %*% t(ev$vectors)
    (o + t(o)) / 2
  } else {
    raw
  }
  list(release = out, raw = raw, sigma = sigma, clipped_fraction = clipped,
       n_negative_eigenvalues = as.integer(n_neg), epsilon = bud$epsilon,
       delta = bud$delta, C = C, n = n,
       warnings = if (n_neg > p %/% 2) {
         sprintf(paste("%d of %d eigenvalues were negative before projection;",
                       "the budget may be too small for this dimension"),
                 n_neg, p)
       } else {
         character(0)
       },
       method = "dp_covariance")
}


#' Differentially private PCA
#'
#' Eigendecomposition of the private covariance. Because every step
#' after the noisy release is post-processing, the eigendecomposition
#' itself is FREE -- the whole privacy cost was paid by
#' \code{\link{morie_dp_covariance}}.
#'
#' Accuracy is governed by the EIGENGAP, not by epsilon alone. When
#' consecutive eigenvalues are close, the individual components are
#' unstable under any perturbation, private or not, although the
#' subspace they jointly span may be perfectly well determined. The
#' function reports the gap and warns when it is small.
#'
#' @inheritParams morie_dp_covariance
#' @param k number of components.
#' @return list with \code{components} (p x k), \code{eigenvalues},
#'   \code{eigengap}, \code{explained_variance_ratio}, \code{scores}.
#' @references Dwork, C. et al. (2014). \emph{STOC}, 11-20.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' morie_dp_pca(X, k = 2, epsilon = 4, C = 4)$eigengap > -Inf
#' @export
morie_dp_pca <- function(X, k = 2, epsilon = 1, delta = 1e-5, C = 1,
                         seed = NULL) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  p <- ncol(X)
  k <- as.integer(k)
  if (k < 1L || k > p) {
    stop(sprintf("k must be between 1 and %d", p), call. = FALSE)
  }
  cov <- morie_dp_covariance(X, C = C, epsilon = epsilon, delta = delta,
                             seed = seed)
  ev <- eigen(cov$release, symmetric = TRUE)      # already descending
  vals <- ev$values
  vecs <- ev$vectors
  gap <- if (k < p) vals[k] - vals[k + 1L] else Inf
  total <- sum(pmax(vals, 0))
  warn <- cov$warnings
  if (is.finite(gap) && vals[1L] > 0 && gap < 0.05 * vals[1L]) {
    warn <- c(warn, paste("the eigengap is small relative to the leading",
                          "eigenvalue; individual components are unstable,",
                          "though the subspace they span may not be"))
  }
  list(components = vecs[, seq_len(k), drop = FALSE], eigenvalues = vals,
       eigengap = gap,
       explained_variance_ratio = if (total > 0) {
         pmax(vals[seq_len(k)], 0) / total
       } else {
         rep(NA_real_, k)
       },
       scores = X %*% vecs[, seq_len(k), drop = FALSE],
       covariance = cov$release, epsilon = cov$epsilon, delta = cov$delta,
       warnings = warn, method = "dp_pca")
}


#' Private substitute for a range
#'
#' The alpha and 1-alpha quantiles, each spending half the budget
#' through \code{\link{morie_dp_quantile}}.
#'
#' There is no private minimum or maximum. Each extreme is determined by
#' exactly one record, so releasing it releases that record. This
#' function returns inner quantiles and says so in \code{warnings};
#' treating them as the range is the error it exists to prevent.
#'
#' @param x values.
#' @param epsilon total budget, split evenly between the two quantiles.
#' @param a,b bounds, as for \code{\link{morie_dp_quantile}}.
#' @param alpha tail fraction, in (0, 0.5).
#' @param seed optional integer seed.
#' @return list with \code{lower}, \code{upper}, \code{epsilon_each},
#'   \code{true_min}, \code{true_max}.
#' @references Smith, A. (2011). \emph{STOC}, 813-822.
#' @examples
#' set.seed(1)
#' r <- morie_dp_minmax(rnorm(500), epsilon = 2, a = -4, b = 4)
#' r$lower < r$upper
#' @export
morie_dp_minmax <- function(x, epsilon = 1, a = NULL, b = NULL, alpha = 0.01,
                            seed = NULL) {
  if (alpha <= 0 || alpha >= 0.5) {
    stop("alpha must be in (0, 0.5)", call. = FALSE)
  }
  v <- as.numeric(x)
  half <- as.numeric(epsilon) / 2
  lo <- morie_dp_quantile(v, q = alpha, epsilon = half, a = a, b = b,
                          seed = seed)
  hi <- morie_dp_quantile(v, q = 1 - alpha, epsilon = half, a = a, b = b,
                          seed = if (is.null(seed)) NULL else seed + 1L)
  list(lower = lo$release, upper = hi$release, alpha = alpha,
       epsilon = as.numeric(epsilon), epsilon_each = half,
       true_min = min(v), true_max = max(v),
       warnings = c(lo$warnings,
                    paste("these are the alpha and 1-alpha quantiles, NOT the",
                          "minimum and maximum -- the true extremes are each",
                          "determined by one record and cannot be released",
                          "privately")),
       method = "dp_minmax")
}


#' Privacy amplification by subsampling
#'
#' \eqn{\epsilon' = \log(1 + q(e^\epsilon - 1))}, which for small
#' epsilon is close to \eqn{q\epsilon} -- running a mechanism on a
#' q-fraction of the data buys roughly a factor q of privacy.
#'
#' The amplification is real only for a SECRET, freshly drawn subsample.
#' If the sample is fixed, published, or inferable, there is no
#' amplification at all and the claimed epsilon is simply wrong.
#'
#' @param epsilon base epsilon.
#' @param q sampling rate in (0, 1].
#' @param delta base delta.
#' @return list with \code{epsilon_amplified}, \code{delta_amplified},
#'   \code{ratio}, \code{linear_approx}.
#' @references Balle, B., Barthe, G. and Gaboardi, M. (2018). Privacy
#'   amplification by subsampling. \emph{NeurIPS}, 6277-6287.
#' @examples
#' round(morie_privacy_amplification(1, 0.01)$epsilon_amplified, 5)
#' @export
morie_privacy_amplification <- function(epsilon, q, delta = 0) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  q <- as.numeric(q)[1L]
  if (q <= 0 || q > 1) stop("q must be in (0, 1]", call. = FALSE)
  eps_a <- log1p(q * expm1(eps))
  list(epsilon_amplified = eps_a, delta_amplified = q * as.numeric(delta),
       ratio = eps_a / eps, linear_approx = q * eps, epsilon = eps, q = q,
       warnings = paste("amplification holds only for secret, freshly drawn",
                        "subsamples; a fixed or observable subsample gives",
                        "none"),
       method = "privacy_amplification")
}


#' Renyi differential privacy composition
#'
#' RDP epsilons at a fixed order alpha compose EXACTLY additively --
#' that is the whole reason to account in RDP rather than in
#' (epsilon, delta). Conversion back costs
#' \eqn{\log(1/\delta)/(\alpha-1)}.
#'
#' Because the conversion penalty falls with alpha while the RDP total
#' rises with it, the final epsilon is minimised at some interior alpha.
#' Account first, then sweep alpha; picking alpha up front leaves budget
#' on the table.
#'
#' @param epsilons vector of per-mechanism RDP epsilons at order alpha.
#' @param alpha Renyi order, greater than 1.
#' @param delta target delta for the conversion.
#' @return list with \code{rdp_total}, \code{epsilon} (converted),
#'   \code{conversion_penalty}.
#' @references Mironov, I. (2017). Renyi differential privacy.
#'   \emph{CSF}, 263-275.
#' @examples
#' round(morie_renyi_dp_composition(rep(0.1, 10), alpha = 4)$epsilon, 4)
#' @export
morie_renyi_dp_composition <- function(epsilons, alpha = 2, delta = 1e-5) {
  alpha <- as.numeric(alpha)[1L]
  if (alpha <= 1) stop("alpha must be greater than 1", call. = FALSE)
  delta <- as.numeric(delta)[1L]
  if (delta <= 0 || delta >= 1) {
    stop("delta must be in (0, 1)", call. = FALSE)
  }
  eps <- as.numeric(epsilons)
  if (length(eps) == 0L) stop("epsilons must be non-empty", call. = FALSE)
  if (any(eps < 0)) {
    stop("RDP epsilons must be non-negative", call. = FALSE)
  }
  total <- sum(eps)
  penalty <- log(1 / delta) / (alpha - 1)
  list(rdp_total = total, epsilon = total + penalty, delta = delta,
       alpha = alpha, k = length(eps), conversion_penalty = penalty,
       method = "renyi_dp_composition")
}


#' Calibrate a privacy budget to a tolerable error, or the reverse
#'
#' For Laplace noise of scale \eqn{b = \Delta/(n\epsilon)}, the
#' confidence half-width is \eqn{b\log(1/(1-c))}. Supply a target error
#' and get the epsilon it implies, or supply epsilon and get the error
#' it buys.
#'
#' The point is to set the budget FROM a tolerable error rather than by
#' convention. An epsilon above 10 means a likelihood ratio above 22000
#' between neighbouring datasets, which is close to no guarantee at all;
#' the function warns rather than returning it silently.
#'
#' @param sensitivity query sensitivity.
#' @param target_error desired half-width. Supply this OR \code{epsilon}.
#' @param epsilon budget. Supply this OR \code{target_error}.
#' @param confidence coverage for the half-width, in (0, 1).
#' @param n number of records the query averages over.
#' @return list with \code{epsilon}, \code{half_width},
#'   \code{noise_scale}, \code{direction}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4).
#' @examples
#' round(morie_dp_release_calibration(1, target_error = 0.01,
#'                                    n = 1000)$epsilon, 4)
#' @export
morie_dp_release_calibration <- function(sensitivity = 1, target_error = NULL,
                                         epsilon = NULL, confidence = 0.95,
                                         n = 1) {
  if (is.null(target_error) == is.null(epsilon)) {
    stop("supply exactly one of target_error or epsilon", call. = FALSE)
  }
  sensitivity <- as.numeric(sensitivity)[1L]
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  if (confidence <= 0 || confidence >= 1) {
    stop("confidence must be in (0, 1)", call. = FALSE)
  }
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1", call. = FALSE)
  z <- log(1 / (1 - confidence))
  if (!is.null(target_error)) {
    w <- as.numeric(target_error)[1L]
    if (w <= 0) stop("target_error must be positive", call. = FALSE)
    eps <- sensitivity * z / (n * w)
    direction <- "error -> epsilon"
  } else {
    eps <- as.numeric(epsilon)[1L]
    if (eps <= 0) stop("epsilon must be positive", call. = FALSE)
    w <- sensitivity * z / (n * eps)
    direction <- "epsilon -> error"
  }
  b <- sensitivity / (n * eps)
  list(epsilon = eps, half_width = w, noise_scale = b,
       noise_sd = sqrt(2) * b, direction = direction,
       sensitivity = sensitivity, confidence = confidence, n = n,
       warnings = if (eps > 10) {
         paste("epsilon exceeds 10, so the likelihood ratio is above 22000",
               "and the guarantee is close to vacuous")
       } else {
         character(0)
       },
       method = "dp_release_calibration")
}


#' Empirical epsilon of a mechanism
#'
#' Samples a mechanism on two neighbouring datasets and takes the
#' largest log density ratio over histogram bins with enough mass in
#' both.
#'
#' This is a LOWER bound from one dataset pair. It can DISPROVE a
#' claimed guarantee -- if the empirical ratio exceeds the claimed
#' epsilon, the claim is false -- but it can never establish one, since
#' some other pair of neighbours may do worse. Use it as a test, never
#' as a certificate.
#'
#' @param mech function of \code{(data)} returning one release; called
#'   repeatedly.
#' @param D,D_prime neighbouring datasets.
#' @param n_samples draws per dataset.
#' @param bins histogram bins.
#' @param seed optional integer seed.
#' @return list with \code{epsilon_empirical}, \code{n_usable_bins},
#'   \code{n_excluded_bins}.
#' @references Ding, Z. et al. (2018). Detecting violations of
#'   differential privacy. \emph{CCS}, 475-489.
#' @examples
#' set.seed(1)
#' m <- function(d) sum(d) + morie_dp_laplace_mechanism(0, 1, 1)$release
#' morie_epsilon_dp(m, rep(1, 10), rep(1, 9), n_samples = 2000)$epsilon_empirical
#' @export
morie_epsilon_dp <- function(mech, D, D_prime, n_samples = 20000, bins = 50,
                             seed = NULL) {
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  n_samples <- as.integer(n_samples)
  a <- vapply(seq_len(n_samples), function(i) as.numeric(mech(D))[1L],
              numeric(1))
  b <- vapply(seq_len(n_samples), function(i) as.numeric(mech(D_prime))[1L],
              numeric(1))
  lo <- min(a, b)
  hi <- max(a, b)
  if (lo == hi) {
    return(list(epsilon_empirical = Inf, max_log_ratio = Inf,
                n_usable_bins = 0L, n_excluded_bins = 0L,
                warnings = "the mechanism is deterministic; it provides no privacy",
                method = "epsilon_dp"))
  }
  edges <- seq(lo, hi, length.out = as.integer(bins) + 1L)
  nb <- length(edges) - 1L
  cnt <- function(v) {
    tabulate(pmin(pmax(findInterval(v, edges), 1L), nb), nbins = nb)
  }
  ca <- cnt(a)
  cb <- cnt(b)
  floor_ <- max(10L, as.integer(0.001 * n_samples))
  usable <- ca >= floor_ & cb >= floor_
  ratio <- if (!any(usable)) {
    Inf
  } else {
    max(abs(log((ca[usable] / sum(ca)) / (cb[usable] / sum(cb)))))
  }
  list(epsilon_empirical = ratio, max_log_ratio = ratio,
       n_usable_bins = as.integer(sum(usable)),
       n_excluded_bins = as.integer(sum(!usable)),
       n_samples = n_samples,
       warnings = paste("this is a LOWER bound from one dataset pair: it can",
                        "disprove a claimed guarantee but never establish one"),
       method = "epsilon_dp")
}


#' Empirical delta at a fixed epsilon
#'
#' The probability mass on which the \eqn{e^\epsilon} bound fails.
#'
#' Delta is not slack. At \eqn{\delta = 1/n} a mechanism may release one
#' record outright and still satisfy the definition, so delta belongs
#' well below 1/n -- cryptographically small, not merely small.
#'
#' @inheritParams morie_epsilon_dp
#' @param epsilon the epsilon whose bound is being tested.
#' @return list with \code{delta_empirical}, \code{n_violating_bins}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4),
#'   Sec. 2.3.
#' @examples
#' set.seed(1)
#' m <- function(d) sum(d) + morie_dp_laplace_mechanism(0, 1, 1)$release
#' morie_approx_dp(m, rep(1, 10), rep(1, 9), epsilon = 1,
#'                 n_samples = 2000)$delta_empirical < 1
#' @export
morie_approx_dp <- function(mech, D, D_prime, epsilon = 1, n_samples = 20000,
                            bins = 50, seed = NULL) {
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  n_samples <- as.integer(n_samples)
  a <- vapply(seq_len(n_samples), function(i) as.numeric(mech(D))[1L],
              numeric(1))
  b <- vapply(seq_len(n_samples), function(i) as.numeric(mech(D_prime))[1L],
              numeric(1))
  lo <- min(a, b)
  hi <- max(a, b)
  if (lo == hi) {
    return(list(delta_empirical = 1, epsilon = as.numeric(epsilon),
                n_violating_bins = 0L,
                warnings = "the mechanism is deterministic; it provides no privacy",
                method = "approx_dp"))
  }
  edges <- seq(lo, hi, length.out = as.integer(bins) + 1L)
  nb <- length(edges) - 1L
  cnt <- function(v) {
    tabulate(pmin(pmax(findInterval(v, edges), 1L), nb), nbins = nb)
  }
  ca <- cnt(a)
  cb <- cnt(b)
  pa <- ca / max(sum(ca), 1)
  pb <- cb / max(sum(cb), 1)
  viol <- pa > exp(epsilon) * pb
  delta <- sum(pmax(pa[viol] - exp(epsilon) * pb[viol], 0))
  list(delta_empirical = delta, epsilon = as.numeric(epsilon),
       n_violating_bins = as.integer(sum(viol)), n_samples = n_samples,
       warnings = paste("a lower bound from one dataset pair; and note that",
                        "delta is not slack -- at delta = 1/n a per-record",
                        "leak is permitted"),
       method = "approx_dp")
}


#' Unit of privacy: how many records one entity contributes
#'
#' Differential privacy protects a UNIT, and the unit is a choice.
#' Sensitivity computed per row protects a row; if one person supplies
#' twenty rows, their protection is twenty times weaker than the stated
#' epsilon suggests.
#'
#' This function counts contributions per unit and reports the
#' multiplier the sensitivity must carry -- or, equivalently, the cap
#' that must be imposed on contributions before the query is run.
#'
#' @param records vector of unit identifiers, one entry per record.
#' @param unit optional identifier whose own contribution to report.
#' @return list with \code{n_records}, \code{n_units},
#'   \code{max_contribution}, \code{sensitivity_multiplier},
#'   \code{contributions}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4),
#'   Sec. 2.3 (group privacy).
#' @examples
#' morie_dp_unit_definition(c(1, 1, 2, 3, 3, 3))$sensitivity_multiplier
#' @export
morie_dp_unit_definition <- function(records, unit = NULL) {
  r <- as.vector(records)
  if (length(r) == 0L) stop("records must be non-empty", call. = FALSE)
  tb <- table(r)
  units <- names(tb)
  counts <- as.integer(tb)
  mx <- max(counts)
  out <- list(n_records = length(r), n_units = length(counts),
              max_contribution = mx, sensitivity_multiplier = mx,
              units = units, contributions = counts,
              mean_contribution = mean(counts),
              warnings = if (mx > 1L) {
                sprintf(paste("one unit contributes %d records, so a",
                              "sensitivity computed per record understates",
                              "the unit-level sensitivity by %dx; either",
                              "multiply the noise or cap contributions first"),
                        mx, mx)
              } else {
                character(0)
              },
              method = "dp_unit_definition")
  if (!is.null(unit)) out$unit_contribution <- sum(r == unit)
  out
}
