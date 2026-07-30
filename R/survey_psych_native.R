# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Survey replication, compositional balance, genomic relatedness and IRT
# linking. R mirrors of the morie.fn modules aitbal, astlb, brrest,
# brrvar, eqhae and eqsl.

#' Aitchison balance coordinate
#'
#' \eqn{\sqrt{rs/(r+s)}\,\log(g(x_{num})/g(x_{den}))}: the normalised
#' log-ratio of the geometric means of two disjoint groups of parts.
#'
#' A composition carries only RELATIVE information -- doubling every
#' part changes nothing -- so any statistic computed on the raw
#' proportions is measuring the constraint rather than the data. Log
#' ratios are the coordinates that respect that, and the
#' \eqn{\sqrt{rs/(r+s)}} factor makes balances from different group
#' sizes comparable.
#'
#' Zeros are fatal, not inconvenient: the logarithm is undefined, and
#' the function refuses rather than adding an arbitrary constant that
#' would silently determine the answer. Replace them deliberately first.
#'
#' @param x composition matrix, strictly positive; rows are
#'   observations.
#' @param numerator,denominator 0-based column indices of the two
#'   disjoint groups, as in the Python module.
#' @return list with \code{balance}, \code{normalizer},
#'   \code{geometric_mean_num}, \code{geometric_mean_den}.
#' @references Egozcue, J. J. and Pawlowsky-Glahn, V. (2005). Groups of
#'   parts and their balances in compositional data analysis.
#'   \emph{Mathematical Geology}, 37(7), 795-828.
#' @examples
#' x <- matrix(c(0.2, 0.3, 0.5, 0.1, 0.6, 0.3), ncol = 3, byrow = TRUE)
#' round(morie_aitchison_balance(x, 0, c(1, 2))$balance, 4)
#' @export
morie_aitchison_balance <- function(x, numerator, denominator) {
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  if (any(X <= 0)) {
    stop("compositions must be strictly positive; replace zeros first",
         call. = FALSE)
  }
  num <- as.integer(numerator)
  den <- as.integer(denominator)
  if (length(intersect(num, den))) {
    stop("numerator and denominator groups must be disjoint", call. = FALSE)
  }
  p <- ncol(X)
  if (any(num >= p) || any(den >= p) || any(num < 0) || any(den < 0)) {
    stop(sprintf("group indices must lie in 0..%d", p - 1L), call. = FALSE)
  }
  r <- length(num)
  s <- length(den)
  if (r == 0L || s == 0L) {
    stop("both groups must be non-empty", call. = FALSE)
  }
  gn <- exp(rowMeans(log(X[, num + 1L, drop = FALSE])))
  gd <- exp(rowMeans(log(X[, den + 1L, drop = FALSE])))
  norm <- sqrt(r * s / (r + s))
  list(balance = norm * log(gn / gd), normalizer = norm,
       geometric_mean_num = gn, geometric_mean_den = gd,
       numerator = num, denominator = den, method = "aitchison_balance")
}


#' Astle-Balding genomic relationship matrix
#'
#' Standardises each marker by its own \eqn{\sqrt{2p(1-p)}} before
#' taking the cross-product.
#'
#' The per-MARKER standardisation is what distinguishes this from
#' VanRaden's, which divides the whole matrix by the total variance. Per
#' marker up-weights rare variants: a variant at frequency 0.01 gets the
#' same weight as one at 0.5 despite carrying far less information. That
#' helps when rare variants genuinely carry signal and hurts when what
#' they carry is genotyping error.
#'
#' Frequencies estimated from the sample make the matrix sample-specific
#' and not comparable across cohorts; supply \code{freq} from a
#' reference panel when comparability matters.
#'
#' @param marker_matrix genotypes coded 0/1/2, individuals by markers.
#' @param freq optional allele frequencies; estimated from the sample
#'   when NULL.
#' @return list with \code{G}, \code{n_markers_used}, \code{n_dropped},
#'   \code{freq}, \code{mean_diagonal}.
#' @references Astle, W. and Balding, D. J. (2009). Population structure
#'   and cryptic relatedness in genetic association studies.
#'   \emph{Statistical Science}, 24(4), 451-471.
#' @examples
#' set.seed(1)
#' M <- matrix(rbinom(200, 2, 0.3), nrow = 20)
#' round(mean(diag(morie_astle_balding_grm(M)$G)), 3)
#' @export
morie_astle_balding_grm <- function(marker_matrix, freq = NULL) {
  X <- as.matrix(marker_matrix)
  storage.mode(X) <- "double"
  n <- nrow(X)
  m <- ncol(X)
  if (any(X < 0 | X > 2)) {
    stop("genotypes must be coded 0, 1 or 2", call. = FALSE)
  }
  p <- if (is.null(freq)) colMeans(X) / 2 else as.numeric(freq)
  if (length(p) != m) {
    stop(sprintf("freq has %d entries but there are %d markers", length(p), m),
         call. = FALSE)
  }
  var <- 2 * p * (1 - p)
  keep <- var > 1e-12
  if (!any(keep)) stop("every marker is monomorphic", call. = FALSE)
  Z <- sweep(X[, keep, drop = FALSE], 2L, 2 * p[keep], "-")
  Z <- sweep(Z, 2L, sqrt(var[keep]), "/")
  G <- tcrossprod(Z) / sum(keep)
  list(G = G, n_markers_used = as.integer(sum(keep)),
       n_dropped = as.integer(sum(!keep)), freq = p,
       mean_diagonal = mean(diag(G)), n = n,
       warnings = c(paste("per-marker standardisation up-weights rare variants",
                          "relative to VanRaden scaling; that helps when rare",
                          "variants carry signal and hurts when they carry",
                          "genotyping error"),
                    if (is.null(freq)) {
                      paste("frequencies were estimated from this sample, so",
                            "the matrix is sample-specific and not comparable",
                            "across cohorts")
                    }),
       method = "astle_balding_grm")
}


#' Balanced repeated replication half-samples
#'
#' Builds replicate weights from a Hadamard matrix, one PSU per stratum
#' in each half-sample.
#'
#' The Hadamard orthogonality is the whole trick: it is why R replicates
#' of order H suffice where the \eqn{2^H} possible half-samples would
#' not be enumerable. Using fewer replicates than strata destroys the
#' balance and biases the variance downward.
#'
#' Fay's adjustment replaces the 0/2 weights with k and 2-k, which keeps
#' every unit in every replicate -- necessary when a zero weight would
#' make a domain empty. It must be paired with the \eqn{(1-k)^2} divisor
#' in \code{\link{morie_brr_variance}}.
#'
#' @param strata stratum label per PSU; every stratum needs exactly 2.
#' @param fay_k Fay factor in [0, 1).
#' @return list with \code{replicate_weights} (R x n),
#'   \code{n_replicates}, \code{hadamard}, \code{n_strata}.
#' @references Wolter, K. M. (2007). \emph{Introduction to Variance
#'   Estimation}, 2nd ed., Ch. 3. Springer. Judkins, D. R. (1990).
#'   Fay's method for variance estimation. \emph{JOS}, 6(3), 223-239.
#' @examples
#' morie_brr_balanced(rep(1:4, each = 2))$n_replicates
#' @export
morie_brr_balanced <- function(strata, fay_k = 0) {
  s <- as.vector(strata)
  levels_ <- unique(sort(s))
  inv <- match(s, levels_)
  H <- length(levels_)
  for (h in seq_len(H)) {
    cnt <- sum(inv == h)
    if (cnt != 2L) {
      stop(sprintf("stratum %s has %d PSUs; BRR requires exactly 2",
                   as.character(levels_[h]), cnt), call. = FALSE)
    }
  }
  fay_k <- as.numeric(fay_k)
  if (fay_k < 0 || fay_k >= 1) {
    stop("fay_k must be in [0, 1)", call. = FALSE)
  }
  # Full balance needs orthogonal COLUMNS of the R x H sign matrix.
  # Truncating a Sylvester Hadamard to a non-power-of-two row count
  # destroys that (off-diagonal inner products of 4 at H = 9..12), so
  # R is the next power of two >= max(H, 4): a few extra replicates,
  # exact balance.
  R <- 4L
  while (R < H) R <- R * 2L
  Hm <- matrix(1, 1L, 1L)
  while (nrow(Hm) < R) Hm <- rbind(cbind(Hm, Hm), cbind(Hm, -Hm))
  Hm <- Hm[, seq_len(H), drop = FALSE]
  n <- length(s)
  W <- matrix(0, R, n)
  for (r in seq_len(R)) {
    for (h in seq_len(H)) {
      members <- which(inv == h)
      pick <- if (Hm[r, h] > 0) 1L else 2L
      W[r, members[pick]] <- 2 - fay_k
      W[r, members[3L - pick]] <- fay_k
    }
  }
  list(replicate_weights = W, n_replicates = R, hadamard = Hm,
       n_strata = H, fay_k = fay_k, n = n,
       warnings = paste("pair these weights with the (1-k)^2 divisor in",
                        "morie_brr_variance; using fewer replicates than",
                        "strata biases the variance"),
       method = "brr_balanced")
}


#' Balanced repeated replication variance
#'
#' \eqn{\hat V = \sum_r (\hat\theta_r - \hat\theta)^2 / (R(1-k)^2)}.
#'
#' The replicate estimates must come from re-running the WHOLE
#' procedure -- weighting, calibration, imputation and all -- not just
#' the final formula on reweighted data. A variance that ignores the
#' weighting steps understates the uncertainty those steps introduced.
#'
#' The \eqn{(1-k)^2} divisor is mandatory under a Fay adjustment.
#' Omitting it inflates the variance by \eqn{1/(1-k)^2}, which at
#' k = 0.3 is a 104% overstatement.
#'
#' @param estimates replicate estimates.
#' @param full_estimate the full-sample estimate; the replicate mean is
#'   used when NULL.
#' @param fay_k the Fay factor used to build the replicates.
#' @return list with \code{variance}, \code{se}, \code{n_replicates},
#'   \code{estimate}, \code{cv}.
#' @references Wolter, K. M. (2007). \emph{Introduction to Variance
#'   Estimation}, 2nd ed., Ch. 3. Springer.
#' @examples
#' round(morie_brr_variance(c(1.9, 2.1, 2.2, 1.8), full_estimate = 2)$se, 4)
#' @export
morie_brr_variance <- function(estimates, full_estimate = NULL, fay_k = 0) {
  est <- as.numeric(estimates)
  if (length(est) < 2L) {
    stop("need at least 2 replicate estimates", call. = FALSE)
  }
  fay_k <- as.numeric(fay_k)
  if (fay_k < 0 || fay_k >= 1) {
    stop("fay_k must be in [0, 1)", call. = FALSE)
  }
  theta <- if (is.null(full_estimate)) mean(est) else as.numeric(full_estimate)
  R <- length(est)
  var <- sum((est - theta)^2) / (R * (1 - fay_k)^2)
  se <- sqrt(max(var, 0))
  list(variance = var, se = se, n_replicates = R, fay_k = fay_k,
       estimate = theta, cv = if (theta != 0) se / abs(theta) else NA_real_,
       warnings = paste("the (1-k)^2 divisor is required under a Fay",
                        "adjustment; omitting it inflates the variance by",
                        "1/(1-k)^2"),
       method = "brr_variance")
}


# Two-parameter logistic ICC with the 1.7 normal-metric scaling.
.morie_irt_icc <- function(a, b, theta) {
  1 / (1 + exp(-1.7 * outer(a, rep(1, length(theta))) *
                 (outer(rep(1, length(a)), theta) - outer(b, rep(1, length(theta))))))
}

# Shared linking machinery for Haebara and Stocking-Lord. The two differ
# ONLY in whether the curve differences are summed before or after
# squaring, which is exactly the difference between matching every item
# and matching the test as a whole.
.morie_irt_link <- function(a_ref, b_ref, a_focal, b_focal, n_quad,
                            theta_range, aggregate_first) {
  a_r <- as.numeric(a_ref)
  b_r <- as.numeric(b_ref)
  a_f <- as.numeric(a_focal)
  b_f <- as.numeric(b_focal)
  if (!(length(a_r) == length(b_r) && length(a_r) == length(a_f) &&
          length(a_r) == length(b_f))) {
    stop("all four parameter vectors must have the same length",
         call. = FALSE)
  }
  if (length(a_r) == 0L) {
    stop("need at least one anchor item", call. = FALSE)
  }
  if (any(a_r <= 0) || any(a_f <= 0)) {
    stop("discriminations must be positive", call. = FALSE)
  }
  th <- seq(-theta_range, theta_range, length.out = as.integer(n_quad))
  w <- exp(-0.5 * th^2)
  w <- w / sum(w)
  Pr <- .morie_irt_icc(a_r, b_r, th)
  crit <- function(par) {
    A <- par[1L]
    B <- par[2L]
    if (A <= 0) return(1e12)
    Pf <- .morie_irt_icc(a_f / A, A * b_f + B, th)
    d <- if (aggregate_first) {
      (colSums(Pr) - colSums(Pf))^2
    } else {
      colSums((Pr - Pf)^2)
    }
    sum(w * d)
  }
  res <- stats::optim(c(1, 0), crit, method = "Nelder-Mead",
                      control = list(reltol = 1e-12, maxit = 2000L))
  res <- stats::optim(res$par, crit, method = "Nelder-Mead",
                      control = list(reltol = 1e-12, maxit = 2000L))
  A <- res$par[1L]
  B <- res$par[2L]
  list(A = A, B = B, criterion = res$value, a_transformed = a_f / A,
       b_transformed = A * b_f + B, n_items = length(a_r),
       converged = identical(res$convergence, 0L),
       warnings = paste("remove anchor items showing DIF BEFORE linking; one",
                        "badly functioning anchor distorts the transform for",
                        "every item"))
}


#' Haebara IRT linking
#'
#' Finds the (A, B) transform minimising the quadrature-weighted squared
#' difference between the reference and transformed item characteristic
#' curves, ITEM BY ITEM.
#'
#' Using the whole response curve rather than two moments is the point:
#' mean/mean and mean/sigma linking match summary statistics of the item
#' parameters and can agree perfectly while the curves themselves do
#' not. Haebara's criterion cannot be satisfied by two items whose
#' misfits cancel, because it squares before summing.
#'
#' @param a_ref,b_ref reference-form discriminations and difficulties.
#' @param a_focal,b_focal focal-form parameters for the same anchors.
#' @param n_quad quadrature points.
#' @param theta_range half-width of the quadrature grid.
#' @return list with \code{A}, \code{B}, \code{criterion},
#'   \code{a_transformed}, \code{b_transformed}.
#' @references Haebara, T. (1980). Equating logistic ability scales by a
#'   weighted least squares method. \emph{Japanese Psychological
#'   Research}, 22(3), 144-149.
#' @examples
#' a <- c(1.0, 1.2, 0.8); b <- c(-0.5, 0.2, 1.1)
#' r <- morie_equating_haebara(a, b, a / 1.3, (b - 0.4) / 1.3)
#' round(c(r$A, r$B), 3)
#' @export
morie_equating_haebara <- function(a_ref, b_ref, a_focal, b_focal,
                                   n_quad = 41, theta_range = 4) {
  out <- .morie_irt_link(a_ref, b_ref, a_focal, b_focal, n_quad, theta_range,
                         aggregate_first = FALSE)
  out$method <- "equating_haebara"
  out
}


#' Stocking-Lord IRT linking
#'
#' As Haebara, but differencing the SUMMED test characteristic curves
#' before squaring.
#'
#' That single change is the whole distinction and it has a consequence:
#' Stocking-Lord permits item-level misfits to cancel, since only the
#' test total has to match. It is the right criterion when the test
#' score is what will be reported and the wrong one when individual item
#' parameters will be used downstream.
#'
#' @inheritParams morie_equating_haebara
#' @return as \code{\link{morie_equating_haebara}}.
#' @references Stocking, M. L. and Lord, F. M. (1983). Developing a
#'   common metric in item response theory. \emph{Applied Psychological
#'   Measurement}, 7(2), 201-210.
#' @examples
#' a <- c(1.0, 1.2, 0.8); b <- c(-0.5, 0.2, 1.1)
#' r <- morie_equating_stocking_lord(a, b, a / 1.3, (b - 0.4) / 1.3)
#' round(c(r$A, r$B), 3)
#' @export
morie_equating_stocking_lord <- function(a_ref, b_ref, a_focal, b_focal,
                                         n_quad = 41, theta_range = 4) {
  out <- .morie_irt_link(a_ref, b_ref, a_focal, b_focal, n_quad, theta_range,
                         aggregate_first = TRUE)
  out$method <- "equating_stocking_lord"
  out
}
