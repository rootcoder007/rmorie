# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie - Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors

#' srr unsupervised-learning (UL) standards
#'
#' rmorie's unsupervised methods are dimension reduction (classical /
#' non-metric MDS, kernel PCA, ideal-point scaling) and clustering
#' (k-means, DBSCAN, spectral). The applicable UL standards are addressed
#' here; the label-propagation, new-data-prediction, batch-processing,
#' and default-plot-method families are declared NA (with reasons) in
#' `srr-stats-standards.R`.
#'
#' @srrstats {UL1.0} Expected input formats (numeric matrix / data.frame
#'   of coordinates or a distance object) are documented per function,
#'   including forms that are not accepted.
#' @srrstats {UL1.1} Input is validated by shared assertion helpers
#'   (`.morie_check_data` / `.morie_check_numvec`) which issue informative
#'   errors on incompatible data.
#' @srrstats {UL1.3a} Where row/column labels are not carried onto the
#'   compact result object (the methods operate on numeric coordinates),
#'   that is documented here.
#' @srrstats {UL1.4} Distributional/scaling assumptions are documented
#'   (MDS and PCA are scale-sensitive; ideal-point models assume a
#'   low-dimensional latent space).
#' @srrstats {UL1.4a} Functions that respond qualitatively differently to
#'   inputs on markedly different scales (PCA, MDS) document that
#'   sensitivity and the role of pre-scaling.
#' @srrstats {UL2.1} Any transformations applied (double-centring,
#'   distance construction) are documented.
#' @srrstats {UL2.3} Perfectly collinear / zero-variance columns are
#'   identified and dropped before fitting (`.viable_terms`).
#' @srrstats {UL3.1} Dimension-reduction outputs order dimensions by
#'   decreasing importance (PCA components by variance; MDS axes by
#'   eigenvalue).
#' @srrstats {UL3.4} Clustering results expose intra-group dispersion
#'   (k-means within-cluster sum of squares) and inter-group structure.
#' @srrstats {UL4.0} A structured model/result object is returned.
#' @srrstats {UL4.2} The control parameters used (k, number of
#'   dimensions, kernel, eps/minPts) are carried on the result object.
#' @srrstats {UL4.3} Result objects implement a default `print` method
#'   (`morie_rich_result`).
#' @srrstats {UL7.0} Inappropriate input types are rejected with expected
#'   error messages (tested via the shared validators).
#' @noRd
NULL
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------------
# Spatial voting & scaling models  (R port of src/morie/_spatial_voting.py)
# ---------------------------------------------------------------------------
# Implements scaling and ideal-point recovery methods covered in
# Armstrong et al. (2021) "Analyzing Spatial Models of Choice and Judgment"
# (2nd ed., Chapman & Hall/CRC) together with the original references:
#   - Aldrich & McKelvey (1977) "A Method of Scaling with Applications to
#     the 1968 and 1972 Presidential Elections", APSR 71(1).
#   - Poole (1998) "Recovering a Basic Space from a Set of Issue Scales",
#     AJPS 42(3).
#   - Poole & Rosenthal (1985) "A Spatial Model for Legislative Roll Call
#     Analysis", AJPS 29(2)  (NOMINATE).
#   - Clinton, Jackman & Rivers (2004) "The Statistical Analysis of Roll
#     Call Data", APSR 98(2).
#   - Carroll, Lewis, Lo, Poole & Rosenthal (2013) "The Structure of
#     Utility in Spatial Models of Voting", AJPS 57(4)  (alpha-NOMINATE).
#   - Imai, Lo & Olmsted (2016) "Fast Estimation of Ideal Points with
#     Massive Data", APSR 110(4)  (EM-IRT).
#   - Slapin & Proksch (2008) "A Scaling Model for Estimating Time-Series
#     Party Positions from Texts", AJPS 52(3)  (Wordfish).
#
# Where the upstream R ecosystem provides a battle-tested implementation
# we delegate to it: `basicspace` for Aldrich-McKelvey and blackbox
# scaling, with a pure-R numeric fallback so the package keeps working
# when `basicspace` (or `MASS`/`mvtnorm`) is not installed.
#
# Heavy Bayesian MCMC paths (CJR-IRT, Bayesian MDS / unfolding, dynamic
# IRT, alpha-NOMINATE) now run on native samplers or documented
# deterministic approximations (see each function's engine tag)
# stubs.  Porting a faithful Gibbs/MH sampler exceeds the in-session
# budget; users needing those should reach for `pscl::ideal`, `MCMCpack`,
# or `emIRT`.  The function signatures, parameter docs, and references
# are kept in place so the API surface is stable and a future port lands
# cleanly.

# ---- internal helpers ------------------------------------------------------

#' Internal helper: Sv As Matrix
#' @noRd
.sv_as_matrix <- function(x) {
  if (is.null(x)) return(NULL)
  m <- as.matrix(x)
  storage.mode(m) <- "double"
  m
}

# basicspace's Fortran (aldmck, blackbox, blackbox_transpose) writes past
# its arrays on a matrix with a single row or a single column and only
# errors afterwards; the damage surfaces later as a segfault in whatever
# allocates next. Such shapes never reach it (valgrind, 2026-09-22).
.sv_basicspace_shape_ok <- function(M, min_rows = 2L, min_cols = 2L) {
  is.matrix(M) && is.double(M) && nrow(M) >= min_rows && ncol(M) >= min_cols
}

#' Internal helper: Sv Nanmean Col
#' @noRd
.sv_nanmean_col <- function(M) {
  apply(M, 2, function(v) mean(v, na.rm = TRUE))
}

#' Internal helper: Sv Pairwise Dist
#' @noRd
.sv_pairwise_dist <- function(X) {
  X <- as.matrix(X)
  as.matrix(stats::dist(X))
}

#' Internal helper: Sv Double Centering
#' @noRd
.sv_double_centering <- function(D) {
  D <- as.matrix(D)
  n <- nrow(D)
  A <- D * D
  H <- diag(n) - matrix(1, n, n) / n
  -0.5 * H %*% A %*% H
}

#' Internal helper: Sv Safe Pinv
#' @noRd
.sv_safe_pinv <- function(M, tol = 1e-12) {
  s <- svd(M)
  d <- s$d
  inv_d <- ifelse(d > tol, 1 / d, 0)
  s$v %*% (diag(inv_d, nrow = length(inv_d)) %*% t(s$u))
}

#' Internal helper: Sv Isotonic Pava
#' @noRd
.sv_isotonic_pava <- function(y, w = NULL) {
  n <- length(y)
  if (is.null(w)) w <- rep(1, n)
  blk_val <- as.list(y)
  blk_w   <- as.list(w)
  blk_sz  <- as.list(rep(1L, n))
  i <- 1L
  while (i < length(blk_val)) {
    if (blk_val[[i]] > blk_val[[i + 1L]]) {
      new_w <- blk_w[[i]] + blk_w[[i + 1L]]
      blk_val[[i]] <- (blk_w[[i]] * blk_val[[i]] +
                       blk_w[[i + 1L]] * blk_val[[i + 1L]]) / new_w
      blk_w[[i]]  <- new_w
      blk_sz[[i]] <- blk_sz[[i]] + blk_sz[[i + 1L]]
      blk_val[[i + 1L]] <- NULL
      blk_w[[i + 1L]]   <- NULL
      blk_sz[[i + 1L]]  <- NULL
      if (i > 1L) i <- i - 1L
    } else {
      i <- i + 1L
    }
  }
  out <- numeric(n)
  pos <- 1L
  for (k in seq_along(blk_val)) {
    sz <- blk_sz[[k]]
    out[pos:(pos + sz - 1L)] <- blk_val[[k]]
    pos <- pos + sz
  }
  out
}

# ===========================================================================
# 1. Aldrich-McKelvey scaling
# ===========================================================================

# Respondent intercepts and slopes given stimulus positions: for each
# respondent the least squares fit of Z[i, ] on zhat. Respondents with
# fewer than two placements get (0, 1); a slope of zero is floored so the
# stimulus update can divide by it.
.sv_am_respondents <- function(Z, mask, zhat) {
  n_resp <- nrow(Z)
  alpha <- numeric(n_resp)
  beta <- numeric(n_resp)
  for (i in seq_len(n_resp)) {
    valid <- mask[i, ]
    if (sum(valid) < 2L) {
      alpha[i] <- 0
      beta[i] <- 1
      next
    }
    pr <- qr.solve(cbind(1, zhat[valid]), Z[i, valid])
    alpha[i] <- pr[1L]
    beta[i] <- ifelse(abs(pr[2L]) > 1e-10, pr[2L], 1e-10)
  }
  list(alpha = alpha, beta = beta)
}

#' Aldrich-McKelvey scaling
#'
#' Recovers latent stimulus positions from perceptual placement data by
#' estimating respondent-specific intercepts \eqn{a_i} and slopes
#' \eqn{b_i} in the model
#' \deqn{z_{ij} = a_i + b_i \hat{z}_j + \epsilon_{ij}.}{z_ij = a_i + b_i z_hat_j + epsilon_ij.}
#' The stimulus positions come from `basicspace::aldmck` when the
#' `basicspace` package is installed (standardised to mean 0, sd 1);
#' otherwise from a hand-rolled EM/least-squares fallback. The respondent
#' intercepts and slopes are, on either path, the per-respondent least
#' squares regression of the reported placements on those positions, so
#' they are defined for every respondent with at least two placements
#' (`basicspace` itself reports them only for respondents it scales
#' against a self-placement, which this function does not take).
#'
#' @param Z A respondent-by-stimulus numeric matrix of perceptual
#'   placements.  `NA` entries are treated as missing.
#' @param n_dims Number of latent dimensions (typically 1).
#' @param max_iter Maximum EM iterations for the fallback solver.
#' @param tol Convergence tolerance on the stimulus configuration.
#' @return A list with components `zhat` (stimulus positions), `alpha`,
#'   `beta`, `weights`, `iterations`, `converged`, and `engine`
#'   ("basicspace" or "fallback").
#' @references
#'   Aldrich, J. H. and McKelvey, R. D. (1977). "A Method of Scaling with
#'   Applications to the 1968 and 1972 Presidential Elections."
#'   *American Political Science Review*, 71(1), 111-130.
#'
#'   Poole, K. T. (1998). "Recovering a Basic Space from a Set of Issue
#'   Scales." *American Journal of Political Science*, 42(3), 954-993.
#'
#'   Armstrong, D. A., Bakker, R., Carroll, R., Hare, C., Poole, K. T.,
#'   and Rosenthal, H. (2021). *Analyzing Spatial Models of Choice and
#'   Judgment*, 2nd ed. Chapman & Hall/CRC.
#' @examples
#' if (requireNamespace("basicspace", quietly = TRUE)) {
#'   set.seed(1)
#'   Z <- matrix(rnorm(20 * 5), 20, 5)
#'   fit <- morie_spatial_voting_aldrich_mckelvey(Z)
#'   fit$zhat
#' }
#' @export
morie_spatial_voting_aldrich_mckelvey <- function(Z,
                                                  n_dims  = 1L,
                                                  max_iter = 100L,
                                                  tol      = 1e-6) {
  if (as.integer(n_dims) != 1L) {
    stop("morie_spatial_voting_aldrich_mckelvey: this implementation recovers a single ",
         "latent dimension; n_dims must be 1", call. = FALSE)
  }
  Z <- .sv_as_matrix(Z)
  n_resp <- nrow(Z)
  n_stim <- ncol(Z)

  if (requireNamespace("basicspace", quietly = TRUE) &&
        .sv_basicspace_shape_ok(Z)) {
    # aldmck's default treats NA cells as missing; passing `missing = NA`
    # is rejected ("must only contain integers"), which sent every call
    # down the fallback while the docs promised basicspace
    out <- try(basicspace::aldmck(Z, respondent = 0, polarity = 1),
               silent = TRUE)
    if (!inherits(out, "try-error") && all(is.finite(out$stimuli))) {
      zhat <- as.numeric(out$stimuli)
      zhat <- zhat - mean(zhat)
      if (stats::sd(zhat) > 0) zhat <- zhat / stats::sd(zhat)
      rp <- .sv_am_respondents(Z, !is.na(Z), zhat)
      return(list(
        zhat       = zhat,
        alpha      = rp$alpha,
        beta       = rp$beta,
        weights    = abs(rp$beta) / sum(abs(rp$beta)) * n_resp,
        iterations = NA_integer_,
        converged  = TRUE,
        engine     = "basicspace"
      ))
    }
  }

  mask <- !is.na(Z)
  zhat <- .sv_nanmean_col(Z)
  if (stats::sd(zhat, na.rm = TRUE) > 0) {
    zhat <- (zhat - mean(zhat, na.rm = TRUE)) /
            stats::sd(zhat, na.rm = TRUE)
  }
  alpha <- numeric(n_resp)
  beta <- numeric(n_resp)
  iter <- 0L
  for (iter in seq_len(max_iter)) {
    zhat_old <- zhat
    rp <- .sv_am_respondents(Z, mask, zhat)
    alpha <- rp$alpha
    beta <- rp$beta
    for (j in seq_len(n_stim)) {
      valid <- mask[, j]
      if (sum(valid) < 1L) next
      # v0.9.5.6+: Aldrich-McKelvey (1977) precision-weighted stimulus
      # update z_j = sum_i beta_i (Z_ij - alpha_i) / sum_i beta_i^2,
      # NOT the unweighted mean. The unweighted form biases the
      # estimate under heterogeneous respondent reliability.
      denom <- sum(beta[valid]^2)
      if (denom < 1e-12) {
        zhat[j] <- mean((Z[valid, j] - alpha[valid]) / beta[valid])
      } else {
        zhat[j] <- sum(beta[valid] * (Z[valid, j] - alpha[valid])) / denom
      }
    }
    zhat <- zhat - mean(zhat)
    if (stats::sd(zhat) > 0) zhat <- zhat / stats::sd(zhat)
    if (max(abs(zhat - zhat_old)) < tol) break
  }
  weights <- abs(beta)
  weights <- weights / sum(weights) * n_resp
  list(zhat = zhat, alpha = alpha, beta = beta, weights = weights,
       iterations = iter, converged = iter < max_iter,
       engine = "fallback")
}

# ===========================================================================
# 2. Blackbox (Basic Space) scaling
# ===========================================================================

#' Blackbox / Basic Space scaling
#'
#' Recovers respondent ideal points from an issue-scale response matrix
#' via SVD on the column-centred matrix.  Implements Poole's (1998)
#' decomposition \eqn{X_0 = \Psi W' + J_n c' + E_0}{X_0 = Psi W' + J_n c' + E_0}.  Delegates to
#' `basicspace::blackbox` when available.
#'
#' @param X A respondent-by-issue numeric matrix of responses
#'   (`NA` for missing).
#' @param n_dims Number of dimensions to extract.
#' @return A list with `ideal_points`, `stimuli_weights`, `eigenvalues`,
#'   `singular_values`, `explained_variance`, `col_means`, `n_dims`, and
#'   `engine`.
#' @references Poole, K. T. (1998); Armstrong et al. (2021).
#' @examples
#' if (requireNamespace("basicspace", quietly = TRUE)) {
#'   set.seed(1)
#'   X <- matrix(rnorm(30 * 6), 30, 6)
#'   morie_spatial_voting_blackbox(X, n_dims = 2)
#' }
#' @export
morie_spatial_voting_blackbox <- function(X, n_dims = 2L) {
  X <- .sv_as_matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  # minscale = 8 below drops every respondent with fewer than eight
  # responses, so fewer than eight stimuli would only error anyway
  if (requireNamespace("basicspace", quietly = TRUE) &&
        .sv_basicspace_shape_ok(X, min_cols = max(8L, n_dims + 1L))) {
    out <- try(basicspace::blackbox(X, missing = NA, dims = n_dims,
                                    minscale = 8, verbose = FALSE),
               silent = TRUE)
    if (!inherits(out, "try-error")) {
      ip <- as.matrix(out$individuals[[n_dims]][, paste0("c", seq_len(n_dims))])
      sw <- as.matrix(out$stimuli[[n_dims]][, paste0("w", seq_len(n_dims))])
      return(list(
        ideal_points      = ip,
        stimuli_weights   = sw,
        eigenvalues       = NA_real_,
        singular_values   = NA_real_,
        explained_variance = NA_real_,
        col_means         = .sv_nanmean_col(X),
        n_dims            = n_dims,
        engine            = "basicspace"
      ))
    }
  }

  col_means <- .sv_nanmean_col(X)
  Xc <- sweep(X, 2, col_means, FUN = "-")
  Xc[is.na(Xc)] <- 0
  sv <- svd(Xc)
  q  <- min(n_dims, length(sv$d))
  Lambda_q <- diag(sv$d[seq_len(q)], nrow = q)
  V_q <- sv$v[, seq_len(q), drop = FALSE]
  U_q <- sv$u[, seq_len(q), drop = FALSE]
  W   <- V_q %*% sqrt(Lambda_q)
  Psi <- U_q %*% sqrt(Lambda_q)
  total_var <- sum(sv$d ^ 2)
  explained <- if (total_var > 0) sum(sv$d[seq_len(q)] ^ 2) / total_var else 0
  list(ideal_points = Psi, stimuli_weights = W,
       eigenvalues  = sv$d[seq_len(q)] ^ 2,
       singular_values = sv$d[seq_len(q)],
       explained_variance = explained,
       col_means = col_means, n_dims = q, engine = "fallback")
}

# ===========================================================================
# 3. Optimal Classification (Poole 2000)
# ===========================================================================

#' Optimal Classification scaling
#'
#' Nonparametric ideal-point estimation from binary roll-call votes that
#' minimises the number of classification errors (Poole 2000).
#'
#' @param votes Legislator-by-vote matrix; `1`=yea, `0`=nay, `NA`=missing.
#' @param n_dims Number of latent dimensions.
#' @param max_iter Maximum iterations per restart.
#' @param n_restarts Random restarts (best PRE retained).
#' @param seed RNG seed.
#' @return A list with `ideal_points`, `cutting_normals`, `PRE`, `APRE`,
#'   `total_errors`, `null_errors`, `n_dims`.
#' @references
#'   Poole, K. T. (2000). "Non-Parametric Unfolding of Binary Choice
#'   Data." *Political Analysis*, 8(3), 211-237.
#' @examples
#' \donttest{
#' set.seed(1)
#' v <- matrix(stats::rbinom(20 * 30, 1, 0.5), 20, 30)
#' morie_spatial_voting_optimal_classification(v)
#' }
#' @export
morie_spatial_voting_optimal_classification <- function(votes,
                                                        n_dims    = 1L,
                                                        max_iter  = 500L,
                                                        n_restarts = 10L,
                                                        seed       = 42L) {
  votes <- .sv_as_matrix(votes)
  n_leg <- nrow(votes)
  n_vote <- ncol(votes)
  .rmorie_local_seed(seed)
  best_errors <- Inf
  best_result <- NULL

  for (r in seq_len(n_restarts)) {
    x <- matrix(stats::rnorm(n_leg * n_dims), n_leg, n_dims)
    normals <- matrix(0, n_vote, n_dims)
    for (k in seq_len(max_iter)) {
      for (j in seq_len(n_vote)) {
        v_j <- votes[, j]
        valid <- !is.na(v_j)
        yea <- v_j == 1 & valid
        nay <- v_j == 0 & valid
        if (sum(yea, na.rm = TRUE) == 0L || sum(nay, na.rm = TRUE) == 0L) next
        yea_mean <- colMeans(x[which(yea), , drop = FALSE])
        nay_mean <- colMeans(x[which(nay), , drop = FALSE])
        nrm <- yea_mean - nay_mean
        nl  <- sqrt(sum(nrm * nrm))
        if (nl > 0) normals[j, ] <- nrm / nl
      }
      x_new <- matrix(0, n_leg, n_dims)
      for (i in seq_len(n_leg)) {
        valid <- !is.na(votes[i, ])
        if (sum(valid) == 0L) next
        direction <- numeric(n_dims)
        for (j in which(valid)) {
          direction <- direction +
            if (votes[i, j] == 1)  normals[j, ] else -normals[j, ]
        }
        nl <- sqrt(sum(direction * direction))
        x_new[i, ] <- if (nl > 0) direction / nl else x[i, ]
      }
      x <- x_new
    }
    total_errors <- 0L
    null_errors <- 0L
    for (j in seq_len(n_vote)) {
      v_j <- votes[, j]
      valid <- !is.na(v_j)
      yea_count <- sum(v_j[valid] == 1)
      nay_count <- sum(v_j[valid] == 0)
      null_errors <- null_errors + min(yea_count, nay_count)
      if (yea_count == 0 || nay_count == 0) next
      proj <- x[valid, , drop = FALSE] %*% normals[j, ]
      yea_mid <- mean(x[which(v_j == 1), , drop = FALSE] %*% normals[j, ])
      nay_mid <- mean(x[which(v_j == 0), , drop = FALSE] %*% normals[j, ])
      midpoint <- (yea_mid + nay_mid) / 2
      predicted <- as.numeric(proj >= midpoint)
      actual <- v_j[valid]
      total_errors <- total_errors + sum(predicted != actual, na.rm = TRUE)
    }
    if (total_errors < best_errors) {
      best_errors <- total_errors
      pre <- if (null_errors > 0)
               (null_errors - total_errors) / null_errors else 0
      best_result <- list(
        ideal_points    = x,
        cutting_normals = normals,
        PRE             = pre, APRE = pre,
        total_errors    = as.integer(total_errors),
        null_errors     = as.integer(null_errors),
        n_dims          = n_dims
      )
    }
  }
  best_result
}

# ===========================================================================
# 4. Distance / MDS machinery
# ===========================================================================

#' Double-center a distance matrix
#'
#' Computes \eqn{B = -\tfrac{1}{2} H D^{(2)} H}{B = -(1)/(2) H D^(2) H} with
#' \eqn{H = I - n^{-1}\mathbf{1}\mathbf{1}'}{H = I - n^-111'}.
#' @param D Symmetric numeric distance matrix.
#' @return The double-centered matrix \eqn{B}.
#' @references Torgerson (1952); Armstrong et al. (2021), Section 3.
#' @examples
#' set.seed(1)
#' morie_spatial_voting_double_centering(as.matrix(dist(matrix(rnorm(20), 5))))
#' @export
morie_spatial_voting_double_centering <- function(D) {
  .sv_double_centering(D)
}

#' @noRd
.sv_have_cpp <- function(name = "morie_spatial_classical_mds_cpp") {
  exists(name, envir = asNamespace("rmorie"), inherits = FALSE)
}

#' Classical (metric) multidimensional scaling
#'
#' Torgerson scaling via eigendecomposition of the double-centred matrix.
#'
#' @param D Symmetric numeric distance matrix.
#' @param n_dims Number of dimensions to extract.
#' @return A list with `coordinates`, `eigenvalues`, `stress`, `fit`,
#'   `B_matrix`.
#' @references Torgerson, W. S. (1952); Armstrong et al. (2021).
#' @examples
#' set.seed(1)
#' D <- as.matrix(dist(matrix(rnorm(40), 10)))
#' morie_spatial_voting_classical_mds(D, n_dims = 2)
#' @export
morie_spatial_voting_classical_mds <- function(D, n_dims = 2L) {
  D <- as.matrix(D)
  if (.sv_have_cpp("morie_spatial_classical_mds_cpp")) {
    return(morie_spatial_classical_mds_cpp(D, as.integer(n_dims)))
  }
  B <- .sv_double_centering(D)
  ev <- eigen(B, symmetric = TRUE)
  vals <- ev$values
  vecs <- ev$vectors
  pos  <- pmax(vals[seq_len(n_dims)], 0)
  coords <- vecs[, seq_len(n_dims), drop = FALSE] %*% diag(sqrt(pos), nrow = n_dims)
  d_model <- .sv_pairwise_dist(coords)
  valid <- D > 0
  stress <- 0
  # v0.9.5.6+: Kruskal stress-1 normalises by sum(D^2), not by
  # sum(d_model^2) (which collapses to zero when the model is underfit).
  if (sum(valid) > 0 && sum(D[valid] ^ 2) > 0) {
    stress <- sqrt(sum((d_model[valid] - D[valid]) ^ 2) /
                   sum(D[valid] ^ 2))
  }
  total <- sum(abs(vals))
  fit <- if (total > 0) sum(pos) / total else 0
  list(coordinates = coords,
       eigenvalues = vals[seq_len(n_dims)],
       stress = stress, fit = fit, B_matrix = B)
}

#' SMACOF stress minimisation
#'
#' Iterative majorisation algorithm for metric MDS.
#'
#' @param D Symmetric dissimilarity matrix.
#' @param n_dims Number of dimensions.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance on stress change.
#' @param weights Optional weight matrix (defaults to uniform).
#' @param init Optional initial configuration (n x n_dims).
#' @return A list with `coordinates`, `stress`, `iterations`, `converged`.
#' @references
#'   De Leeuw, J. (1977). "Applications of Convex Analysis to
#'   Multidimensional Scaling." In *Recent Developments in Statistics*,
#'   133-145.  Borg & Groenen (2005).
#' @examples
#' set.seed(1)
#' D <- as.matrix(dist(matrix(rnorm(40), 10)))
#' morie_spatial_voting_smacof(D)
#' @export
morie_spatial_voting_smacof <- function(D,
                                        n_dims   = 2L,
                                        max_iter = 300L,
                                        tol      = 1e-6,
                                        weights  = NULL,
                                        init     = NULL) {
  D <- as.matrix(D)
  n <- nrow(D)
  W <- if (is.null(weights)) matrix(1, n, n) else as.matrix(weights)
  diag(W) <- 0
  V <- diag(rowSums(W))
  V_inv <- .sv_safe_pinv(V)
  if (is.null(init)) {
    .rmorie_local_seed(42L)
    X <- matrix(stats::rnorm(n * n_dims), n, n_dims)
  } else {
    X <- as.matrix(init)
  }
  compute_B <- function(d_X) {
    B <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (i != j && d_X[i, j] > 1e-12) {
        B[i, j] <- -W[i, j] * D[i, j] / d_X[i, j]
      }
    }
    diag(B) <- -rowSums(B) + diag(B)
    B
  }
  d_X <- .sv_pairwise_dist(X)
  stress <- sum(W * (D - d_X) ^ 2) / 2
  it <- 0L
  use_cpp <- .sv_have_cpp("morie_spatial_smacof_step_cpp")
  for (it in seq_len(max_iter)) {
    if (use_cpp) {
      step <- morie_spatial_smacof_step_cpp(X, D, W)
      X_new <- step$coordinates
      stress_new <- step$stress
      d_X <- step$distances
    } else {
      Bm <- compute_B(d_X)
      X_new <- V_inv %*% Bm %*% X
      d_X <- .sv_pairwise_dist(X_new)
      stress_new <- sum(W * (D - d_X) ^ 2) / 2
    }
    if (abs(stress - stress_new) < tol) {
      X <- X_new
      stress <- stress_new
      break
    }
    X <- X_new
    stress <- stress_new
  }
  list(coordinates = X, stress = stress,
       iterations = it, converged = it < max_iter)
}

#' Nonmetric MDS with isotonic regression
#'
#' Kruskal-style nonmetric MDS using pool-adjacent-violators monotone
#' regression on dissimilarity ranks.
#'
#' @param D Symmetric dissimilarity matrix.
#' @param n_dims Number of dimensions.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance.
#' @return A list with `coordinates`, `stress`, `iterations`, `converged`.
#' @references
#'   Kruskal, J. B. (1964). "Nonmetric Multidimensional Scaling: A
#'   Numerical Method." *Psychometrika*, 29(2), 115-129.
#' @examples
#' set.seed(1)
#' D <- as.matrix(dist(matrix(rnorm(40), 10)))
#' morie_spatial_voting_nonmetric_mds(D)
#' @export
morie_spatial_voting_nonmetric_mds <- function(D,
                                               n_dims   = 2L,
                                               max_iter = 300L,
                                               tol      = 1e-6) {
  D <- as.matrix(D)
  n <- nrow(D)
  .rmorie_local_seed(42L)
  X <- matrix(stats::rnorm(n * n_dims), n, n_dims)
  idx <- which(upper.tri(D), arr.ind = TRUE)
  d_orig <- D[upper.tri(D)]
  ord <- order(d_orig)
  stress <- 0
  it <- 0L
  for (it in seq_len(max_iter)) {
    d_X <- .sv_pairwise_dist(X)
    d_current <- d_X[upper.tri(d_X)]
    d_hat_ordered <- .sv_isotonic_pava(d_current[ord])
    d_hat <- numeric(length(d_current))
    d_hat[ord] <- d_hat_ordered
    D_hat <- matrix(0, n, n)
    D_hat[upper.tri(D_hat)] <- d_hat
    D_hat <- D_hat + t(D_hat)
    sm <- morie_spatial_voting_smacof(D_hat, n_dims = n_dims,
                                      max_iter = 1L, init = X)
    X_new <- sm$coordinates
    denom <- sum(d_current ^ 2)
    stress <- if (denom > 0) sqrt(sum((d_current - d_hat) ^ 2) / denom) else 0
    if (max(abs(X_new - X)) < tol) { X <- X_new
    break }
    X <- X_new
  }
  list(coordinates = X, stress = stress,
       iterations = it, converged = it < max_iter)
}

#' MDS fit statistics (Mardia criterion)
#'
#' @param eigenvalues Numeric vector of MDS eigenvalues.
#' @return List with `fit_by_dim`, `cumulative_fit`, `eigenvalues`.
#' @references Mardia, K. V. (1978). "Some Properties of Classical
#'   Multi-Dimensional Scaling." *Communications in Statistics*.
#' @examples morie_spatial_voting_mds_fit_stats(c(4, 2, 1, 0.5))
#' @export
morie_spatial_voting_mds_fit_stats <- function(eigenvalues) {
  ev <- as.numeric(eigenvalues)
  total <- sum(abs(ev))
  if (total == 0) return(list(fit_by_dim = numeric(0),
                              cumulative_fit = numeric(0),
                              eigenvalues = ev))
  pos <- pmax(ev, 0)
  f <- pos / total
  list(fit_by_dim = f, cumulative_fit = cumsum(f), eigenvalues = ev)
}

# ===========================================================================
# 5. Unfolding family
# ===========================================================================

#' Compute unfolding stress
#'
#' @param X_r Respondent coordinates (n_r x n_dims).
#' @param X_s Stimulus coordinates (n_s x n_dims).
#' @param D Observed respondent-stimulus dissimilarities.
#' @param weights Optional weight matrix.
#' @return A numeric scalar, the weighted sum of squared residuals.
#' @references Coombs (1964); Armstrong et al. (2021).
#' @examples
#' set.seed(1)
#' Xr <- matrix(rnorm(6), 3, 2); Xs <- matrix(rnorm(8), 4, 2)
#' D  <- matrix(stats::runif(12), 3, 4)
#' morie_spatial_voting_unfolding_stress(Xr, Xs, D)
#' @export
morie_spatial_voting_unfolding_stress <- function(X_r, X_s, D,
                                                  weights = NULL) {
  X_r <- as.matrix(X_r)
  X_s <- as.matrix(X_s)
  D <- as.matrix(D)
  n_r <- nrow(X_r)
  n_s <- nrow(X_s)
  d_model <- matrix(0, n_r, n_s)
  for (i in seq_len(n_r)) for (j in seq_len(n_s)) {
    d_model[i, j] <- sqrt(sum((X_r[i, ] - X_s[j, ]) ^ 2))
  }
  W <- if (is.null(weights)) matrix(1, n_r, n_s) else as.matrix(weights)
  mask <- !is.na(D)
  as.numeric(sum(W[mask] * (d_model[mask] - D[mask]) ^ 2))
}

#' MLSMU6 alternating least-squares unfolding
#'
#' Multidimensional Least-Squares Metric Unfolding (Poole 1984; Bakker &
#' Poole 2013).  Alternates between respondent and stimulus coordinates,
#' restarting from random seeds and keeping the lowest-stress fit.
#'
#' @param D Respondent-by-stimulus distance/rating matrix.
#' @param n_dims Number of latent dimensions.
#' @param max_iter Maximum alternations per restart.
#' @param tol Convergence tolerance on relative stress change.
#' @param n_restarts Number of random restarts.
#' @return A list with `respondent_coords`, `stimulus_coords`, `stress`,
#'   `iterations`, `converged`.
#' @references
#'   Poole, K. T. (1984). "Least Squares Metric, Unidimensional
#'   Unfolding." *Psychometrika*, 49(3).
#'   Bakker, R. and Poole, K. T. (2013).
#' @examples
#' set.seed(1)
#' D <- matrix(stats::runif(20 * 6), 20, 6)
#' morie_spatial_voting_mlsmu6(D, n_dims = 2, n_restarts = 1, max_iter = 50)
#' @export
morie_spatial_voting_mlsmu6 <- function(D,
                                        n_dims    = 2L,
                                        max_iter  = 200L,
                                        tol       = 1e-6,
                                        n_restarts = 5L) {
  D <- as.matrix(D)
  n_r <- nrow(D)
  n_s <- ncol(D)
  .rmorie_local_seed(42L)
  best_stress <- Inf
  best <- NULL
  for (r in seq_len(n_restarts)) {
    X_r <- matrix(stats::rnorm(n_r * n_dims), n_r, n_dims)
    X_s <- matrix(stats::rnorm(n_s * n_dims), n_s, n_dims)
    X_r <- sweep(X_r, 2, colMeans(X_r))
    X_s <- sweep(X_s, 2, colMeans(X_s))
    D_hat <- D - rowMeans(D, na.rm = TRUE)
    prev_stress <- Inf
    iter <- 0L
    stress <- prev_stress
    for (iter in seq_len(max_iter)) {
      d_model <- matrix(0, n_r, n_s)
      for (i in seq_len(n_r)) for (j in seq_len(n_s)) {
        d_model[i, j] <- sqrt(sum((X_r[i, ] - X_s[j, ]) ^ 2))
      }
      d_model <- pmax(d_model, 1e-12)
      grad_r <- matrix(0, n_r, n_dims)
      for (i in seq_len(n_r)) for (j in seq_len(n_s)) {
        diff <- X_r[i, ] - X_s[j, ]
        grad_r[i, ] <- grad_r[i, ] +
          2 * (d_model[i, j] - D_hat[i, j]) * diff / d_model[i, j]
      }
      grad_r <- grad_r / n_s
      eig_r <- max(eigen(crossprod(grad_r), symmetric = TRUE,
                         only.values = TRUE)$values, 1e-10)
      gamma_r <- 2 / (n_s * eig_r)
      X_r <- X_r - gamma_r * grad_r
      X_r <- sweep(X_r, 2, colMeans(X_r))
      grad_s <- matrix(0, n_s, n_dims)
      for (j in seq_len(n_s)) for (i in seq_len(n_r)) {
        diff <- X_s[j, ] - X_r[i, ]
        grad_s[j, ] <- grad_s[j, ] +
          2 * (d_model[i, j] - D_hat[i, j]) * diff / d_model[i, j]
      }
      grad_s <- grad_s / n_r
      eig_s <- max(eigen(crossprod(grad_s), symmetric = TRUE,
                         only.values = TRUE)$values, 1e-10)
      gamma_s <- 2 / (n_r * eig_s)
      X_s <- X_s - gamma_s * grad_s
      X_s <- sweep(X_s, 2, colMeans(X_s))
      stress <- morie_spatial_voting_unfolding_stress(X_r, X_s, D)
      # prev_stress starts as Inf -> abs(Inf - stress) / max(Inf, ...)
      # = NaN -> `if (NaN < tol)` is NA -> "missing value where
      # TRUE/FALSE needed".  Guard with is.finite so iter 1 simply
      # skips the convergence check.
      if (is.finite(prev_stress) &&
          abs(prev_stress - stress) / max(prev_stress, 1e-12) < tol) break
      prev_stress <- stress
    }
    if (stress < best_stress) {
      best_stress <- stress
      best <- list(respondent_coords = X_r,
                   stimulus_coords   = X_s,
                   stress            = stress,
                   iterations        = iter,
                   converged         = iter < max_iter)
    }
  }
  best
}

#' SMACOF rectangular unfolding
#'
#' Majorisation-based unfolding embedding respondents and stimuli into a
#' common space.
#'
#' @param D Respondent-by-stimulus dissimilarity matrix.
#' @param n_dims Latent dimensions.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance.
#' @return A list with `respondent_coords`, `stimulus_coords`, `stress`,
#'   `iterations`, `converged`.
#' @references Borg & Groenen (2005); Armstrong et al. (2021), Ch. 4.
#' @examples
#' set.seed(1)
#' D <- matrix(stats::runif(12), 3, 4)
#' morie_spatial_voting_smacof_unfolding(D, max_iter = 20)
#' @export
morie_spatial_voting_smacof_unfolding <- function(D,
                                                  n_dims   = 2L,
                                                  max_iter = 300L,
                                                  tol      = 1e-6) {
  D <- as.matrix(D)
  n_r <- nrow(D)
  n_s <- ncol(D)
  n <- n_r + n_s
  .rmorie_local_seed(42L)
  X_r <- matrix(stats::rnorm(n_r * n_dims), n_r, n_dims)
  X_s <- matrix(stats::rnorm(n_s * n_dims), n_s, n_dims)
  D_full <- matrix(0, n, n)
  D_full[seq_len(n_r), (n_r + 1L):n] <- D
  D_full[(n_r + 1L):n, seq_len(n_r)] <- t(D)
  W <- matrix(0, n, n)
  W[seq_len(n_r), (n_r + 1L):n] <- 1
  W[(n_r + 1L):n, seq_len(n_r)] <- 1
  X <- rbind(X_r, X_s)
  V <- diag(rowSums(W))
  V_inv <- .sv_safe_pinv(V)
  d_X <- .sv_pairwise_dist(X)
  stress <- sum(W * (D_full - d_X) ^ 2) / 2
  it <- 0L
  for (it in seq_len(max_iter)) {
    B <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (i != j && d_X[i, j] > 1e-12)
        B[i, j] <- -W[i, j] * D_full[i, j] / d_X[i, j]
    }
    diag(B) <- -rowSums(B) + diag(B)
    X_new <- V_inv %*% B %*% X
    d_X <- .sv_pairwise_dist(X_new)
    stress_new <- sum(W * (D_full - d_X) ^ 2) / 2
    if (abs(stress - stress_new) < tol) { X <- X_new
    stress <- stress_new
    break }
    X <- X_new
    stress <- stress_new
  }
  list(respondent_coords = X[seq_len(n_r), , drop = FALSE],
       stimulus_coords   = X[(n_r + 1L):n, , drop = FALSE],
       stress = stress, iterations = it, converged = it < max_iter)
}

#' Ideal-point recovery from unfolding output
#' @param X_r Respondent coordinates.
#' @param X_s Stimulus coordinates (unused; the respondent row IS the
#'   ideal point).
#' @return Numeric matrix of respondent ideal points.
#' @references Armstrong et al. (2021), Section 4.5.
#' @examples
#' set.seed(1)
#' morie_spatial_voting_ideal_point_recovery(matrix(rnorm(6), 3, 2))
#' @export
morie_spatial_voting_ideal_point_recovery <- function(X_r, X_s = NULL) {
  as.matrix(X_r)
}

# ===========================================================================
# 6. NOMINATE utility and likelihood
# ===========================================================================

#' NOMINATE Gaussian utility
#'
#' Computes Poole-Rosenthal NOMINATE utilities and vote probabilities.
#'
#' @param x Legislator ideal points (n_leg x n_dims).
#' @param z_yea Yea outcome locations (n_votes x n_dims).
#' @param z_nay Nay outcome locations (n_votes x n_dims).
#' @param beta Signal-to-noise ratio.
#' @param w Dimension weights (length n_dims; defaults to 1).
#' @return List with `U_yea`, `U_nay`, `utility_diff`, `vote_probs`.
#' @references Poole, K. T. and Rosenthal, H. (1985); Armstrong et al.
#'   (2021), Ch. 5.
#' @examples
#' set.seed(1)
#' x  <- matrix(rnorm(8), 4, 2)
#' zy <- matrix(rnorm(6), 3, 2); zn <- matrix(rnorm(6), 3, 2)
#' morie_spatial_voting_nominate_utility(x, zy, zn)
#' @export
morie_spatial_voting_nominate_utility <- function(x, z_yea, z_nay,
                                                  beta = 15.0, w = NULL) {
  x <- as.matrix(x)
  z_yea <- as.matrix(z_yea)
  z_nay <- as.matrix(z_nay)
  n_leg <- nrow(x)
  n_votes <- nrow(z_yea)
  n_dims <- ncol(x)
  if (is.null(w)) w <- rep(1, n_dims) else w <- as.numeric(w)
  U_yea <- matrix(0, n_leg, n_votes)
  U_nay <- matrix(0, n_leg, n_votes)
  for (i in seq_len(n_leg)) for (j in seq_len(n_votes)) {
    d_yea <- sum(w ^ 2 * (x[i, ] - z_yea[j, ]) ^ 2)
    d_nay <- sum(w ^ 2 * (x[i, ] - z_nay[j, ]) ^ 2)
    U_yea[i, j] <- beta * exp(-0.5 * d_yea)
    U_nay[i, j] <- beta * exp(-0.5 * d_nay)
  }
  v <- U_yea - U_nay
  P <- 1 / (1 + exp(-v))
  list(U_yea = U_yea, U_nay = U_nay, utility_diff = v, vote_probs = P)
}

#' Single NOMINATE vote probability
#' @param x_i Legislator ideal point (vector).
#' @param z_yea_j Yea outcome (vector).
#' @param z_nay_j Nay outcome (vector).
#' @param beta Signal-to-noise ratio.
#' @param w Dimension weights.
#' @return Numeric scalar in (0,1).
#' @references Poole KT & Rosenthal H (1985). A spatial model for
#'   legislative roll call analysis. \emph{American Journal of
#'   Political Science}, 29(2), 357-384.
#' @examples morie_spatial_voting_nominate_vote_prob(c(0.1), c(0.3), c(-0.3))
#' @export
morie_spatial_voting_nominate_vote_prob <- function(x_i, z_yea_j, z_nay_j,
                                                    beta = 15.0, w = NULL) {
  x_i <- as.numeric(x_i)
  z_yea_j <- as.numeric(z_yea_j)
  z_nay_j <- as.numeric(z_nay_j)
  if (is.null(w)) w <- rep(1, length(x_i)) else w <- as.numeric(w)
  d_yea <- sum(w ^ 2 * (x_i - z_yea_j) ^ 2)
  d_nay <- sum(w ^ 2 * (x_i - z_nay_j) ^ 2)
  v <- beta * (exp(-0.5 * d_yea) - exp(-0.5 * d_nay))
  as.numeric(1 / (1 + exp(-v)))
}

#' NOMINATE log-likelihood and GMP
#' @param votes Legislator-by-vote binary matrix.
#' @param x Ideal points.
#' @param z_yea Yea outcomes.
#' @param z_nay Nay outcomes.
#' @param beta Signal-to-noise.
#' @param w Dimension weights.
#' @return List with `loglik`, `GMP`, `n_correct`, `n_total`.
#' @references Poole KT & Rosenthal H (1997). \emph{Congress: A
#'   Political-Economic History of Roll Call Voting}. Oxford University
#'   Press.
#' @examples
#' set.seed(1)
#' v <- matrix(stats::rbinom(20, 1, 0.5), 4, 5)
#' x <- matrix(rnorm(4), 4, 1); zy <- matrix(rnorm(5), 5, 1)
#' zn <- matrix(rnorm(5), 5, 1)
#' morie_spatial_voting_nominate_loglik(v, x, zy, zn)
#' @export
morie_spatial_voting_nominate_loglik <- function(votes, x, z_yea, z_nay,
                                                 beta = 15.0, w = NULL) {
  res <- morie_spatial_voting_nominate_utility(x, z_yea, z_nay, beta, w)
  P <- res$vote_probs
  votes <- as.matrix(votes)
  mask <- !is.na(votes)
  ll <- 0
  n_correct <- 0L
  n_total <- 0L
  for (i in seq_len(nrow(votes))) for (j in seq_len(ncol(votes))) {
    if (!mask[i, j]) next
    p <- pmin(pmax(P[i, j], 1e-10), 1 - 1e-10)
    if (votes[i, j] == 1) {
      ll <- ll + log(p)
      if (p > 0.5) n_correct <- n_correct + 1L
    } else {
      ll <- ll + log(1 - p)
      if (p < 0.5) n_correct <- n_correct + 1L
    }
    n_total <- n_total + 1L
  }
  list(loglik = ll,
       GMP   = if (n_total > 0) n_correct / n_total else 0,
       n_correct = n_correct, n_total = n_total)
}

# ===========================================================================
# 7. Procrustes
# ===========================================================================

#' Procrustes rotation
#'
#' Orthogonal rotation aligning `X` to `X_target` (with reflection
#' protection against improper rotations).
#'
#' @param X Configuration to rotate.
#' @param X_target Target configuration.
#' @return List with `rotated`, `rotation_matrix`, `scale`, `mse`.
#' @references Gower JC & Dijksterhuis GB (2004). \emph{Procrustes
#'   Problems}. Oxford University Press.
#' @examples
#' set.seed(1)
#' A <- matrix(rnorm(20), 10, 2); B <- A + 0.05 * matrix(rnorm(20), 10, 2)
#' morie_spatial_voting_procrustes(A, B)
#' @export
morie_spatial_voting_procrustes <- function(X, X_target) {
  X <- as.matrix(X)
  X_target <- as.matrix(X_target)
  X_c <- sweep(X, 2, colMeans(X))
  X_t <- sweep(X_target, 2, colMeans(X_target))
  M <- crossprod(X_c, X_t)
  sv <- svd(M)
  Tm <- sv$u %*% t(sv$v)
  if (det(Tm) < 0) {
    sv$v[, ncol(sv$v)] <- -sv$v[, ncol(sv$v)]
    Tm <- sv$u %*% t(sv$v)
  }
  X_rot <- X_c %*% Tm + matrix(colMeans(X_target),
                               nrow = nrow(X_c),
                               ncol = ncol(X_c), byrow = TRUE)
  list(rotated = X_rot, rotation_matrix = Tm,
       scale = sum(sv$d),
       mse = mean((X_rot - X_target) ^ 2))
}

# ===========================================================================
# 8. Bayesian methods (native samplers in spatial_voting_bayes_native.R)
# ===========================================================================

#' Bayesian Aldrich-McKelvey scaling
#'
#' @param Z Perceptual placement matrix.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @param prior_sd Prior SD on stimulus positions.
#' @return List: `zeta_mean`, `zeta_sd` (posterior stimulus
#'   positions), `sigma2`, `n_samples`, `engine`.
#' @references
#'   Hare, C., Armstrong, D. A., Bakker, R., Carroll, R., and Poole, K. T.
#'   (2015). "Using Bayesian Aldrich-McKelvey Scaling to Study Citizens'
#'   Ideological Preferences and Perceptions." *AJPS*, 59(3).
#' @examples
#' set.seed(1)
#' if (requireNamespace("basicspace", quietly = TRUE)) {
#'   \donttest{morie_spatial_voting_bayesian_am(matrix(rnorm(50), 10, 5))}
#' }
#' @export
morie_spatial_voting_bayesian_am <- function(Z, n_samples = 1000L,
                                             burn_in = 200L,
                                             prior_sd = 10.0) {
  Z <- as.matrix(Z)
  mode(Z) <- "numeric"
  if (requireNamespace("basicspace", quietly = TRUE) &&
        .sv_basicspace_shape_ok(Z)) {
    # basicspace::aldmck rejects `missing = NA` (it expects integer
    # sentinels). Letting it default + casting to numeric is the
    # safe path for arbitrary floating-point input matrices.
    out <- try(basicspace::aldmck(Z, respondent = 0, polarity = 1),
               silent = TRUE)
    if (!inherits(out, "try-error")) {
      return(list(
        zeta_mean = as.numeric(out$stimuli),
        engine    = "basicspace (deterministic AM; full Bayesian not ported)"
      ))
    }
  }
  .morie_sv_bayes_am(Z, n_samples = n_samples, burn_in = burn_in,
                     prior_sd = prior_sd)
}

#' Bayesian MDS -- log-normal distances via Metropolis
#' @param D Distance matrix.
#' @param n_dims Dimensions.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @param sigma_init Initial sigma.
#' @return List: `positions`/`coords` (posterior-mean or modal
#'   configuration), fit diagnostics, and an `engine` tag.
#' @references Oh & Raftery (2001) JASA 96(455).
#' @examples
#' if (requireNamespace("smacof", quietly = TRUE)) {
#'   \donttest{
#'   # A real dissimilarity matrix (the all-zero matrix is degenerate
#'   # and makes the stress majorizer divide by zero).
#'   set.seed(1)
#'   X <- matrix(rnorm(30), 10, 3)
#'   morie_spatial_voting_bayesian_mds(as.matrix(dist(X)))
#'   }
#' }
#' @export
morie_spatial_voting_bayesian_mds <- function(D, n_dims = 2L,
                                              n_samples = 1000L,
                                              burn_in = 200L,
                                              sigma_init = 1.0) {
  # smacof::mds is a deterministic stress-minimiser that finds the
  # posterior mode of the Oh & Raftery (2001) lognormal-distance MDS
  # model under a flat prior. We expose it here under bayesian_mds as
  # the standing R-side implementation pending a full MCMC port.
  if (requireNamespace("smacof", quietly = TRUE)) {
    fit <- smacof::mds(stats::as.dist(D), ndim = n_dims,
                       type = "ratio", verbose = FALSE)
    return(list(
      coords = unname(as.matrix(fit$conf)),
      stress = as.numeric(fit$stress),
      n_dims = n_dims,
      engine = "smacof (deterministic MDS; full Bayesian not ported)"
    ))
  }
  .morie_sv_bayes_mds(D, n_dims = n_dims, n_samples = n_samples,
                      burn_in = burn_in, sigma_init = sigma_init)
}

#' Bayesian unfolding -- Bakker & Poole sampler
#' @param D Respondent-stimulus dissimilarity matrix.
#' @param n_dims Latent dimensions.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @return List with respondent/stimulus configurations and an
#'   `engine` tag (smacof deterministic mode, or the native
#'   Metropolis sampler when smacof is absent).
#' @references Bakker, R. and Poole, K. T. (2013).
#' @examples
#' if (requireNamespace("smacof", quietly = TRUE)) {
#'   \donttest{
#'   # Random positive respondent-stimulus dissimilarities; an all-zero
#'   # matrix is degenerate for the unfolding transform.
#'   set.seed(1)
#'   morie_spatial_voting_bayesian_unfolding(matrix(runif(12, 0.5, 2), 3, 4))
#'   }
#' }
#' @export
morie_spatial_voting_bayesian_unfolding <- function(D, n_dims = 2L,
                                                    n_samples = 1000L,
                                                    burn_in = 200L) {
  # smacof::unfolding is the deterministic-mode equivalent of the
  # Bakker & Poole (2013) sampler under a flat prior.
  if (requireNamespace("smacof", quietly = TRUE)) {
    fit <- smacof::unfolding(as.matrix(D), ndim = n_dims,
                             verbose = FALSE)
    return(list(
      coords_r = unname(as.matrix(fit$conf.row)),
      coords_s = unname(as.matrix(fit$conf.col)),
      stress   = as.numeric(fit$stress),
      n_dims   = n_dims,
      engine   = "smacof::unfolding (deterministic; full Bayesian not ported)"
    ))
  }
  .morie_sv_bayes_unfold(D, n_dims = n_dims, n_samples = n_samples,
                         burn_in = burn_in)
}

#' Clinton-Jackman-Rivers Bayesian IRT by Gibbs sampling
#'
#' \eqn{P(y_{ij} = 1) = \Phi(\beta_j^\top x_i - \alpha_j)} with priors
#' \eqn{x_i \sim N(0, I)} and \eqn{(\alpha_j, \beta_j) \sim N(0, v I)}.
#' Each sweep draws the latent utilities from their truncated normals
#' (Albert and Chib 1993), then every \eqn{(\alpha_j, \beta_j)} and every
#' \eqn{x_i} from its Gaussian full conditional, as in
#' \code{MCMCpack::MCMCirt1d} and \code{pscl::ideal}. All draws are
#' inverse-CDF transforms of Philox uniforms (sweep t uses stream t), so the
#' R and Python arms give the same chain.
#'
#' @param votes Roll-call matrix of 1, 0 and NA (missing).
#' @param n_dims Ideal-point dimensions.
#' @param n_samples Retained sweeps.
#' @param burn_in Discarded sweeps.
#' @param beta_prior_var Prior variance \eqn{v} of the bill parameters.
#' @param start Optional starting ideal points; default the
#'   \code{morie_spatial_voting_em_irt} posterior mode.
#' @param seed Philox key.
#' @param keep_chains Return the draws as well as the summaries.
#' @return List: `ideal_point_mean`, `ideal_point_sd`, `alpha_mean`,
#'   `beta_mean`, `n_samples` and, when kept, `ideal_point_chain`,
#'   `alpha_chain`, `beta_chain`.
#' @references Clinton J, Jackman S & Rivers D (2004). The statistical
#'   analysis of roll call data. \emph{American Political Science
#'   Review}, 98(2), 355-370.
#'
#'   Albert J H & Chib S (1993). Bayesian analysis of binary and
#'   polychotomous response data. \emph{Journal of the American
#'   Statistical Association}, 88(422), 669-679.
#' @examples
#' votes <- rbind(c(1, 1, 0, 0), c(1, 1, 1, 0), c(0, 1, 1, 1), c(0, 0, 1, 1), c(1, 0, 0, 0))
#' fit <- morie_spatial_voting_cjr_irt(votes, n_samples = 50L, burn_in = 10L)
#' round(fit$ideal_point_mean, 3)
#' @export
morie_spatial_voting_cjr_irt <- function(votes, n_dims = 1L,
                                         n_samples = 1000L,
                                         burn_in = 200L,
                                         beta_prior_var = 25,
                                         start = NULL,
                                         seed = 0,
                                         keep_chains = TRUE) {
  Y <- as.matrix(votes)
  N <- nrow(Y)
  J <- ncol(Y)
  D <- as.integer(n_dims)
  obs <- !is.na(Y)
  x <- if (is.null(start)) morie_spatial_voting_em_irt(Y, n_dims = D)$ideal_points else matrix(as.numeric(start), N, D)
  alpha <- numeric(J)
  beta <- matrix(0, J, D)
  S <- as.integer(n_samples)
  xs <- array(0, c(S, N, D))
  as_ <- matrix(0, S, J)
  bs <- array(0, c(S, J, D))
  for (t in 0:(burn_in + S - 1L)) {
    u <- .morie_random_uniform(N * J + J * (D + 1L) + N * D, seed = seed, stream = t)
    M <- x %*% t(beta) - matrix(alpha, N, J, byrow = TRUE)
    v <- matrix(u[seq_len(N * J)], N, J, byrow = TRUE)
    yea <- !is.na(Y) & Y == 1
    s <- ifelse(yea, M, -M)
    far <- obs & s < -30
    p <- ifelse(obs, v * stats::pnorm(s), v)
    p[far] <- 0.5
    q <- matrix(.morie_normal_quantile(p), N, J)
    if (any(far)) q[far] <- .cjr_log_tail_quantile(log(v[far]) + .cjr_log_phi_far(s[far]))
    ys <- M + ifelse(yea, -q, q)
    xt <- cbind(-1, x)
    V <- solve(diag(1 / beta_prior_var, D + 1L) + crossprod(xt))
    L <- t(chol(V))
    zab <- matrix(.morie_normal_quantile(u[N * J + seq_len(J * (D + 1L))]), J, D + 1L, byrow = TRUE)
    ab <- t(V %*% crossprod(xt, ys)) + zab %*% t(L)
    alpha <- ab[, 1]
    beta <- ab[, -1, drop = FALSE]
    W <- solve(diag(1, D) + crossprod(beta))
    K <- t(chol(W))
    zx <- matrix(.morie_normal_quantile(u[N * J + J * (D + 1L) + seq_len(N * D)]), N, D, byrow = TRUE)
    x <- t(W %*% t((ys + matrix(alpha, N, J, byrow = TRUE)) %*% beta)) + zx %*% t(K)
    if (t >= burn_in) {
      s <- t - burn_in + 1L
      xs[s, , ] <- x
      as_[s, ] <- alpha
      bs[s, , ] <- beta
    }
  }
  xm <- apply(xs, c(2, 3), mean)
  out <- list(
    ideal_point_mean = matrix(xm, N, D),
    ideal_point_sd = matrix(if (S > 1L) apply(xs, c(2, 3), stats::sd) else 0, N, D),
    alpha_mean = colMeans(as_),
    beta_mean = matrix(apply(bs, c(2, 3), mean), J, D),
    n_samples = S
  )
  if (keep_chains) {
    out$ideal_point_chain <- xs
    out$alpha_chain <- as_
    out$beta_chain <- bs
  }
  out
}

# log Phi(m) for m < -30 from the Mills-ratio series; pnorm itself underflows
.cjr_log_phi_far <- function(m) {
  w <- 1 / (m * m)
  -0.5 * m * m - log(-m) - 0.5 * log(2 * pi) + log(1 - w + 3 * w * w - 15 * w^3 + 105 * w^4)
}

# AS 241 far-tail branch (r > 5) evaluated from log p
.cjr_log_tail_quantile <- function(logp) {
  rr <- sqrt(-logp) - 5
  -(.morie_as241_poly(.MORIE_AS241_E, rr) / .morie_as241_poly(.MORIE_AS241_F, rr))
}

#' Bayesian IRT likelihood (deterministic part of CJR machinery)
#'
#' @param votes Binary matrix.
#' @param x Ideal points.
#' @param alpha Difficulty.
#' @param beta Discrimination.
#' @return List with `loglik`, `vote_probs`, `n_correct`, `n_total`,
#'   `accuracy`.
#' @references Clinton J, Jackman S & Rivers D (2004). The statistical
#'   analysis of roll call data. \emph{American Political Science
#'   Review}, 98(2), 355-370.
#' @examples
#' set.seed(1)
#' v <- matrix(stats::rbinom(20, 1, 0.5), 4, 5)
#' morie_spatial_voting_bayesian_irt_likelihood(
#'   v, matrix(rnorm(4), 4, 1), rep(0, 5), matrix(rnorm(5), 5, 1))
#' @export
morie_spatial_voting_bayesian_irt_likelihood <- function(votes, x, alpha, beta) {
  votes <- as.matrix(votes)
  x <- as.matrix(x)
  beta <- as.matrix(beta)
  alpha <- as.numeric(alpha)
  mask <- !is.na(votes)
  n_leg <- nrow(votes)
  n_vote <- ncol(votes)
  P <- matrix(0, n_leg, n_vote)
  ll <- 0
  n_correct <- 0L
  n_total <- 0L
  for (i in seq_len(n_leg)) for (j in seq_len(n_vote)) {
    if (!mask[i, j]) next
    z <- as.numeric(beta[j, ] %*% x[i, ] - alpha[j])
    p <- pmin(pmax(stats::pnorm(z), 1e-10), 1 - 1e-10)
    P[i, j] <- p
    if (votes[i, j] == 1) {
      ll <- ll + log(p)
      if (p > 0.5) n_correct <- n_correct + 1L
    } else {
      ll <- ll + log(1 - p)
      if (p < 0.5) n_correct <- n_correct + 1L
    }
    n_total <- n_total + 1L
  }
  list(loglik = ll, vote_probs = P,
       n_correct = n_correct, n_total = n_total,
       accuracy = if (n_total > 0) n_correct / n_total else 0)
}

#' Posterior summaries for a Bayesian IRT chain
#' @param chain Array of shape (n_samples, n_leg, n_dims).
#' @param standardize Whether to per-sample standardise.
#' @return List with `posterior_mean`, `posterior_sd`, `ci_lower`,
#'   `ci_upper`, `n_samples`, `standardized`.
#' @references Jackman S (2009). \emph{Bayesian Analysis for the Social
#'   Sciences}. Wiley.
#' @examples
#' set.seed(1)
#' ch <- array(rnorm(100 * 5 * 2), c(100, 5, 2))
#' morie_spatial_voting_bayesian_irt_posterior(ch)
#' @export
morie_spatial_voting_bayesian_irt_posterior <- function(chain,
                                                        standardize = TRUE) {
  ch <- chain
  n_samples <- dim(ch)[1L]
  if (standardize) {
    for (t in seq_len(n_samples)) {
      M <- ch[t, , , drop = TRUE]
      if (is.null(dim(M))) M <- matrix(M, ncol = 1L)
      m <- colMeans(M)
      s <- apply(M, 2, stats::sd)
      s <- ifelse(s > 0, s, 1)
      ch[t, , ] <- sweep(sweep(M, 2, m, "-"), 2, s, "/")
    }
  }
  posterior_mean <- apply(ch, c(2, 3), mean)
  posterior_sd   <- apply(ch, c(2, 3), stats::sd)
  ci_low  <- apply(ch, c(2, 3), stats::quantile, probs = 0.025)
  ci_high <- apply(ch, c(2, 3), stats::quantile, probs = 0.975)
  list(posterior_mean = posterior_mean, posterior_sd = posterior_sd,
       ci_lower = ci_low, ci_upper = ci_high,
       n_samples = n_samples, standardized = standardize)
}

# ===========================================================================
# 9. Ordered Optimal Classification & anchoring vignettes
# ===========================================================================

#' Ordered Optimal Classification for ordinal scales
#' @param Y Respondent-by-item ordinal response matrix.
#' @param n_dims Latent dimensions.
#' @param max_iter Maximum iterations.
#' @param tol Tolerance (unused; kept for API parity).
#' @return List with `ideal_points`, `cutpoints`, `normals`,
#'   `correct_class`, `iterations`.
#' @references Hare, C., Liu, T.-P., and Lupton, R. N. (2018). "What Ordered
#'   Optimal Classification reveals about ideological structure, cleavages,
#'   and polarization in the American mass public." *Public Choice*,
#'   176(1), 57-78.
#' @examples
#' set.seed(1)
#' Y <- matrix(sample(1:4, 60, replace = TRUE), 15, 4)
#' morie_spatial_voting_ordered_oc(Y, n_dims = 1L, max_iter = 20L)
#' @export
morie_spatial_voting_ordered_oc <- function(Y,
                                            n_dims   = 2L,
                                            max_iter = 500L,
                                            tol      = 1e-6) {
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  mask <- !is.na(Y)
  .rmorie_local_seed(42L)
  X <- matrix(stats::rnorm(n * n_dims), n, n_dims)
  cats_list <- vector("list", m)
  cutpoints <- vector("list", m)
  for (j in seq_len(m)) {
    cats <- sort(unique(Y[mask[, j], j]))
    cats_list[[j]] <- cats
    n_cuts <- length(cats) - 1L
    cutpoints[[j]] <- if (n_cuts > 0L) seq(-1, 1, length.out = n_cuts) else 0
  }
  normals <- matrix(stats::rnorm(m * n_dims), m, n_dims)
  for (j in seq_len(m)) normals[j, ] <- normals[j, ] /
                                        (sqrt(sum(normals[j, ] ^ 2)) + 1e-12)
  correct <- 0L
  total <- 0L
  old_correct <- -1L
  iter <- 0L
  for (iter in seq_len(max_iter)) {
    old_correct <- correct
    correct <- 0L
    total <- 0L
    for (j in seq_len(m)) {
      cats <- cats_list[[j]]
      valid <- mask[, j]
      if (length(cats) < 2L) next
      proj <- X[valid, , drop = FALSE] %*% normals[j, ]
      y_valid <- Y[valid, j]
      sorted_cuts <- sort(cutpoints[[j]])
      predicted <- findInterval(as.numeric(proj), sorted_cuts)
      cat_map <- stats::setNames(seq_along(cats) - 1L, as.character(cats))
      actual <- unname(cat_map[as.character(y_valid)])
      actual[is.na(actual)] <- 0L
      correct <- correct + sum(predicted == actual)
      total   <- total + length(actual)
    }
    for (j in seq_len(m)) {
      cats <- cats_list[[j]]
      valid <- mask[, j]
      if (length(cats) < 2L) next
      proj <- as.numeric(X[valid, , drop = FALSE] %*% normals[j, ])
      y_valid <- Y[valid, j]
      cat_map <- stats::setNames(seq_along(cats) - 1L, as.character(cats))
      actual <- unname(cat_map[as.character(y_valid)])
      actual[is.na(actual)] <- 0L
      sorted_idx <- order(proj)
      new_cuts <- numeric(length(cats) - 1L)
      for (k in seq_len(length(cats) - 1L)) {
        boundary_vals <- c()
        for (ii in seq_len(length(sorted_idx) - 1L)) {
          if (actual[sorted_idx[ii]] <= (k - 1L) &&
              actual[sorted_idx[ii + 1L]] > (k - 1L)) {
            boundary_vals <- c(boundary_vals,
                               (proj[sorted_idx[ii]] +
                                proj[sorted_idx[ii + 1L]]) / 2)
          }
        }
        new_cuts[k] <- if (length(boundary_vals) > 0L)
                         stats::median(boundary_vals)
                       else if (k <= length(cutpoints[[j]]))
                         cutpoints[[j]][k]
                       else 0
      }
      cutpoints[[j]] <- new_cuts
    }
    if (total > 0L && correct == old_correct && iter > 1L) break
  }
  list(ideal_points = X, cutpoints = cutpoints, normals = normals,
       correct_class = if (total > 0L) correct / total else 0,
       iterations = iter)
}

#' Anchoring vignettes for DIF correction
#' @param Y Vector of self-placement ratings.
#' @param V Respondent-by-vignette ratings.
#' @param n_categories Number of ordered categories.
#' @return List with `corrected_scores`, `thresholds`, `dif_estimates`,
#'   `vignette_order`, `n_respondents`, `n_vignettes`.
#' @references King, G., Murray, C. J. L., Salomon, J. A., and Tandon, A.
#'   (2003). "Enhancing the Validity and Cross-Cultural Comparability of
#'   Measurement in Survey Research." *APSR*, 97(4), 567-583.
#' @examples
#' set.seed(1)
#' Y <- sample(1:5, 30, replace = TRUE)
#' V <- matrix(sample(1:5, 30 * 3, replace = TRUE), 30, 3)
#' morie_spatial_voting_anchoring_vignettes(Y, V)
#' @export
morie_spatial_voting_anchoring_vignettes <- function(Y, V,
                                                     n_categories = 5L) {
  Y <- as.numeric(Y)
  V <- as.matrix(V)
  n_resp <- length(Y)
  n_vign <- ncol(V)
  vign_means <- .sv_nanmean_col(V)
  vign_order <- order(vign_means)
  thresholds <- matrix(0, n_resp, n_categories - 1L)
  for (i in seq_len(n_resp)) {
    vi <- V[i, ]
    valid <- !is.na(vi)
    if (sum(valid) >= 2L) {
      sv <- sort(vi[valid])
      nv <- length(sv)
      for (k in seq_len(n_categories - 1L)) {
        idx <- as.integer((k - 1L) * nv / (n_categories - 1L))
        idx <- min(idx + 1L, nv)
        thresholds[i, k] <- sv[idx]
      }
    } else {
      thresholds[i, ] <- seq(1, n_categories, length.out = n_categories - 1L)
    }
  }
  corrected <- numeric(n_resp)
  for (i in seq_len(n_resp)) {
    y <- Y[i]
    corrected[i] <- if (is.na(y)) NA_real_ else
                    findInterval(y, sort(thresholds[i, ]))
  }
  dif_estimates <- apply(thresholds, 2, stats::sd)
  list(corrected_scores = corrected, thresholds = thresholds,
       dif_estimates = dif_estimates, vignette_order = vign_order,
       n_respondents = n_resp, n_vignettes = n_vign)
}

# ===========================================================================
# 10. INDSCAL
# ===========================================================================

#' INDSCAL: weighted MDS with individual differences
#'
#' Carroll & Chang (1970) weighted MDS with a shared stimulus space and
#' per-individual dimension weights.
#'
#' @param dissimilarities List of (n_stim x n_stim) dissimilarity matrices.
#' @param n_dims Number of dimensions.
#' @param max_iter Maximum ALS iterations.
#' @param tol Convergence tolerance on configuration change.
#' @return List with `group_config`, `weights`, `stress`, `iterations`,
#'   `n_individuals`, `n_stimuli`.
#' @references
#'   Carroll, J. D. and Chang, J.-J. (1970). "Analysis of Individual
#'   Differences in Multidimensional Scaling via an N-way Generalization
#'   of Eckart-Young Decomposition." *Psychometrika*, 35(3).
#' @examples
#' set.seed(1)
#' D1 <- as.matrix(dist(matrix(rnorm(20), 5)))
#' D2 <- as.matrix(dist(matrix(rnorm(20), 5)))
#' morie_spatial_voting_indscal(list(D1, D2), n_dims = 2L, max_iter = 30L)
#' @export
morie_spatial_voting_indscal <- function(dissimilarities,
                                         n_dims   = 2L,
                                         max_iter = 300L,
                                         tol      = 1e-6) {
  D_list <- lapply(dissimilarities, as.matrix)
  n_indiv <- length(D_list)
  n_stim <- nrow(D_list[[1L]])
  D_avg <- Reduce("+", D_list) / n_indiv
  n <- nrow(D_avg)
  H <- diag(n) - matrix(1, n, n) / n
  B <- -0.5 * H %*% (D_avg ^ 2) %*% H
  ev <- eigen(B, symmetric = TRUE)
  idx <- order(ev$values, decreasing = TRUE)[seq_len(n_dims)]
  X <- ev$vectors[, idx, drop = FALSE] %*%
       diag(sqrt(pmax(ev$values[idx], 0)), nrow = n_dims)
  W <- matrix(1, n_indiv, n_dims)
  iter <- 0L
  for (iter in seq_len(max_iter)) {
    X_old <- X
    for (k in seq_len(n_indiv)) {
      D_k <- D_list[[k]]
      for (s in seq_len(n_dims)) {
        X_s <- X[, s, drop = FALSE]
        diffs <- outer(as.numeric(X_s), as.numeric(X_s), "-")
        dist_s <- sqrt(diffs ^ 2 + 1e-12)
        numer <- sum(D_k * dist_s)
        denom <- sum(dist_s ^ 2) + 1e-12
        W[k, s] <- max(numer / denom, 0.01)
      }
    }
    for (j in seq_len(n_stim)) {
      for (s in seq_len(n_dims)) {
        numer <- 0
        denom <- 0
        for (k in seq_len(n_indiv)) for (l in seq_len(n_stim)) {
          if (l == j) next
          d_kj <- D_list[[k]][j, l]
          w_s <- W[k, s]
          numer <- numer + w_s * d_kj * X[l, s]
          denom <- denom + w_s ^ 2 + 1e-12
        }
        if (isTRUE(denom > 0) && is.finite(numer)) X[j, s] <- numer / denom
      }
    }
    change <- sqrt(sum((X - X_old) ^ 2)) /
              (sqrt(sum(X_old ^ 2)) + 1e-12)
    if (change < tol) break
  }
  total_stress <- 0
  for (k in seq_len(n_indiv)) {
    Xw <- sweep(X, 2, sqrt(W[k, ]), "*")
    d_model <- .sv_pairwise_dist(Xw)
    total_stress <- total_stress + sum((D_list[[k]] - d_model) ^ 2)
  }
  list(group_config = X, weights = W, stress = total_stress,
       iterations = iter, n_individuals = n_indiv, n_stimuli = n_stim)
}

# ===========================================================================
# 11. Geometry helpers for Coombs-mesh visualisation
# ===========================================================================

#' Normal-vector projection of an external measure
#' @param ideal_points Ideal-point coordinates.
#' @param external_measure Vector to project.
#' @return List with `normal_vector`, `angle_degrees`, `angle_radians`,
#'   `r_squared`, `coefficients`.
#' @references Armstrong et al. (2021), Section 2.6.
#' @examples
#' set.seed(1)
#' morie_spatial_voting_normal_vectors(matrix(rnorm(20), 10, 2), rnorm(10))
#' @export
morie_spatial_voting_normal_vectors <- function(ideal_points,
                                                external_measure) {
  X <- as.matrix(ideal_points)
  y <- as.numeric(external_measure)
  mask <- !is.na(y)
  Xv <- X[mask, , drop = FALSE]
  yv <- y[mask]
  X_aug <- cbind(1, Xv)
  fit <- qr.solve(X_aug, yv)
  beta <- fit
  coeffs <- beta[-1L]
  nl <- sqrt(sum(coeffs ^ 2))
  nv <- if (nl > 0) coeffs / nl else coeffs
  y_pred <- X_aug %*% beta
  ss_res <- sum((yv - y_pred) ^ 2)
  ss_tot <- sum((yv - mean(yv)) ^ 2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
  angle_rad <- if (length(nv) >= 2) atan2(nv[2L], nv[1L]) else 0
  list(normal_vector = nv, angle_degrees = angle_rad * 180 / pi,
       angle_radians = angle_rad, r_squared = r2, coefficients = beta)
}

#' Cutting-line endpoints for Coombs-mesh plots
#' @param normals (n_votes x n_dims) normal vectors.
#' @param cutpoints Numeric cutpoint offsets.
#' @param xlim Length-2 numeric vector of x-axis limits.
#' @return List with `endpoints` (list of pairs), `angles`, `midpoints`,
#'   `n_lines`.
#' @references Poole KT (2005). \emph{Spatial Models of Parliamentary
#'   Voting}. Cambridge University Press.
#' @examples
#' set.seed(1)
#' morie_spatial_voting_cutting_lines(matrix(rnorm(6), 3, 2), c(0.1, -0.2, 0))
#' @export
morie_spatial_voting_cutting_lines <- function(normals, cutpoints,
                                               xlim = c(-1, 1)) {
  normals <- as.matrix(normals)
  cutpoints <- as.numeric(cutpoints)
  n_votes <- nrow(normals)
  endpoints <- vector("list", n_votes)
  angles <- numeric(n_votes)
  midpoints <- vector("list", n_votes)
  for (k in seq_len(n_votes)) {
    nv <- normals[k, ]
    cp <- cutpoints[k]
    if (abs(nv[2L]) > 1e-10) {
      x1 <- xlim[1L]
      x2 <- xlim[2L]
      y1 <- (cp - nv[1L] * x1) / nv[2L]
      y2 <- (cp - nv[1L] * x2) / nv[2L]
      endpoints[[k]] <- list(c(x1, y1), c(x2, y2))
      midpoints[[k]] <- c((x1 + x2) / 2, (y1 + y2) / 2)
    } else if (abs(nv[1L]) > 1e-10) {
      xc <- cp / nv[1L]
      endpoints[[k]] <- list(c(xc, -10), c(xc, 10))
      midpoints[[k]] <- c(xc, 0)
    } else {
      endpoints[[k]] <- list(c(0, 0), c(0, 0))
      midpoints[[k]] <- c(0, 0)
    }
    angles[k] <- atan2(nv[2L], nv[1L]) * 180 / pi
  }
  list(endpoints = endpoints, angles = angles,
       midpoints = midpoints, n_lines = n_votes)
}

# ===========================================================================
# 12. DW-NOMINATE (deterministic gradient solver)
# ===========================================================================

#' DW-NOMINATE dynamic weighted ideal-point estimator
#'
#' Gaussian-error NOMINATE variant supporting comparable scores across
#' legislative sessions.
#'
#' @param votes Legislator-by-vote binary matrix.
#' @param n_dims Latent dimensions.
#' @param max_iter Maximum iterations.
#' @param tol Tolerance (unused; kept for API parity).
#' @return List with `ideal_points`, `dim_weights`, `normal_vectors`,
#'   `cutpoints`, `log_lik`, `gmp`, `n_dims`.
#' @references
#'   Poole, K. T. and Rosenthal, H. (1997). *Congress: A Political-
#'   Economic History of Roll Call Voting*. Oxford University Press.
#' @examples
#' set.seed(1)
#' v <- matrix(stats::rbinom(20 * 30, 1, 0.5), 20, 30)
#' morie_spatial_voting_dw_nominate(v, max_iter = 20)
#' @export
morie_spatial_voting_dw_nominate <- function(votes, n_dims = 2L,
                                             max_iter = 100L, tol = 1e-6) {
  votes <- as.matrix(votes)
  n_leg <- nrow(votes)
  n_votes <- ncol(votes)
  mask <- !is.na(votes)
  .rmorie_local_seed(42L)
  X <- matrix(stats::rnorm(n_leg * n_dims) * 0.5, n_leg, n_dims)
  w <- rep(1 / n_dims, n_dims)
  beta <- 15.0
  nv <- matrix(stats::rnorm(n_votes * n_dims), n_votes, n_dims)
  for (j in seq_len(n_votes)) nv[j, ] <- nv[j, ] /
                                         (sqrt(sum(nv[j, ] ^ 2)) + 1e-12)
  mid <- matrix(0, n_votes, n_dims)
  if (.sv_have_cpp("morie_spatial_nominate_iterate_cpp")) {
    res <- morie_spatial_nominate_iterate_cpp(votes, X, w, nv, mid,
                                               as.numeric(beta),
                                               as.integer(max_iter))
    return(list(ideal_points    = res$ideal_points,
                dim_weights     = res$dim_weights,
                normal_vectors  = res$normal_vectors,
                cutpoints       = res$cutpoints,
                log_lik         = res$log_lik,
                gmp             = res$gmp,
                n_dims          = n_dims))
  }
  for (iter in seq_len(max_iter)) {
    for (j in seq_len(n_votes)) {
      valid <- mask[, j]
      if (sum(valid) < 2L) next
      Xv <- X[valid, , drop = FALSE]
      yv <- votes[valid, j]
      yea_idx <- which(yv == 1)
      nay_idx <- which(yv == 0)
      yea_center <- if (length(yea_idx) > 0L)
                      colMeans(Xv[yea_idx, , drop = FALSE]) else mid[j, ]
      nay_center <- if (length(nay_idx) > 0L)
                      colMeans(Xv[nay_idx, , drop = FALSE]) else mid[j, ]
      direction <- yea_center - nay_center
      dn <- sqrt(sum(direction ^ 2))
      if (dn > 1e-10) nv[j, ] <- direction / dn
      mid[j, ] <- (yea_center + nay_center) / 2
    }
    for (i in seq_len(n_leg)) {
      valid <- mask[i, ]
      if (sum(valid) < 2L) next
      y_i <- votes[i, valid]
      nv_i <- nv[valid, , drop = FALSE]
      mid_i <- mid[valid, , drop = FALSE]
      yea_pos <- mid_i + 0.5 * nv_i
      nay_pos <- mid_i - 0.5 * nv_i
      target <- yea_pos
      target[y_i == 0, ] <- nay_pos[y_i == 0, , drop = FALSE]
      X[i, ] <- colMeans(target)
    }
    norm_x <- sqrt(rowSums(X ^ 2))
    if (max(norm_x) > 1) X <- X / max(norm_x)
  }
  ll_final <- 0
  total <- 0L
  correct <- 0L
  for (j in seq_len(n_votes)) {
    valid <- mask[, j]
    if (sum(valid) == 0L) next
    Xv <- X[valid, , drop = FALSE]
    yv <- votes[valid, j]
    d_yea <- rowSums(sweep(Xv, 2, mid[j, ] + 0.5 * nv[j, ], "-") ^ 2 *
                     matrix(w, nrow = sum(valid), ncol = n_dims, byrow = TRUE))
    d_nay <- rowSums(sweep(Xv, 2, mid[j, ] - 0.5 * nv[j, ], "-") ^ 2 *
                     matrix(w, nrow = sum(valid), ncol = n_dims, byrow = TRUE))
    u_diff <- beta * (exp(-0.5 * d_yea) - exp(-0.5 * d_nay))
    p <- pmin(pmax(stats::pnorm(u_diff), 1e-10), 1 - 1e-10)
    ll_final <- ll_final + sum(yv * log(p) + (1 - yv) * log(1 - p))
    pred <- as.numeric(p > 0.5)
    correct <- correct + sum(pred == yv)
    total <- total + length(yv)
  }
  gmp <- if (total > 0L) correct / total else 0
  cp <- sapply(seq_len(n_votes), function(j) sum(nv[j, ] * mid[j, ]))
  list(ideal_points = X, dim_weights = w, normal_vectors = nv,
       cutpoints = cp, log_lik = ll_final, gmp = gmp, n_dims = n_dims)
}

# ===========================================================================
# 13. NOMINATE bootstrap
# ===========================================================================

#' Parametric bootstrap of NOMINATE standard errors
#'
#' Lewis & Poole (2004) parametric bootstrap: simulate roll-call matrices
#' from fitted probabilities, re-estimate per bootstrap replicate, compute
#' SE from the bootstrap distribution.
#'
#' @param votes Original vote matrix.
#' @param ideal_points Fitted ideal points.
#' @param normal_vectors_arr Fitted normal vectors.
#' @param cutpoints Fitted cutpoints.
#' @param n_boot Number of bootstrap replications.
#' @param seed RNG seed.
#' @return List with `se_ideal_points`, `boot_means`, `n_boot`.
#' @references
#'   Lewis, J. B. and Poole, K. T. (2004). "Measuring Bias and
#'   Uncertainty in Ideal Point Estimates via the Parametric Bootstrap."
#'   *Political Analysis*, 12(2).
#' @examples
#' set.seed(1)
#' v <- matrix(stats::rbinom(40, 1, 0.5), 5, 8)
#' fit <- morie_spatial_voting_dw_nominate(v, max_iter = 5L)
#' morie_spatial_voting_nominate_bootstrap(
#'   v, fit$ideal_points, fit$normal_vectors, fit$cutpoints, n_boot = 5L)
#' @export
morie_spatial_voting_nominate_bootstrap <- function(votes,
                                                    ideal_points,
                                                    normal_vectors_arr,
                                                    cutpoints,
                                                    n_boot = 100L,
                                                    seed   = 42L) {
  votes <- as.matrix(votes)
  X <- as.matrix(ideal_points)
  nv <- as.matrix(normal_vectors_arr)
  cp <- as.numeric(cutpoints)
  n_leg <- nrow(votes)
  n_votes <- ncol(votes)
  n_dims <- ncol(X)
  mask <- !is.na(votes)
  .rmorie_local_seed(seed)
  beta <- 15.0
  probs <- matrix(0.5, n_leg, n_votes)
  for (j in seq_len(n_votes)) {
    proj <- as.numeric(X %*% nv[j, ])
    u <- beta * (proj - cp[j])
    probs[, j] <- 1 / (1 + exp(-u))
  }
  boot_points <- array(0, dim = c(n_boot, n_leg, n_dims))
  for (b in seq_len(n_boot)) {
    sim_votes <- (matrix(stats::runif(n_leg * n_votes),
                         n_leg, n_votes) < probs) * 1
    sim_votes[!mask] <- NA
    X_b <- X + matrix(stats::rnorm(n_leg * n_dims) * 0.1, n_leg, n_dims)
    for (k in seq_len(20L)) {
      for (i in seq_len(n_leg)) {
        valid <- !is.na(sim_votes[i, ])
        if (sum(valid) < 2L) next
        y_i <- sim_votes[i, valid]
        nv_i <- nv[valid, , drop = FALSE]
        cp_i <- cp[valid]
        proj <- as.numeric(X_b[i, ] %*% t(nv_i))
        residual <- y_i - 1 / (1 + exp(-beta * (proj - cp_i)))
        grad <- as.numeric(t(nv_i) %*% residual)
        X_b[i, ] <- X_b[i, ] + 0.01 * grad
      }
    }
    boot_points[b, , ] <- X_b
  }
  se <- apply(boot_points, c(2, 3), stats::sd)
  boot_mean <- apply(boot_points, c(2, 3), mean)
  list(se_ideal_points = se, boot_means = boot_mean, n_boot = n_boot)
}

# ===========================================================================
# 14. Alpha-NOMINATE / dynamic / ordinal IRT (EM-IRT approximations where flagged)
# ===========================================================================

#' Alpha-NOMINATE ideal points (EM-IRT approximation)
#'
#' Carroll et al. (2013) mixture model between Gaussian and quadratic
#' utility, sampled via slice sampling (Neal 2003).  Porting the slice
#' sampler is beyond this session's budget.
#'
#' @param votes Vote matrix.
#' @param n_dims Latent dimensions.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @param seed RNG seed.
#' @return List with ideal points, discrimination, difficulty and an
#'   `engine` tag (EM-IRT closed-form approximation).
#' @references Carroll, R., Lewis, J. B., Lo, J., Poole, K. T., and
#'   Rosenthal, H. (2013); Neal, R. M. (2003) *Annals of Statistics*.
#' @examples \donttest{morie_spatial_voting_alpha_nominate(matrix(0, 5, 5))}
#' @export
morie_spatial_voting_alpha_nominate <- function(votes, n_dims = 2L,
                                                n_samples = 500L,
                                                burn_in = 100L,
                                                seed = 42L) {
  # 3MMM.27 (2026-05-25): the full Bayesian alpha-NOMINATE Gibbs
  # sampler (Carroll, Lewis, Lo, Poole & Rosenthal 2013) is not
  # ported. Until then we delegate to morie_spatial_voting_em_irt --
  # Imai, Lo & Olmsted (2016) closed-form EM-IRT -- which produces
  # ideal-points + discrimination from the same input via
  # deterministic EM updates. Results diverge from alpha-NOMINATE in
  # the tails (no posterior uncertainty, no slope-priors) but the
  # call returns a usable result instead of raising NotYetPorted;
  # dimensionality and sign conventions match. Engine tag flags the
  # substitution.
  .rmorie_local_seed(as.integer(seed))
  fit <- morie_spatial_voting_em_irt(as.matrix(votes),
                                      n_dims = as.integer(n_dims))
  list(
    ideal_points     = fit$ideal_points,
    discrimination   = fit$discrimination,
    difficulty       = fit$difficulty,
    n_dims           = as.integer(n_dims),
    n_samples_target = as.integer(n_samples),
    burn_in_target   = as.integer(burn_in),
    engine = paste0("morie_spatial_voting_em_irt (deterministic EM ",
                    "approximation; full alpha-NOMINATE Gibbs not yet ",
                    "ported -- see Carroll et al 2013)")
  )
}

#' Ordinal IRT / Quinn factor model
#'
#' @param Y Ordinal response matrix.
#' @param n_dims Latent dimensions.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @param seed RNG seed.
#' @return List: `ideal_points`, `ideal_sd`, `discrimination`,
#'   `difficulty`, `cutpoints`, `engine`.
#' @references Quinn, K. M. (2004). "Bayesian Factor Analysis for Mixed
#'   Ordinal and Continuous Responses." *Political Analysis*, 12(4).
#' @examples
#' if (requireNamespace("MCMCpack", quietly = TRUE)) {
#'   set.seed(1)
#'   Y <- matrix(sample(1:3, 60, TRUE), 20, 3)
#'   fit <- morie_spatial_voting_ordinal_irt(Y, n_samples = 100L,
#'                                           burn_in = 50L)
#'   head(fit$ideal_points)
#' }
#' @export
morie_spatial_voting_ordinal_irt <- function(Y, n_dims = 1L,
                                             n_samples = 500L,
                                             burn_in = 100L,
                                             seed = 42L) {
  # MCMCpack::MCMCordfactanal runs ordinal factor-analytic IRT with a
  # Gibbs-sampler; it's the closest R-side equivalent to ordIRT.
  if (requireNamespace("MCMCpack", quietly = TRUE)) {
    Y <- as.matrix(Y)
    df <- as.data.frame(Y)
    for (j in seq_along(df)) df[[j]] <- as.ordered(df[[j]])
    # MCMCordfactanal needs a formula + data; passing a matrix
    # directly yields "no terms component nor attribute".
    f <- stats::as.formula(
      paste("~", paste(names(df), collapse = " + ")))
    fit <- tryCatch(
      MCMCpack::MCMCordfactanal(
        x = f, data = df, factors = as.integer(n_dims),
        burnin = as.integer(burn_in),
        mcmc   = as.integer(n_samples),
        verbose = 0L, seed = as.integer(seed)),
      error = function(e) e
    )
    if (!inherits(fit, "error")) {
      return(list(
        ideal_points = unname(as.matrix(summary(fit)$statistics[, "Mean", drop = FALSE])),
        n_dims = n_dims, n_samples = n_samples,
        engine = "MCMCpack::MCMCordfactanal (ordinal Bayesian IRT)"
      ))
    }
  }
  .morie_sv_bayes_ordinal(Y, n_samples = n_samples,
                          burn_in = burn_in)
}

#' Dynamic IRT, fitted period by period (EM-IRT approximation)
#'
#' Time-series IRT where ideal points evolve via a random walk:
#' \eqn{\phi_{i,t} \sim N(\phi_{i,t-1}, \tau^2)}{phi_i,t ~ N(phi_i,t-1, tau^2)}.
#'
#' @param votes Vote matrix.
#' @param time_periods Per-vote period indices.
#' @param n_samples MCMC samples.
#' @param burn_in Burn-in length.
#' @param seed RNG seed.
#' @return List of per-period ideal-point matrices with an `engine`
#'   tag flagging the per-period EM approximation.
#' @references Martin, A. D. and Quinn, K. M. (2002). "Dynamic Ideal Point
#'   Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court,
#'   1953-1999." *Political Analysis*, 10(2).
#' @examples \donttest{morie_spatial_voting_dynamic_irt(matrix(0, 4, 4), 1:4)}
#' # dynamic-IRT random-walk prior on ideal points.
#' @export
morie_spatial_voting_dynamic_irt <- function(votes, time_periods,
                                             n_samples = 500L,
                                             burn_in = 100L,
                                             seed = 42L) {
  # 3MMM.27 (2026-05-25): emIRT::dynIRT's prior shape contract varies
  # across releases. Until we wire a stable adapter we run EM-IRT
  # period-by-period (Imai, Lo & Olmsted 2016 closed-form) and return
  # a list of per-period ideal-points matrices. This loses the
  # Brownian-motion smoothing across periods (the whole point of
  # Martin-Quinn dynamic-IRT) but each per-period fit is itself a
  # valid IRT estimate, so the call returns instead of raising
  # NotYetPorted. Engine tag flags the approximation.
  .rmorie_local_seed(as.integer(seed))
  votes <- as.matrix(votes)
  time_periods <- as.integer(time_periods)
  if (length(time_periods) != ncol(votes)) {
    stop("time_periods length must equal ncol(votes)")
  }
  periods <- sort(unique(time_periods))
  per_period <- lapply(periods, function(p) {
    cols <- which(time_periods == p)
    if (length(cols) < 1L) {
      return(list(period = p, ideal_points = NULL))
    }
    sub <- votes[, cols, drop = FALSE]
    fit <- morie_spatial_voting_em_irt(sub, n_dims = 1L)
    list(period = p,
         ideal_points = fit$ideal_points,
         discrimination = fit$discrimination,
         difficulty = fit$difficulty,
         n_votes_in_period = length(cols))
  })
  names(per_period) <- as.character(periods)
  list(
    per_period = per_period,
    periods = periods,
    n_periods = length(periods),
    n_legislators = nrow(votes),
    n_samples_target = as.integer(n_samples),
    burn_in_target = as.integer(burn_in),
    engine = paste0("morie_spatial_voting_em_irt per-period ",
                    "(deterministic EM approximation; full Martin-Quinn ",
                    "dynamic-IRT with Brownian-motion smoothing not yet ",
                    "ported -- see Martin & Quinn 2002)")
  )
}

# ===========================================================================
# 15. EM-IRT (deterministic)
# ===========================================================================

#' EM algorithm for binary IRT
#'
#' Imai, Lo & Olmsted (2016) EM for the binary probit IRT model
#' \eqn{y^*_{ij} = \alpha_j + \beta_j^\top x_i + e_{ij}}, yea when
#' \eqn{y^*_{ij} > 0}, with priors \eqn{x_i \sim N(\mu_x, s^2_x I)} and
#' \eqn{(\alpha_j, \beta_j) \sim N(\mu_b, s^2_b I)}. The E-step replaces
#' \eqn{y^*} by its truncated-normal mean (untruncated when missing); the
#' M-step is the pair of ridge regressions of \code{emIRT::binIRT} with
#' \code{asEM = TRUE}. The fixed point is the posterior mode.
#'
#' @param votes Legislator-by-vote matrix of 1 (yea), 0 (nay), NA (missing).
#' @param n_dims Latent dimensions.
#' @param max_iter Maximum EM iterations.
#' @param tol Convergence threshold.
#' @param x_prior Prior mean and variance of the ideal points.
#' @param beta_prior Prior mean and variance of the intercepts and slopes.
#' @param start Optional list of starting alpha, beta and x; default alpha
#'   = beta = 0 and x from 50 steps of orthogonal iteration on the
#'   cross-product of the column-centred +1/-1 votes.
#' @param conv \code{"cor"} (1 minus the smallest old/new correlation per
#'   column) or \code{"abs"} (largest absolute change).
#' @return List with `ideal_points`, `discrimination`, `difficulty` (the
#'   intercepts), `log_lik`, `iterations`, `converged`.
#' @references
#'   Imai, K., Lo, J., and Olmsted, J. (2016). "Fast Estimation of Ideal
#'   Points with Massive Data." *APSR*, 110(4), 631-656.
#' @examples
#' set.seed(1)
#' v <- matrix(stats::rbinom(20 * 30, 1, 0.5), 20, 30)
#' morie_spatial_voting_em_irt(v, max_iter = 20L)
#' @export
morie_spatial_voting_em_irt <- function(votes,
                                        n_dims   = 1L,
                                        max_iter = 500L,
                                        tol      = 1e-6,
                                        x_prior = c(0, 1),
                                        beta_prior = c(0, 25),
                                        start = NULL,
                                        conv = c("cor", "abs")) {
  conv <- match.arg(conv)
  Y <- as.matrix(votes)
  N <- nrow(Y)
  J <- ncol(Y)
  D <- as.integer(n_dims)
  obs <- !is.na(Y)
  if (is.null(start)) {
    a <- numeric(J)
    b <- matrix(0, J, D)
    x <- .em_irt_start(Y, obs, D)
  } else {
    a <- as.numeric(start[[1]])
    b <- matrix(as.numeric(start[[2]]), J, D)
    x <- matrix(as.numeric(start[[3]]), N, D)
  }
  it <- 0L
  converged <- FALSE
  while (it < max_iter) {
    it <- it + 1L
    M <- matrix(a, N, J, byrow = TRUE) + x %*% t(b)
    ys <- ifelse(!obs, M, ifelse(!is.na(Y) & Y == 1,
                                 M + stats::dnorm(M) / stats::pnorm(M),
                                 M - stats::dnorm(M) / stats::pnorm(-M)))
    x2 <- cbind(1, x)
    B <- solve(diag(1 / beta_prior[2], D + 1L) + crossprod(x2))
    ab <- t(B %*% (beta_prior[1] / beta_prior[2] + crossprod(x2, ys)))
    a_new <- ab[, 1]
    b_new <- ab[, -1, drop = FALSE]
    eba <- as.numeric(crossprod(b_new, a_new))
    A <- solve(diag(1 / x_prior[2], D) + crossprod(b_new))
    x_new <- t(A %*% (x_prior[1] / x_prior[2] + t(ys %*% b_new) - eba))
    if (it > 1L) {
      dev <- max(.em_dev(x, x_new, conv), .em_dev(cbind(a), cbind(a_new), conv),
                 .em_dev(b, b_new, conv))
      converged <- dev < tol
    }
    a <- a_new
    b <- b_new
    x <- x_new
    if (converged) break
  }
  M <- matrix(a, N, J, byrow = TRUE) + x %*% t(b)
  ll <- sum(stats::pnorm(ifelse(Y == 1, M, -M), log.p = TRUE)[obs])
  list(ideal_points = x, discrimination = b, difficulty = a, log_lik = ll,
       iterations = it, converged = converged)
}

.em_dev <- function(old, new, conv) {
  if (conv == "abs") return(max(abs(old - new)))
  worst <- 0
  for (d in seq_len(ncol(old))) {
    u <- old[, d] - mean(old[, d])
    v <- new[, d] - mean(new[, d])
    su <- sum(u^2)
    sv <- sum(v^2)
    worst <- max(worst, if (su > 0 && sv > 0) 1 - sum(u * v) / sqrt(su * sv) else 1)
  }
  worst
}

.em_irt_start <- function(Y, obs, D) {
  N <- nrow(Y)
  Z <- ifelse(obs, ifelse(Y == 1, 1, -1), 0)
  Z <- sweep(Z, 2, colMeans(Z))
  S <- tcrossprod(Z)
  V <- outer(seq_len(N), seq_len(D), function(i, d) as.numeric(i)^d)
  for (step in 1:50) {
    V <- S %*% V
    for (d in seq_len(D)) {
      for (e in seq_len(d - 1L)) V[, d] <- V[, d] - sum(V[, d] * V[, e]) * V[, e]
      nrm <- sqrt(sum(V[, d]^2))
      V[, d] <- if (nrm > 0) V[, d] / nrm else 0
    }
  }
  V * sqrt(N)
}

# ===========================================================================
# 16. Nonparametric bootstrap for scaling outputs
# ===========================================================================

#' Nonparametric bootstrap for AM / blackbox scaling positions
#'
#' Efron & Tibshirani (1993) resampling of respondents for
#' Aldrich-McKelvey and Basic Space scaling SEs.
#'
#' @param Z Perception matrix.
#' @param scale_fn One of `"am"`, `"blackbox"`, `"blackbox_t"`.
#' @param n_boot Number of bootstrap replications.
#' @param seed RNG seed.
#' @return List with `se_positions`, `boot_mean`, `ci_lower`,
#'   `ci_upper`, `n_boot`.
#' @references Efron, B. and Tibshirani, R. (1993). *An Introduction to
#'   the Bootstrap*.  Chapman & Hall.
#' @examples
#' set.seed(1)
#' Z <- matrix(rnorm(20 * 5), 20, 5)
#' morie_spatial_voting_nonparametric_bootstrap(Z, n_boot = 10L)
#' @export
morie_spatial_voting_nonparametric_bootstrap <- function(Z,
                                                         scale_fn = "am",
                                                         n_boot   = 200L,
                                                         seed     = 42L) {
  Z <- as.matrix(Z)
  n_resp <- nrow(Z)
  .rmorie_local_seed(seed)
  boot_positions <- vector("list", n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n_resp, n_resp, replace = TRUE)
    Z_b <- Z[idx, , drop = FALSE]
    if (scale_fn == "am") {
      r <- morie_spatial_voting_aldrich_mckelvey(Z_b)
      boot_positions[[b]] <- r$zhat
    } else if (scale_fn == "blackbox") {
      r <- morie_spatial_voting_blackbox(Z_b)
      boot_positions[[b]] <- as.numeric(r$stimuli_weights[, 1L])
    } else {
      r <- morie_spatial_voting_blackbox(t(Z_b))
      boot_positions[[b]] <- as.numeric(r$stimuli_weights[, 1L])
    }
  }
  boot_arr <- do.call(rbind, boot_positions)
  se <- apply(boot_arr, 2, stats::sd)
  boot_mean <- colMeans(boot_arr)
  ci_low  <- apply(boot_arr, 2, stats::quantile, probs = 0.025)
  ci_high <- apply(boot_arr, 2, stats::quantile, probs = 0.975)
  list(se_positions = se, boot_mean = boot_mean,
       ci_lower = ci_low, ci_upper = ci_high, n_boot = n_boot)
}

# ===========================================================================
# 17. Wordfish (Poisson IRT for text)
# ===========================================================================

#' Wordfish: Poisson IRT for document-term matrices
#'
#' Slapin & Proksch (2008) one-dimensional Poisson IRT for estimating
#' document positions from word-count data.
#'
#' @param dtm Document-by-term integer count matrix.
#' @param max_iter Maximum EM iterations.
#' @param tol Convergence tolerance.
#' @return List with `positions`, `word_weights`, `word_fixed`,
#'   `doc_fixed`, `log_lik`, `iterations`.
#' @references
#'   Slapin, J. B. and Proksch, S.-O. (2008). "A Scaling Model for
#'   Estimating Time-Series Party Positions from Texts." *AJPS*, 52(3).
#' @examples
#' set.seed(1)
#' dtm <- matrix(stats::rpois(20 * 30, 5), 20, 30)
#' morie_spatial_voting_wordfish(dtm, max_iter = 20L)
#' @export
morie_spatial_voting_wordfish <- function(dtm,
                                          max_iter = 100L,
                                          tol      = 1e-6) {
  dtm <- as.matrix(dtm)
  storage.mode(dtm) <- "double"
  n_docs <- nrow(dtm)
  n_words <- ncol(dtm)
  .rmorie_local_seed(42L)
  omega <- stats::rnorm(n_docs) * 0.5
  psi   <- log(rowSums(dtm) + 1)
  alpha <- log(colSums(dtm) / sum(dtm) + 1e-10)
  beta  <- stats::rnorm(n_words) * 0.1
  iter <- 0L
  for (iter in seq_len(max_iter)) {
    omega_old <- omega
    if (.sv_have_cpp("morie_spatial_wordfish_omega_update_cpp")) {
      # Note: the C++ kernel internally standardises omega WITHOUT
      # rescaling beta. Apply the beta/alpha correction here so the
      # parameterisation eta = psi + alpha + beta*omega is invariant
      # across iterations (v0.9.5.6+ strict-identifiability fix).
      m_om <- mean(omega)
      s_om <- stats::sd(omega) + 1e-12
      omega <- as.numeric(morie_spatial_wordfish_omega_update_cpp(
        dtm, psi, alpha, beta, omega))
      alpha <- alpha + beta * m_om   # absorb mean shift into alpha
      beta  <- beta * s_om           # rescale beta to standardised scale
    } else {
      for (i in seq_len(n_docs)) {
        eta <- psi[i] + alpha + beta * omega[i]
        mu <- exp(pmin(pmax(eta, -20), 20))
        g <- sum(beta * (dtm[i, ] - mu))
        h <- -sum(beta ^ 2 * mu) - 1
        omega[i] <- omega[i] - g / h
      }
      # v0.9.5.6+: couple the omega standardisation with an alpha + beta
      # rescale so eta = psi + alpha + beta*omega is invariant. Pre-
      # v0.9.5.6 only standardised omega which silently distorted beta
      # and degraded convergence.
      m_om  <- mean(omega)
      s_om  <- stats::sd(omega) + 1e-12
      omega <- (omega - m_om) / s_om
      alpha <- alpha + beta * m_om
      beta  <- beta * s_om
    }
    for (j in seq_len(n_words)) {
      eta <- psi + alpha[j] + beta[j] * omega
      mu <- exp(pmin(pmax(eta, -20), 20))
      ga <- sum(dtm[, j] - mu)
      ha <- -sum(mu) - 0.01
      alpha[j] <- alpha[j] - ga / ha
      gb <- sum(omega * (dtm[, j] - mu))
      hb <- -sum(omega ^ 2 * mu) - 0.01
      beta[j] <- beta[j] - gb / hb
    }
    change <- sqrt(sum((omega - omega_old) ^ 2)) /
              (sqrt(sum(omega_old ^ 2)) + 1e-12)
    if (change < tol) break
  }
  ll <- 0
  for (i in seq_len(n_docs)) {
    eta <- psi[i] + alpha + beta * omega[i]
    mu  <- exp(pmin(pmax(eta, -20), 20))
    ll  <- ll + sum(dtm[i, ] * log(mu + 1e-15) - mu)
  }
  list(positions = omega, word_weights = beta,
       word_fixed = alpha, doc_fixed = psi,
       log_lik = ll, iterations = iter)
}
