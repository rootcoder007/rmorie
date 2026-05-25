# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spatial Durbin / SAR direct-indirect-total decomposition (MRM primitive)
#'
#' Mirrors the Python module
#' \code{morie.mrm_primitives.spatial_spillover}, adapted from
#' Laniyonu (2018) Urban Affairs Review 54(5):898-930, which in turn
#' uses LeSage & Pace (2009) + Elhorst (2010) + the Yang/Noah/Shoff
#' (2015) decomposition formula.
#'
#' The Laniyonu (2018) result -- gentrification's effect on
#' stops-per-capita is ~0 direct but +51 to +90\% indirect (spillover
#' into neighbouring tracts) -- only surfaces once you decompose. An
#' OLS or non-spatial FE model would report "no effect" and miss the
#' entire story.
#'
#' This primitive is the SDM with the canonical decomposition + the
#' Moran's-I diagnostic that justifies SDM over OLS. We deliberately
#' do NOT fit the SDM ourselves -- \pkg{spdep}/\pkg{spatialreg} are
#' hard deps we don't want to force. The caller passes the estimated
#' \eqn{\rho}{rho} + \eqn{\beta}{beta} vectors; this primitive does the
#' decomposition arithmetic.
#'
#' @name mrm_spatial_spillover
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# .mrm_result and print.morie_mrm_result are defined in
# mrm_primitives_gentrification.R and shared across the three files.


# ---------------------------------------------------------------------------
# mrm_spatial_spillover_decomposition
# ---------------------------------------------------------------------------

#' SDM direct / indirect / total decomposition
#'
#' Implements the standard LeSage & Pace formula:
#'
#' \deqn{(I - \rho W)^{-1} (I \beta_k + W \theta_k)}{(I - rho W)^-1 (I beta_k + W theta_k)}
#'
#' for each covariate \eqn{k}. The diagonal of the resulting
#' per-observation effects matrix is averaged for the \emph{direct}
#' effect; the average off-diagonal-row-sum is the \emph{indirect}
#' effect; \emph{total} = direct + indirect.
#'
#' @param rho Numeric scalar. Spatial-autoregressive coefficient from
#'   the fitted SDM.
#' @param beta_direct Numeric vector of length \eqn{K}. Coefficients
#'   on the K covariates (no lagged terms).
#' @param beta_spatial Numeric vector of length \eqn{K}. Coefficients
#'   on the spatially-lagged covariates (\eqn{WX}). Set to all zeros
#'   if you fit a SAR (lag-only) model.
#' @param W Numeric matrix of shape (N, N). Row-standardised spatial
#'   weight matrix.
#' @param coefficient_names Optional character vector of length K with
#'   human-readable covariate names; defaults to \code{c("x1", ...,
#'   "xK")}.
#' @return A named list with classes \code{morie_mrm_result},
#'   \code{morie_rich_result}, \code{list}. Carries
#'   \code{decomposition} (a \code{data.frame} with columns
#'   \code{coefficient}, \code{direct}, \code{indirect}, \code{total},
#'   \code{note}), \code{rho}, plus \code{interpretation} +
#'   \code{warnings}.
#' @examples
#' set.seed(3)
#' N <- 12
#' W <- matrix(runif(N * N), N, N)
#' diag(W) <- 0
#' W <- W / rowSums(W)
#' res <- mrm_spatial_spillover_decomposition(
#'   rho = 0.4,
#'   beta_direct  = c(0.10, -0.05),
#'   beta_spatial = c(0.30,  0.00),
#'   W = W,
#'   coefficient_names = c("gentrification", "controls")
#' )
#' res$decomposition
#' @export
mrm_spatial_spillover_decomposition <- function(rho,
                                                beta_direct,
                                                beta_spatial,
                                                W,
                                                coefficient_names = NULL) {
  stopifnot(is.numeric(rho), length(rho) == 1L,
            is.numeric(beta_direct), is.numeric(beta_spatial),
            is.matrix(W) || inherits(W, "Matrix"))
  W <- as.matrix(W)
  storage.mode(W) <- "double"

  K <- length(beta_direct)
  if (is.null(coefficient_names)) {
    coefficient_names <- paste0("x", seq_len(K))
  }
  if (length(beta_spatial) != K) {
    stop("beta_direct and beta_spatial must have the same length")
  }
  if (length(coefficient_names) != K) {
    stop("coefficient_names must have length equal to length(beta_direct)")
  }
  if (nrow(W) != ncol(W)) {
    stop(sprintf("W must be square, got shape (%d, %d)", nrow(W), ncol(W)))
  }

  call_str <- sprintf(
    "mrm_spatial_spillover_decomposition(rho=%.4f, K=%d, N=%d)",
    rho, K, nrow(W)
  )
  warnings <- character(0)

  N <- nrow(W)
  IN <- diag(N)
  S <- tryCatch(
    solve(IN - rho * W),
    error = function(e) NULL
  )
  if (is.null(S)) {
    stop(sprintf(
      "Could not invert (I - rho*W) at rho=%.6f; spatial multiplier is singular (rho near 1/lambda_max?)",
      rho
    ))
  }

  row_sums_W <- rowSums(W)
  if (any(abs(row_sums_W - 1) > 1e-6 & row_sums_W != 0)) {
    warnings <- c(warnings,
      "W does not appear to be row-standardised; direct/indirect effect scales may not be comparable to LeSage & Pace conventions.")
  }
  if (abs(rho) >= 1) {
    warnings <- c(warnings, sprintf(
      "|rho|=%.4f >= 1; spatial multiplier is non-stationary and decomposition is undefined in the standard SDM theory.",
      abs(rho)
    ))
  }

  decomp <- data.frame(
    coefficient = character(K),
    direct      = numeric(K),
    indirect    = numeric(K),
    total       = numeric(K),
    note        = character(K),
    stringsAsFactors = FALSE
  )
  for (k in seq_len(K)) {
    # M_k = S * (I * beta_k + W * theta_k)
    M_k <- S %*% (beta_direct[k] * IN + beta_spatial[k] * W)
    direct_k <- mean(diag(M_k))
    row_sums <- rowSums(M_k)
    indirect_k <- mean(row_sums - diag(M_k))
    total_k <- direct_k + indirect_k
    decomp$coefficient[k] <- coefficient_names[k]
    decomp$direct[k]      <- direct_k
    decomp$indirect[k]    <- indirect_k
    decomp$total[k]       <- total_k
    decomp$note[k] <- sprintf(
      "Per-observation marginal effects averaged across N=%d units (rho=%.4f).",
      N, rho
    )
  }

  # Headline interpretation: largest |indirect| coefficient.
  abs_indirect <- abs(decomp$indirect)
  hi <- which.max(abs_indirect)
  lead_txt <- if (length(hi) == 0L) {
    "No covariates supplied; nothing to decompose."
  } else {
    sprintf(
      "Largest indirect effect: %s -> indirect=%+.4f vs direct=%+.4f (total=%+.4f). %s",
      decomp$coefficient[hi], decomp$indirect[hi],
      decomp$direct[hi], decomp$total[hi],
      if (abs(decomp$indirect[hi]) > 2 * abs(decomp$direct[hi]) &&
          abs(decomp$direct[hi]) > 0) {
        "Indirect dominates direct by >2x; spillover is the story, not the own-tract effect."
      } else if (abs(decomp$direct[hi]) > 2 * abs(decomp$indirect[hi]) &&
                 abs(decomp$indirect[hi]) > 0) {
        "Direct dominates indirect; effect concentrates in the own tract."
      } else {
        "Direct and indirect effects are of comparable magnitude."
      }
    )
  }

  interp <- paste(
    sprintf(
      "Spatial Durbin decomposition with rho=%.4f over N=%d units and K=%d covariate(s).",
      rho, N, K
    ),
    lead_txt,
    "Direct = average diagonal of the per-observation effects matrix (own-tract impact); indirect = average off-diagonal-row-sum (spillover to neighbours through the spatial multiplier (I - rho*W)^{-1}); total = direct + indirect.",
    sep = " "
  )

  .mrm_result(
    "MRM Spatial Spillover Decomposition",
    call_str,
    summary_lines = list(
      `Units (N)`      = N,
      `Covariates (K)` = K,
      `rho`            = rho
    ),
    warnings = warnings,
    interpretation = interp,
    n = N,
    rho = rho,
    decomposition = decomp,
    coefficient_names = coefficient_names,
    beta_direct = beta_direct,
    beta_spatial = beta_spatial,
    value = if (length(hi) > 0L) decomp$indirect[hi] else NA_real_
  )
}


# ---------------------------------------------------------------------------
# mrm_morans_i
# ---------------------------------------------------------------------------

#' Moran's I statistic for residual spatial autocorrelation
#'
#' First rung of the diagnostic ladder: if OLS residuals show
#' significant Moran's I, an SDM (or SEM/SAR) is warranted over OLS.
#'
#' Statistic:
#'
#' \deqn{I = \frac{n}{\sum_{ij} w_{ij}} \cdot
#'   \frac{e^\top W e}{e^\top e}, \quad e = r - \bar r.}{I = frac{n}{sum_ij w_ij} * (e^top W e)/(e^top e), e = r - bar r.}
#'
#' \eqn{I \in `[-1, 1]`}{I in `[-1, 1]`}. Positive -> clustering, negative ->
#' dispersion, ~0 -> spatial randomness.
#'
#' @param residuals Numeric vector of length N (e.g. OLS residuals).
#' @param W Numeric matrix of shape (N, N) -- the spatial weight
#'   matrix. Need not be row-standardised but must be aligned with
#'   \code{residuals}.
#' @return A named list with classes \code{morie_mrm_result},
#'   \code{morie_rich_result}, \code{list}. Carries
#'   \code{morans_i} (the scalar statistic) plus \code{interpretation}
#'   + \code{warnings}.
#' @examples
#' set.seed(4)
#' N <- 20
#' W <- matrix(runif(N * N), N, N); diag(W) <- 0; W <- W / rowSums(W)
#' resid <- rnorm(N)
#' mrm_morans_i(resid, W)$morans_i
#' @export
mrm_morans_i <- function(residuals, W) {
  residuals <- as.numeric(residuals)
  if (is.matrix(W) || inherits(W, "Matrix")) {
    W <- as.matrix(W)
    storage.mode(W) <- "double"
  } else {
    stop("W must be a matrix.")
  }
  n <- length(residuals)
  call_str <- sprintf("mrm_morans_i(residuals=<%d>, W=<%dx%d>)",
                      n, nrow(W), ncol(W))
  warnings <- character(0)

  if (!all(dim(W) == c(n, n))) {
    stop(sprintf("W must be (%d, %d) to match residuals; got (%d, %d)",
                 n, n, nrow(W), ncol(W)))
  }

  e <- residuals - mean(residuals)
  W_sum <- sum(W)
  denom <- as.numeric(crossprod(e))
  if (W_sum == 0 || denom == 0) {
    stat <- NA_real_
    if (W_sum == 0) {
      warnings <- c(warnings,
        "Sum of weights is zero; Moran's I is undefined.")
    }
    if (denom == 0) {
      warnings <- c(warnings,
        "Residual variance is zero; Moran's I is undefined.")
    }
  } else {
    numerator <- as.numeric(crossprod(e, W %*% e))
    stat <- (n / W_sum) * (numerator / denom)
  }

  interp <- if (is.na(stat)) {
    "Moran's I could not be computed (zero residual variance or zero-sum weight matrix)."
  } else if (stat > 0.30) {
    sprintf(
      "Moran's I = %+.4f indicates strong positive spatial autocorrelation; OLS residuals cluster across neighbours and a spatial model (SDM, SEM, or SAR) is warranted.",
      stat
    )
  } else if (stat > 0.10) {
    sprintf(
      "Moran's I = %+.4f indicates moderate positive spatial autocorrelation; consider a spatial specification.",
      stat
    )
  } else if (stat > -0.10) {
    sprintf(
      "Moran's I = %+.4f is close to zero; residuals are approximately spatially random and OLS is defensible on this diagnostic.",
      stat
    )
  } else {
    sprintf(
      "Moran's I = %+.4f indicates negative spatial autocorrelation (dispersion); residuals alternate across neighbours.",
      stat
    )
  }

  .mrm_result(
    "MRM Moran's I (residual)",
    call_str,
    summary_lines = list(
      `N`           = n,
      `Sum(W)`      = W_sum,
      `Moran's I`   = stat
    ),
    warnings = warnings,
    interpretation = interp,
    n = n,
    morans_i = stat,
    value = stat
  )
}
