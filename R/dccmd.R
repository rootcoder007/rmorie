# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: DCC(1,1) two-step Gaussian negative log-likelihood for the
# base-R fallback path. Extracted from the morie_dcc_multivariate_garch()
# optimiser closure so the parameter-domain and non-positive-determinant
# guards are directly unit-testable. `Q_bar` is the unconditional
# correlation, `n` the sample size, `Z` the standardised residuals.
.dccmd_negll <- function(p, Q_bar, n, Z) {
  a <- p[1]
  b <- p[2]
  if (a < 0 || b < 0 || a + b >= 0.9999) {
    return(1e10)
  }
  Q <- Q_bar
  ll <- 0
  for (t in seq_len(n)) {
    d <- sqrt(pmax(diag(Q), 1e-12))
    R <- Q / outer(d, d)
    ld <- determinant(R, logarithm = TRUE)
    # determinant() reports sign = +1 for a singular matrix (det == 0),
    # so a sign test alone misses it; modulus == -Inf catches singularity
    # and prevents the solve(R) below from erroring on a non-invertible R.
    if (ld$sign <= 0 || !is.finite(ld$modulus)) {
      return(1e10)
    }
    Rinv <- solve(R)
    zt <- Z[t, ]
    ll <- ll + 0.5 * (ld$modulus + sum(zt * (Rinv %*% zt)) - sum(zt^2))
    Q <- (1 - a - b) * Q_bar + a * tcrossprod(zt) + b * Q
  }
  as.numeric(ll)
}

#' DCC multivariate GARCH (Engle 2002)
#'
#' Two-step DCC(1,1) on a panel of return series.
#'
#' @param x Numeric matrix of returns (T x k).
#' @return Named list with \code{a, b, unconditional_correlation,
#'   conditional_correlation, conditional_variance, loglik, n, k, method}.
#' @examples
#' morie_dcc_multivariate_garch(x = matrix(rnorm(150), 50, 3))
#' @export
morie_dcc_multivariate_garch <- function(x) {
  X <- as.matrix(x)
  if (nrow(X) < ncol(X)) X <- t(X)
  n <- nrow(X)
  k <- ncol(X)
  if (n < 30 || k < 2) stop("Need n>=30, k>=2.")
  if (requireNamespace("rmgarch", quietly = TRUE) &&
    requireNamespace("rugarch", quietly = TRUE)) {
    # The rmgarch DCC path relies on S4 `coef`/`sigma` methods whose
    # dispatch and slot layout vary across rmgarch versions.  Wrap it so
    # that any API mismatch degrades gracefully to the base-R DCC below
    # rather than hard-failing.
    res <- tryCatch(
      {
        uspec <- rugarch::multispec(replicate(k, rugarch::ugarchspec(
          variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
        ),
        simplify = FALSE
        ))
        dccspec <- rmgarch::dccspec(
          uspec = uspec, dccOrder = c(1, 1),
          distribution = "mvnorm"
        )
        fit <- rmgarch::dccfit(dccspec, data = X)
        p <- stats::coef(fit)
        sig_mat <- as.matrix(stats::sigma(fit))
        list(
          a = unname(p["[Joint]dcca1"]),
          b = unname(p["[Joint]dccb1"]),
          unconditional_correlation = cor(X),
          conditional_correlation = rmgarch::rcor(fit),
          conditional_variance = sig_mat^2,
          loglik = as.numeric(rugarch::likelihood(fit)),
          n = n, k = k,
          method = "DCC(1,1) via rmgarch"
        )
      },
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(res)
    }
  }
  # Fallback: two-step EWMA-marginal + closed-form DCC update.
  H <- matrix(NA_real_, n, k)
  Z <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    rj <- X[, j] - mean(X[, j])
    g <- morie_garch_fit(rj)
    H[, j] <- g$conditional_variance
    Z[, j] <- rj / sqrt(H[, j] + 1e-12)
  }
  Q_bar <- crossprod(Z) / n
  neg_ll <- function(p) .dccmd_negll(p, Q_bar, n, Z)
  opt <- nlminb(c(0.02, 0.95), neg_ll,
    lower = c(1e-6, 1e-6),
    upper = c(0.5, 0.999)
  )
  a <- opt$par[1]
  b <- opt$par[2]
  Q <- Q_bar
  R_path <- array(NA_real_, c(n, k, k))
  for (t in seq_len(n)) {
    d <- sqrt(pmax(diag(Q), 1e-12))
    R_path[t, , ] <- Q / outer(d, d)
    Q <- (1 - a - b) * Q_bar + a * tcrossprod(Z[t, ]) + b * Q
  }
  list(
    a = a, b = b,
    unconditional_correlation = Q_bar,
    conditional_correlation = R_path,
    conditional_variance = H,
    loglik = -opt$objective,
    n = n, k = k,
    method = "DCC(1,1) two-step Gaussian MLE (base R)"
  )
}
