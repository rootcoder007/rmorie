.emimp_estep <- function(X, mu, S) {
  n <- nrow(X)
  p <- ncol(X)
  xs <- X
  cs <- vector("list", n)
  for (i in seq_len(n)) {
    o <- which(!is.na(X[i, ]))
    m <- which(is.na(X[i, ]))
    ci <- matrix(0, p, p)
    xs[i, m] <- mu[m]
    if (length(m) && length(o)) {
      Soo <- solve(S[o, o, drop = FALSE])
      reg <- S[m, o, drop = FALSE] %*% Soo
      xs[i, m] <- mu[m] + as.numeric(reg %*% (X[i, o] - mu[o]))
      ci[m, m] <- S[m, m, drop = FALSE] - reg %*% S[o, m, drop = FALSE]
    } else if (length(m)) {
      ci[m, m] <- S[m, m, drop = FALSE]
    }
    cs[[i]] <- ci
  }
  list(x = xs, c = cs)
}

#' EM imputation under a multivariate normal model
#'
#' Maximum-likelihood mean and covariance of incomplete multivariate normal
#' data by EM (Little and Rubin 2002, section 11.2): the E-step replaces each
#' row's missing coordinates by their conditional mean given the observed
#' ones and carries their conditional covariance \eqn{C_i}, which the M-step
#' adds back, \eqn{\Sigma = n^{-1} \sum_i ((x_i - \mu)(x_i - \mu)' + C_i)}.
#' The returned data are the conditional means at the final estimates.
#' Identical to the Python arm \code{morie.fn.emimq.em_imputation}.
#'
#' @param data Numeric matrix (or vector) with \code{NA} for missing values.
#' @param max_iter Maximum EM iterations.
#' @param tol Stop when the largest change in the mean or covariance is below
#'   this.
#' @return A list with \code{value} and \code{n_missing}, \code{n}, \code{p},
#'   \code{imputed_means}, \code{mean}, \code{cov}, \code{imputed},
#'   \code{loglik} (observed-data), \code{iterations} and \code{converged}.
#' @references Dempster, A. P., Laird, N. M. and Rubin, D. B. (1977). Maximum
#'   likelihood from incomplete data via the EM algorithm. Journal of the Royal
#'   Statistical Society B 39, 1-38.
#'
#'   Little, R. J. A. and Rubin, D. B. (2002). Statistical Analysis with Missing
#'   Data, 2nd ed. Wiley.
#' @examples
#' X <- cbind(c(1, 2, 3.5, 4, 5.5, 6), c(2.1, 2.9, 4.2, 5.1, NA, NA))
#' em_imputation(X)$mean
#' @export
em_imputation <- function(data, max_iter = 100, tol = 1e-6) {
  X <- if (is.null(dim(data))) matrix(as.numeric(data), ncol = 1) else as.matrix(data)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  if (any(colSums(!is.na(X)) == 0)) stop("a column is entirely missing")
  mu <- vapply(seq_len(p), function(j) mean(X[!is.na(X[, j]), j]), 0)
  S <- diag(vapply(seq_len(p), function(j) {
    v <- X[!is.na(X[, j]), j]
    max(sum((v - mu[j])^2) / length(v), 1e-8)
  }, 0), p, p)
  it <- 0L
  converged <- FALSE
  for (k in seq_len(max_iter)) {
    it <- it + 1L
    e <- .emimp_estep(X, mu, S)
    nmu <- colSums(e$x) / n
    D <- sweep(e$x, 2, nmu)
    nS <- (crossprod(D) + Reduce(`+`, e$c)) / n
    delta <- max(max(abs(nmu - mu)), max(abs(nS - S)))
    mu <- nmu
    S <- nS
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  ll <- 0
  for (i in seq_len(n)) {
    o <- which(!is.na(X[i, ]))
    So <- S[o, o, drop = FALSE]
    dev <- X[i, o] - mu[o]
    ll <- ll - 0.5 * (length(o) * log(2 * pi) + as.numeric(determinant(So)$modulus) +
      sum(dev * solve(So, dev)))
  }
  imp <- .emimp_estep(X, mu, S)$x
  list(value = sum(is.na(X)), n_missing = sum(is.na(X)), n = n, p = p,
       imputed_means = colSums(imp) / n, mean = mu, cov = S, imputed = imp,
       loglik = ll, iterations = it, converged = converged)
}
