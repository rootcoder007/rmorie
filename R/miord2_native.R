# MICE multiple imputation by chained equations (normal model).
# Source: van Buuren (2018), Flexible Imputation of Missing Data,
# 2nd ed., Algorithm 4.3 (MICE) and Algorithm 3.1 (Bayesian normal
# draw; adapted from Rubin 1987 p. 167)
# (fetched-wave3/vanbuuren-fimd-ch4-mice.html, -ch3-norm.html);
# van Buuren & Groothuis-Oudshoorn (2011), JSS 45(3).  Mirrors
# Python morie.fn.miord2 exactly: the shared SplitMix64 stream
# (.ghc_rng/.ghc_unif/.ghc_norm/.ghc_gamma1) is consumed draw for
# draw in the same order.

#' .miord2_norm_draw
#'
#' Part of the miord2_native implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param X_obs See Usage.
#' @param y_obs See Usage.
#' @param X_mis See Usage.
#' @param kappa See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.miord2_norm_draw <- function(e, X_obs, y_obs, X_mis, kappa) {
  n1 <- length(y_obs)
  q <- ncol(X_obs)
  S <- t(X_obs) %*% X_obs
  A <- S + diag(diag(S) * kappa, q)
  V <- solve(A)
  beta_hat <- as.numeric(V %*% (t(X_obs) %*% y_obs))
  ssr <- sum((y_obs - X_obs %*% beta_hat)^2)
  nu <- max(n1 - q, 1)
  g <- 2 * .ghc_gamma1(e, nu / 2)          # chi^2_nu draw
  sigma <- sqrt(ssr / max(g, 1e-300))
  z1 <- .ghc_norm(e, q)
  L <- t(chol((V + t(V)) / 2))
  beta_dot <- beta_hat + sigma * as.numeric(L %*% z1)
  out <- numeric(nrow(X_mis))
  for (i in seq_len(nrow(X_mis))) {
    z2 <- .ghc_norm(e, 1)
    out[i] <- sum(beta_dot * X_mis[i, ]) + sigma * z2
  }
  out
}

#' MICE: multivariate imputation by chained equations (normal model)
#'
#' van Buuren's Algorithm 4.3 with the Bayesian normal linear
#' imputation of Algorithm 3.1 (mice method "norm"): random-draw
#' starting imputations, then per iteration each incomplete variable
#' is imputed from a posterior draw of its regression on the other
#' (currently complete) variables.
#'
#' @param data Numeric matrix or data frame; missing entries NA.
#' @param m Number of imputed data sets.
#' @param maxit Iterations per chain.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param kappa Ridge parameter of Algorithm 3.1.
#' @return A list with elements \code{imputations} (list of m
#'   completed matrices), \code{missing_mask}, \code{m},
#'   \code{maxit}, \code{column_means}, \code{seed}, \code{method}.
#' @references van Buuren, S. (2018). Flexible Imputation of Missing
#'   Data, 2nd ed. Chapman & Hall/CRC.  van Buuren, S. and
#'   Groothuis-Oudshoorn, K. (2011). JSS, 45(3).  Rubin, D. B.
#'   (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.
#' @export
morie_miord2 <- function(data, m = 5, maxit = 5, seed = 0,
                         kappa = 1e-4) {
  X <- as.matrix(data)
  n <- nrow(X)
  p <- ncol(X)
  if (n < 3) stop("need at least three rows")
  mask <- is.na(X)
  mis_cols <- which(apply(mask, 2, any))
  if (any(apply(mask, 2, all))) stop("a column has no observed values")
  m <- as.integer(m)
  maxit <- as.integer(maxit)
  e <- .ghc_rng(seed)
  imps <- vector("list", m)
  for (chain in seq_len(m)) {
    cur <- X
    for (j in mis_cols) {
      obs <- X[!mask[, j], j]
      for (i in which(mask[, j])) {
        pick <- min(floor(.ghc_unif(e, 1) * length(obs)),
                    length(obs) - 1) + 1
        cur[i, j] <- obs[pick]
      }
    }
    for (t_ in seq_len(maxit)) {
      for (j in mis_cols) {
        others <- setdiff(seq_len(p), j)
        Xd <- cbind(1, cur[, others, drop = FALSE])
        mi <- mask[, j]
        if (!any(mi)) next
        draws <- .miord2_norm_draw(e, Xd[!mi, , drop = FALSE],
                                   X[!mi, j], Xd[mi, , drop = FALSE],
                                   kappa)
        cur[mi, j] <- draws
      }
    }
    imps[[chain]] <- cur
  }
  means <- lapply(imps, colMeans)
  list(imputations = imps, missing_mask = mask, m = m, maxit = maxit,
       column_means = means, seed = seed,
       method = "MICE norm (van Buuren Algs. 3.1 + 4.3)")
}
