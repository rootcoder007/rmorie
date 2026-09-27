# SPDX-License-Identifier: AGPL-3.0-or-later
.mlfa_state <- function(S, psi, m) {
  sq <- sqrt(psi)
  e <- eigen(S / outer(sq, sq), symmetric = TRUE)
  th <- e$values
  L <- sq * e$vectors[, seq_len(m), drop = FALSE] %*% diag(sqrt(pmax(th[seq_len(m)] - 1, 0)), m)
  list(F = sum(th[-seq_len(m)] - log(th[-seq_len(m)])) - (nrow(S) - m),
       g = (rowSums(L^2) + psi - diag(S)) / psi^2, L = L)
}

.mlfa_fit <- function(S, m, lower = 0.005, max_iter = 1000, gtol = 1e-10) {
  p <- nrow(S)
  if (m < 1 || m >= p) stop("need 1 <= m < p", call. = FALSE)
  psi <- pmin(1, pmax(lower, (1 - 0.5 * m / p) / diag(solve(S))))
  st <- .mlfa_state(S, psi, m)
  free_set <- function(x, g) which(!((x <= lower & g > 0) | (x >= 1 & g < 0)))
  free <- free_set(psi, st$g)
  Hinv <- diag(p)
  stall <- 0
  for (it in seq_len(max_iter)) {
    if (!length(free) || max(abs(st$g[free])) < gtol) break
    d <- numeric(p)
    d[free] <- -drop(Hinv[free, free, drop = FALSE] %*% st$g[free])
    if (sum(d[free] * st$g[free]) >= 0) {
      Hinv <- diag(p)
      d <- numeric(p)
      d[free] <- -st$g[free]
    }
    t <- 1
    slope <- abs(sum(d[free] * st$g[free]))
    repeat {
      cand <- pmin(1, pmax(lower, psi + t * d))
      sc <- .mlfa_state(S, cand, m)
      if (sc$F <= st$F - 1e-4 * t * slope || t < 1e-14) break
      t <- t / 2
    }
    sv <- cand - psi
    yv <- sc$g - st$g
    drop_f <- st$F - sc$F
    psi <- cand
    st <- sc
    newfree <- free_set(psi, st$g)
    if (!identical(newfree, free)) {
      Hinv <- diag(p)
      free <- newfree
    } else {
      sy <- sum(sv[free] * yv[free])
      if (sy > 1e-300) {
        Hy <- drop(Hinv[, free, drop = FALSE] %*% yv[free])
        yHy <- sum(yv[free] * Hy[free])
        Hinv[free, free] <- Hinv[free, free] + (sy + yHy) * tcrossprod(sv[free]) / sy^2 -
          (outer(Hy[free], sv[free]) + outer(sv[free], Hy[free])) / sy
      }
    }
    stall <- if (drop_f <= 1e-16 * max(1, abs(st$F))) stall + 1 else 0
    if (stall >= 3) break
  }
  list(uniquenesses = psi, loadings = st$L, objective = st$F)
}

#' Maximum likelihood factor analysis
#'
#' Minimises factanal's objective over uniquenesses between 0.005 and 1 by projected BFGS
#' steps; returns unrotated loadings, uniquenesses, Bartlett's chi-square, AIC and BIC.
#'
#' @param X Data matrix.
#' @param n_factors Number of factors (default: Kaiser count).
#' @param max_iter BFGS iterations.
#' @param tol Gradient tolerance.
#' @param scale Analyse correlations (TRUE) or covariances.
#' @return list(loadings, communalities, uniqueness, variance_explained, objective,
#'   log_likelihood, statistic, dof, p_value, aic, bic, n_factors).
#' @references Joreskog, K. G. (1967). Psychometrika 32, 443-482.
#' @examples
#' X <- cbind(1:8, c(2, 3, 3, 5, 5, 7, 8, 8), c(1, 2, 4, 4, 6, 5, 7, 9), c(3, 4, 4, 6, 5, 8, 8, 9))
#' MlFac(X, 1)$uniqueness
#' @export
MlFac <- function(X, n_factors = NULL, max_iter = 500, tol = 1e-12, scale = TRUE) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  C <- stats::cov(X)
  R <- stats::cov2cor(C)
  if (is.null(n_factors)) n_factors <- max(1, sum(eigen(R, symmetric = TRUE)$values > 1))
  m <- n_factors
  fit <- .mlfa_fit(R, m, max_iter = max_iter, gtol = tol)
  L <- if (scale) fit$loadings else fit$loadings * sqrt(diag(C))
  dof <- ((p - m)^2 - p - m) / 2
  stat <- (n - 1 - (2 * p + 5) / 6 - 2 * m / 3) * fit$objective
  list(loadings = L, communalities = rowSums(fit$loadings^2), uniqueness = fit$uniquenesses, variance_explained = colSums(L^2),
       objective = fit$objective,
       log_likelihood = -n / 2 * (p * log(2 * pi) + fit$objective + sum(log(eigen(R, symmetric = TRUE, only.values = TRUE)$values)) + p),
       statistic = stat, dof = dof, p_value = if (dof > 0) pchisq(stat, dof, lower.tail = FALSE) else NaN,
       aic = stat - 2 * dof, bic = stat - dof * log(n), n_factors = m)
}

#' Number of factors to retain
#'
#' Parallel analysis (Philox normals), Velicer's MAP (squared or fourth powers),
#' Kaiser, scree optimal coordinates and acceleration factor (nScree rules),
#' variance explained, and AIC/BIC of maximum likelihood factor models.
#'
#' @param data Numeric data (rows with missing values are dropped).
#' @param method One of "parallel", "map", "map4", "kaiser", "scree", "af", "variance", "aic", "bic".
#' @param nsim Parallel-analysis simulations.
#' @param seed Philox seed.
#' @param quantile Parallel-analysis quantile.
#' @param threshold Proportion of variance for method "variance".
#' @param max_factors Largest model for AIC/BIC.
#' @return list(n_factors, method, eigenvalues, ...criterion values).
#' @references Horn, J. L. (1965). Psychometrika 30, 179-185. Velicer, W. F. (1976).
#'   Psychometrika 41, 321-327. Raiche, G. et al. (2013). Methodology 9, 23-29.
#' @examples
#' X <- cbind(1:20, 2 * (1:20) + (1:20) %% 3, 3 * (1:20) - (1:20) %% 2, (7 * (1:20)) %% 5)
#' EfaNfactors(X, method = "kaiser")$n_factors
#' @export
EfaNfactors <- function(data, method = c("parallel", "map", "map4", "kaiser", "scree", "af", "variance", "aic", "bic"),
                        nsim = 100, seed = 42, quantile = 0.95, threshold = 0.7, max_factors = NULL) {
  method <- match.arg(method)
  X <- as.matrix(data)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  n <- nrow(X)
  p <- ncol(X)
  if (n < 3 || p < 2) stop("need at least 3 complete rows and 2 variables", call. = FALSE)
  R <- stats::cor(X)
  e <- eigen(R, symmetric = TRUE)
  ev <- e$values
  out <- list(method = method, eigenvalues = ev)
  if (method == "parallel") {
    sims <- t(vapply(seq_len(nsim) - 1, function(i) {
      Z <- matrix(.morie_random_normal(n * p, seed = seed, stream = i), n, p, byrow = TRUE)
      eigen(stats::cor(Z), symmetric = TRUE, only.values = TRUE)$values
    }, numeric(p)))
    thr <- apply(sims, 2, stats::quantile, probs = quantile, type = 7, names = FALSE)
    k <- 0
    while (k < p && ev[k + 1] > thr[k + 1]) k <- k + 1
    out$n_factors <- k
    out$threshold <- thr
  } else if (method %in% c("map", "map4")) {
    pw <- if (method == "map") 2 else 4
    off <- function(M) sum(M[row(M) != col(M)]^pw) / (p * (p - 1))
    vals <- c(off(R), vapply(seq_len(p - 2), function(m) {
      A <- e$vectors[, seq_len(m), drop = FALSE] %*% diag(sqrt(pmax(ev[seq_len(m)], 0)), m)
      C <- R - tcrossprod(A)
      off(C / sqrt(outer(diag(C), diag(C))))
    }, numeric(1)))
    out$n_factors <- which.min(vals) - 1
    out$map_values <- vals
  } else if (method == "kaiser") {
    out$n_factors <- sum(ev > 1)
  } else if (method %in% c("scree", "af")) {
    i <- seq_len(p - 2)
    pred <- c(ev[i + 1] - (ev[p] - ev[i + 1]) / (p - 1 - i), NA, NA)
    pass <- ev[i] >= pred[i] & ev[i] >= 1
    nc <- if (all(pass)) p - 2 else which(!pass)[1] - 1
    af <- c(NA, ev[3:p] - 2 * ev[2:(p - 1)] + ev[1:(p - 2)], NA)
    cand <- which(seq_len(p) %in% 2:(p - 1) & c(NA, ev[-p]) >= 1)
    if (!length(cand)) cand <- 2:(p - 1)
    naf <- cand[which.max(af[cand])] - 1
    out$n_factors <- if (method == "scree") nc else naf
    out$predicted <- pred
    out$acceleration <- af
  } else if (method == "variance") {
    cum <- cumsum(ev) / sum(ev)
    out$n_factors <- which(cum >= threshold - 1e-12)[1]
    out$cumulative <- cum
  } else {
    top <- if (is.null(max_factors)) p - 1 else max_factors
    vals <- numeric(0)
    for (m in seq_len(top)) {
      dof <- ((p - m)^2 - p - m) / 2
      if (dof < 0) break
      stat <- (n - 1 - (2 * p + 5) / 6 - 2 * m / 3) * .mlfa_fit(R, m)$objective
      vals <- c(vals, if (method == "aic") stat - 2 * dof else stat - dof * log(n))
    }
    if (!length(vals)) stop("no factor model with non-negative degrees of freedom", call. = FALSE)
    out$n_factors <- which.min(vals)
    out[[paste0(method, "_values")]] <- vals
  }
  out
}
