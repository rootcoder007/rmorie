#' AUC, Poisson GLM, k-means and ridge with verbose results
#'
#' `aurroc` is the Mann-Whitney AUC `P(S+ > S-) + P(tie)/2`; `glmpoi` the
#' Poisson GLM by IRLS (as `stats::glm(family = poisson)`); `kmeans2` Lloyd
#' k-means (`KmeansLloyd`) from `n_init` Philox-drawn starts keeping the
#' least within-cluster SSE (0-based labels); `rdgr` ridge regression
#' `(Xc'Xc + alpha I)^-1 Xc'yc` with an unpenalised intercept.
#'
#' @param y_true Binary labels (1 positive).
#' @param score Scores.
#' @param X Regressors (or data matrix for `kmeans2`).
#' @param y Response.
#' @param add_intercept,fit_intercept Include an intercept.
#' @param tol Relative deviance tolerance.
#' @param maxit Maximum IRLS iterations.
#' @param n_clusters Number of clusters.
#' @param n_init Number of random starts.
#' @param random_state Philox seed.
#' @param alpha Ridge penalty.
#' @return Lists with the components of the Python arm's payload.
#' @references Hanley, J. A. and McNeil, B. J. (1982). The meaning and use of
#'   the area under a receiver operating characteristic (ROC) curve. Radiology
#'   143, 29-36. McCullagh, P. and Nelder, J. A. (1989). Generalized Linear
#'   Models, 2nd ed. Chapman and Hall. Lloyd, S. P. (1982). Least squares
#'   quantization in PCM. IEEE Transactions on Information Theory 28,
#'   129-137. Hoerl, A. E. and Kennard, R. W. (1970). Ridge regression.
#'   Technometrics 12, 55-67.
#' @examples
#' aurroc(c(0, 0, 1, 1, 0, 1), c(0.1, 0.4, 0.35, 0.8, 0.4, 0.9))$value
#' rdgr(matrix(1:4), c(1, 2.1, 2.9, 4.2))$coef
#' @export
aurroc <- function(y_true, score) {
  y <- as.integer(y_true)
  s <- as.numeric(score)
  pos <- s[y == 1]
  neg <- s[y == 0]
  if (!length(pos) || !length(neg)) stop("both classes are needed for the AUC")
  auc <- mean(outer(pos, neg, function(p, q) (p > q) + 0.5 * (p == q)))
  bench <- if (auc >= 0.9) "excellent" else if (auc >= 0.8) "good" else if (auc >= 0.7) "fair" else if (auc > 0.5) "poor" else
    "no better than chance"
  list(value = auc, statistic = auc, benchmark = bench, n_positive = length(pos), n_negative = length(neg))
}

#' @rdname aurroc
#' @export
glmpoi <- function(X, y, add_intercept = TRUE, tol = 1e-12, maxit = 100) {
  X <- unname(as.matrix(X)) * 1
  y <- as.numeric(y)
  if (any(y < 0)) stop("Poisson y must be non-negative.")
  if (add_intercept) X <- cbind(1, X)
  n <- nrow(X)
  p <- ncol(X)
  dev <- function(mu) 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
  mu <- y + 0.1
  eta <- log(mu)
  d_old <- dev(mu)
  for (it in seq_len(maxit)) {
    z <- eta + (y - mu) / mu
    beta <- as.vector(solve(crossprod(X, mu * X), crossprod(X, mu * z)))
    eta <- as.vector(X %*% beta)
    mu <- exp(eta)
    d_new <- dev(mu)
    done <- abs(d_new - d_old) / (abs(d_new) + 0.1) < tol
    d_old <- d_new
    if (done) break
  }
  se <- sqrt(diag(solve(crossprod(X, mu * X))))
  ll <- sum(y * eta - mu - lgamma(y + 1))
  pearson <- sum((y - mu)^2 / mu)
  list(coef = beta, se = se, pvalues = 2 * stats::pnorm(-abs(beta / se)), aic = -2 * ll + 2 * p, deviance = d_old,
       loglik = ll, fitted = mu, overdispersion = if (n > p) pearson / (n - p) else NaN)
}

#' @rdname aurroc
#' @export
kmeans2 <- function(X, n_clusters = 3, n_init = 10, random_state = 42) {
  P <- unname(as.matrix(X)) * 1
  n <- nrow(P)
  k <- as.integer(n_clusters)
  if (k < 2) stop("n_clusters must be >= 2")
  best <- NULL
  for (s in seq_len(n_init) - 1) {
    u <- .morie_random_uniform(n, seed = random_state, stream = s)
    idx <- seq_len(n)
    for (i in seq_len(k)) {
      j <- i + floor(u[i] * (n - i + 1))
      tmp <- idx[i]
      idx[i] <- idx[j]
      idx[j] <- tmp
    }
    r <- KmeansLloyd(P, P[idx[seq_len(k)], , drop = FALSE])
    if (is.null(best) || r$tot_withinss < best$tot_withinss) best <- r
  }
  labels <- best$cluster - 1L
  list(labels = labels, centroids = best$centers, inertia = best$tot_withinss,
       sizes = vapply(seq_len(k) - 1L, function(i) sum(labels == i), 0L))
}

#' @rdname aurroc
#' @export
rdgr <- function(X, y, alpha = 1, fit_intercept = TRUE) {
  X <- unname(as.matrix(X)) * 1
  y <- as.numeric(y)
  xm <- if (fit_intercept) colMeans(X) else rep(0, ncol(X))
  ym <- if (fit_intercept) mean(y) else 0
  Xc <- sweep(X, 2, xm)
  b <- as.vector(solve(crossprod(Xc) + alpha * diag(ncol(X)), crossprod(Xc, y - ym)))
  b0 <- ym - sum(xm * b)
  fit <- b0 + as.vector(X %*% b)
  tss <- sum((y - mean(y))^2)
  list(coef = b, intercept = b0, r2 = if (tss > 0) 1 - sum((y - fit)^2) / tss else 0, alpha = alpha, fitted = fit)
}
